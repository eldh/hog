/*
 * The ignore list: what hog refuses to walk into.
 *
 * This is not a display filter. An ignored directory is PRUNED - never
 * opened, never read - which is the whole reason the feature exists: the
 * user's ignore list makes their scans faster, not just quieter. Walk asks
 * `matches` about every entry it is about to create, BEFORE it decides
 * whether to descend.
 *
 * SYNTAX, deliberately tiny:
 *
 *   - A pattern CONTAINING '/' is an absolute path, and must begin with '/'
 *     or '~/'. It ignores that exact directory (and, because we prune,
 *     everything under it). Anything else containing a '/' is a reported
 *     error, never silently reinterpreted.
 *   - A pattern WITHOUT '/' is a basename, matched at any depth, with '*'
 *     and '?' as the only wildcards.
 *   - '#' starts a comment; comments and blank lines round-trip verbatim.
 *   - Matching is case-insensitive, because APFS is by default. On a
 *     case-sensitive volume this over-matches; that is the accepted trade.
 *
 * THERE IS NO NEGATION, and that is a decision rather than an omission. It
 * is the part of gitignore semantics that everyone gets wrong, it forces
 * order-dependent evaluation, and it cannot work with pruning at all - you
 * cannot un-ignore something inside a directory you never opened. The need
 * it serves (overriding a shipped default) is served better by shipping the
 * defaults as ordinary editable lines, which is what `defaults` below is.
 *
 * MATCH ORDER matters for speed. Walk calls this once per directory entry,
 * so on a half-million-file scan it runs half a million times:
 *
 *   1. basename hashtable  - needs no path string at all
 *   2. absolute-path hashtable - one lookup
 *   3. the glob array - linear, but it is tiny and rarely reached
 *
 * Step 2 looks like it should need a prefix scan and does not: BECAUSE we
 * prune, we never see the descendants of an ignored directory, so a path
 * rule only ever has to match its own root exactly. That turns an
 * O(rules x depth) prefix walk into one hashtable lookup.
 */

/* ------------------------------------------------------------- the model */

type pattern =
  | Path(string) /* lowercased absolute path, matched exactly */
  | Name(string) /* lowercased literal basename, any depth */
  | Glob(string); /* lowercased basename glob, '*' and '?' only */

/* One source line, kept verbatim so the file round-trips losslessly when
 * hog appends to it. `rule` is None for a comment or a blank line. */
type parsed = {
  text: string,
  rule: option(pattern),
  error: option(string),
};

type rules = {
  paths: Hashtbl.t(string, unit),
  names: Hashtbl.t(string, unit),
  globs: array(string),
  source: array(parsed),
};

let empty: rules = {
  paths: Hashtbl.create(1),
  names: Hashtbl.create(1),
  globs: [||],
  source: [||],
};

/* ------------------------------------------------------------- utilities */

let lower = String.lowercase_ascii;

let startsWith = (~prefix: string, s: string): bool => {
  let pl = String.length(prefix);
  String.length(s) >= pl && String.sub(s, 0, pl) == prefix;
};

/* Drop a trailing '/' so that "~/Code/" and "~/Code" are the same rule. The
 * root "/" is left alone - it has nothing to strip. */
let stripTrailingSlash = (s: string): string => {
  let n = String.length(s);
  n > 1 && s.[n - 1] == '/' ? String.sub(s, 0, n - 1) : s;
};

/* Expand a leading '~' only. No $VAR expansion, no '~user' - both would be
 * surprising in a file the user edits by hand, and neither is needed. */
let expandHome = (~home: string, s: string): string =>
  if (s == "~") {
    home;
  } else if (startsWith(~prefix="~/", s)) {
    home ++ String.sub(s, 1, String.length(s) - 1);
  } else {
    s;
  };

/* Glob matching over '*' (any run, including empty) and '?' (exactly one).
 *
 * Recursive with a memo-free backtrack, which is fine here and only here:
 * these patterns are basenames from a hand-written config file, not
 * adversarial input, and the pathological "a*a*a*a*b" blowup needs several
 * stars to appear at all. Cell-accurate matching is deliberately NOT used -
 * a glob compares bytes, so a '?' matches one byte rather than one
 * character. That is only visible on a non-ASCII basename with a '?' in the
 * rule, which is not a case worth the complexity. */
let globMatch = (~pat: string, s: string): bool => {
  let pn = String.length(pat);
  let sn = String.length(s);
  let rec go = (pi: int, si: int): bool =>
    if (pi >= pn) {
      si >= sn;
    } else if (pat.[pi] == '*') {
      /* Try the shortest match first and grow it. */
      let rec tryFrom = (k: int): bool =>
        k > sn ? false : go(pi + 1, k) || tryFrom(k + 1);
      tryFrom(si);
    } else if (si >= sn) {
      false;
    } else if (pat.[pi] == '?' || pat.[pi] == s.[si]) {
      go(pi + 1, si + 1);
    } else {
      false;
    };
  go(0, 0);
};

/* --------------------------------------------------------------- parsing */

/* PURE. Turn one source line into a rule, a comment, or an error. */
let parseLine = (~home: string, text: string): parsed => {
  let trimmed = String.trim(text);
  if (trimmed == "" || trimmed.[0] == '#') {
    {text, rule: None, error: None};
  } else if (String.contains(trimmed, '/')) {
    if (startsWith(~prefix="/", trimmed) || startsWith(~prefix="~/", trimmed)
        || trimmed == "~") {
      let abs = stripTrailingSlash(expandHome(~home, trimmed));
      {text, rule: Some(Path(lower(abs))), error: None};
    } else {
      {
        text,
        rule: None,
        error:
          Some(
            "a rule with a '/' must be an absolute path, starting with '/' or '~/': "
            ++ trimmed,
          ),
      };
    };
  } else if (String.contains(trimmed, '*') || String.contains(trimmed, '?')) {
    {text, rule: Some(Glob(lower(trimmed))), error: None};
  } else {
    {text, rule: Some(Name(lower(trimmed))), error: None};
  };
};

/* PURE. Compile source lines into the three lookup structures. */
let compile = (~home: string, lines: array(string)): rules => {
  let source = Array.map(parseLine(~home), lines);
  let paths = Hashtbl.create(64);
  let names = Hashtbl.create(64);
  let globs = ref([]);
  Array.iter(
    p =>
      switch (p.rule) {
      | Some(Path(s)) => Hashtbl.replace(paths, s, ())
      | Some(Name(s)) => Hashtbl.replace(names, s, ())
      | Some(Glob(s)) => globs := [s, ...globs^]
      | None => ()
      },
    source,
  );
  {paths, names, globs: Array.of_list(List.rev(globs^)), source};
};

/* Every error line, for the UI to surface. A rule the user got wrong must
 * be visible - silently ignoring an ignore rule is the worst outcome. */
let errors = (r: rules): list(string) =>
  Array.to_list(r.source)
  |> List.filter_map(p => p.error);

/* --------------------------------------------------------------- matching */

/* PURE, and on the hot path - called once per directory entry.
 *
 * Cheapest test first. `name` alone answers most calls, and the caller has
 * it without building anything. */
let matchesName = (r: rules, name: string): bool => {
  let n = lower(name);
  Hashtbl.mem(r.names, n)
  || {
    let found = ref(false);
    let i = ref(0);
    let len = Array.length(r.globs);
    while (! found^ && i^ < len) {
      if (globMatch(~pat=r.globs[i^], n)) {
        found := true;
      };
      incr(i);
    };
    found^;
  };
};

let matchesPath = (r: rules, path: string): bool =>
  Hashtbl.mem(r.paths, lower(stripTrailingSlash(path)));

/* The full test Walk uses. */
let matches = (r: rules, ~name: string, ~path: string): bool =>
  matchesName(r, name) || matchesPath(r, path);

/* ---------------------------------------------------------- rule authoring */

/* An absolute path as it should be WRITTEN into the file: tilde-shortened,
 * so the file stays readable and survives being copied to another machine
 * with a different username. */
let toRule = (~home: string, path: string): string =>
  Fmt.tildify(~home, stripTrailingSlash(path));

/* Is this rule already present, as text? Compared against the source lines
 * rather than the compiled tables, because that is what we would append to
 * and what the user reads. */
let contains = (r: rules, rule: string): bool => {
  let want = lower(String.trim(rule));
  Array.exists(p => lower(String.trim(p.text)) == want, r.source);
};

/* --------------------------------------------------------- the defaults */

/* Written once, on first run, and never consulted again: after that the
 * file on disk is the single source of truth, so everything hog skips is
 * visible and editable rather than hidden in the binary.
 *
 * These are the things that are large, SLOW or DANGEROUS to walk - not
 * merely big. Deliberately absent: ~/.Trash (a full Trash is the best
 * finding this tool can produce, one keystroke from reclaimed), and
 * DerivedData / CoreSimulator / MobileSync / .git, which are frequently
 * enormous AND safe to delete. Those are the answer, not the noise. */
let defaults: array(string) = [|
  "# hog ignore file - one rule per line.",
  "#",
  "#   /abs/path  or  ~/path    ignore that directory and everything under it",
  "#   name       or  na*me     ignore any entry with that basename, at any depth",
  "#",
  "# '#' starts a comment. Matching is case-insensitive. There is no negation:",
  "# to un-ignore something, delete or comment out its line.",
  "",
  "# Cloud storage. Two reasons, and the second is the important one: a",
  "# dataless placeholder reports its FULL logical size while occupying no",
  "# local disk, and merely reading one can make the system download it.",
  "~/Library/CloudStorage",
  "~/Library/Mobile Documents",
  "",
  "# Other volumes and the synthetic mount layout. /System/Volumes/Data is a",
  "# firmlink onto the same data volume as /Users, so walking it counts the",
  "# whole disk twice. /Volumes also covers /Volumes/.timemachine, where APFS",
  "# local snapshots automount - re-walking the disk once per snapshot.",
  "/Volumes",
  "/System/Volumes",
  "/System",
  "/dev",
  "/net",
  "",
  "# Swap and the sleep image: RAM-sized, and not reclaimable by deleting them.",
  "/private/var/vm",
  "",
  "# Uncomment for much faster scans, at the cost of hiding real hoards:",
  "# node_modules",
|];

/* ================================================================== I/O ==
 *
 * Everything below this line touches the filesystem. Everything above it is
 * pure and testable without one.
 */

let configDir = (): string =>
  switch (Sys.getenv_opt("XDG_CONFIG_HOME")) {
  | Some(d) when d != "" => Filename.concat(d, "hog")
  | _ =>
    let home =
      switch (Sys.getenv_opt("HOME")) {
      | Some(h) => h
      | None => "."
      };
    Filename.concat(Filename.concat(home, ".config"), "hog");
  };

let filePath = (): string => Filename.concat(configDir(), "ignore");

let readLines = (path: string): array(string) => {
  let ic = open_in_bin(path);
  Fun.protect(
    ~finally=() => close_in_noerr(ic),
    () => {
      let n = in_channel_length(ic);
      let contents = really_input_string(ic, n);
      /* split_on_char leaves a trailing "" for a file ending in a newline;
         dropping it keeps a round-trip from growing a blank line each time. */
      let parts = String.split_on_char('\n', contents);
      let parts =
        switch (List.rev(parts)) {
        | ["", ...rest] => List.rev(rest)
        | _ => parts
        };
      Array.of_list(parts);
    },
  );
};

/* Write `lines` to `path` so that a crash or a full disk can never leave a
 * truncated file: build a complete temp file, fsync it, then rename over
 * the target. The temp file MUST be in the same directory - a different one
 * is potentially a different filesystem, and rename would fail with EXDEV. */
let writeAtomic = (~path: string, lines: array(string)): result(unit, string) => {
  let dir = Filename.dirname(path);
  let tmp = Filename.concat(dir, ".ignore.tmp." ++ string_of_int(Unix.getpid()));
  try({
    let fd = Unix.openfile(tmp, [Unix.O_WRONLY, Unix.O_CREAT, Unix.O_TRUNC], 0o644);
    Fun.protect(
      ~finally=() => (try(Unix.close(fd)) { | Unix.Unix_error(_, _, _) => () }),
      () => {
        let oc = Unix.out_channel_of_descr(fd);
        Array.iter(l => output_string(oc, l ++ "\n"), lines);
        flush(oc);
        Unix.fsync(fd);
      },
    );
    Unix.rename(tmp, path);
    Ok();
  }) {
  | Unix.Unix_error(e, _, _) =>
    (try(Sys.remove(tmp)) { | Sys_error(_) => () });
    Error(Unix.error_message(e));
  };
};

let rec mkdirP = (dir: string): unit =>
  if (dir != "" && dir != "/" && dir != "." && !Sys.file_exists(dir)) {
    mkdirP(Filename.dirname(dir));
    try(Unix.mkdir(dir, 0o755)) {
    | Unix.Unix_error(Unix.EEXIST, _, _) => ()
    };
  };

/* Read the ignore file, seeding it with `defaults` if it is not there yet.
 *
 * A missing file is not an error, and neither is an unwritable config
 * directory - hog still runs, just without persistence. Only a file that
 * exists and cannot be READ is worth reporting, because that is a real
 * surprise. */
let load = (~home: string): (rules, option(string)) => {
  let path = filePath();
  if (Sys.file_exists(path)) {
    switch (readLines(path)) {
    | lines => (compile(~home, lines), None)
    | exception (Sys_error(msg)) => (compile(~home, defaults), Some(msg))
    };
  } else {
    (
      compile(~home, defaults),
      switch (
        {
          mkdirP(configDir());
          writeAtomic(~path, defaults);
        }
      ) {
      | Ok () => None
      | Error(msg) => Some("could not create " ++ path ++ ": " ++ msg)
      | exception (Unix.Unix_error(e, _, _)) =>
        Some("could not create " ++ path ++ ": " ++ Unix.error_message(e))
      },
    );
  };
};

/* Append one rule and re-read from disk.
 *
 * Re-reading rather than trusting the in-memory array is deliberate: it is
 * the only way to be sure the compiled rules match what a later hand-edit
 * of the file will see. */
let addPath = (~home: string, r: rules, path: string): (rules, option(string)) => {
  let rule = toRule(~home, path);
  if (contains(r, rule)) {
    (r, None);
  } else {
    let header = "# added by hog";
    let existing = Array.map(p => p.text, r.source);
    let hasHeader = Array.exists(t => String.trim(t) == header, existing);
    let additions = hasHeader ? [|rule|] : [|"", header, rule|];
    let lines = Array.append(existing, additions);
    let file = filePath();
    mkdirP(configDir());
    switch (writeAtomic(~path=file, lines)) {
    | Error(msg) => (r, Some("could not write " ++ file ++ ": " ++ msg))
    | Ok () =>
      switch (readLines(file)) {
      | fresh => (compile(~home, fresh), None)
      | exception (Sys_error(msg)) => (compile(~home, lines), Some(msg))
      }
    };
  };
};

/* Remove a rule by its text. Used by the `i` toggle when the selected entry
 * is already ignored. */
let removePath = (~home: string, r: rules, path: string): (rules, option(string)) => {
  let rule = lower(String.trim(toRule(~home, path)));
  let kept =
    Array.of_list(
      List.filter(
        t => lower(String.trim(t)) != rule,
        Array.to_list(Array.map(p => p.text, r.source)),
      ),
    );
  let file = filePath();
  mkdirP(configDir());
  switch (writeAtomic(~path=file, kept)) {
  | Error(msg) => (r, Some("could not write " ++ file ++ ": " ++ msg))
  | Ok () =>
    switch (readLines(file)) {
    | fresh => (compile(~home, fresh), None)
    | exception (Sys_error(msg)) => (compile(~home, kept), Some(msg))
    }
  };
};
