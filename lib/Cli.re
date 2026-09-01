/*
 * Cli - the command line: `hog [OPTIONS] [PATH]`.
 *
 * Pure, and free of I/O. `parse` takes an argv array and returns a verdict;
 * bin/main.re owns the printing, the exit codes, ~-expansion and the
 * filesystem checks. That split is what lets test/cli_tests.re exercise the
 * whole grammar with hand-built arrays and no directory on disk anywhere.
 */

/* ============================================================================
 * Types
 * ============================================================================ */

type config = {
  /* The PATH positional, exactly as typed - no ~-expansion, no
     Unix.realpath, no existence check. None means "the caller works it out"
     (bin/main.re: $HOME). */
  pathArg: option(string),
  /* -i/--ignore GLOB, repeatable, kept in the order given. What each glob
     means is Ignore's business, not Cli's - this is just the list of raw
     strings the user typed. */
  ignores: list(string),
  /* -m/--min-size SIZE, already resolved to bytes by parseSize. A display
     and ranking floor only - see the module note below. */
  minSize: int,
  /* -a/--all: show entries the ignore list would otherwise hide, from the
     first frame, rather than requiring the user to toggle it on. */
  showIgnored: bool,
  /* --cross-devices: follow the walk across filesystem boundaries instead
     of stopping at them (the default, so a `du`-style scan from / does not
     wander into every mounted volume). */
  crossDevices: bool,
};

type parsed =
  | Help
  | Run(config)
  | Invalid(string);

/* 10 MiB. Big enough that a run against $HOME leads with real hoards
   instead of every node_modules and .git, small enough that nothing
   worth seeing is hidden by default. */
let defaultMinSize = 10 * 1024 * 1024;

/* What a bare `hog` means. Also the starting point every flag mutates, and
   the default for test helpers that do not care about the command line. */
let defaultConfig = {
  pathArg: None,
  ignores: [],
  minSize: defaultMinSize,
  showIgnored: false,
  crossDevices: false,
};

/* ============================================================================
 * The help text
 * ============================================================================ */

let usage = {|hog [OPTIONS] [PATH]

hog scans a directory tree and ranks the biggest things it finds anywhere
underneath, at any depth - a forgotten cache or build directory surfaces
without being hunted for. Walk into an entry to see its contents, filter by
name, and move things to the Trash.

  PATH                 Directory to scan. Default: $HOME.

Options:
  -i, --ignore GLOB    Ignore paths matching GLOB. Repeatable. The ignore
                        list also lives at ~/.config/hog/ignore, and is
                        pruned during the walk rather than filtered after it.
  -m, --min-size SIZE  Hide entries below SIZE (500K, 10M, 2G). Default: 10M.
                        A display and ranking floor only - it never affects
                        the walk itself, so hiding small entries can never
                        corrupt a parent's total.
  -a, --all            Show ignored entries from the start.
      --cross-devices  Follow the walk into other filesystems.
  -h, --help           Print this help and exit.
|};

/* Tail of every Invalid message. Short on purpose: the full usage is one
   flag away, and an error should not bury its own first line. */
let hint = "try 'hog --help' for usage.";

/* ============================================================================
 * Sizes
 * ============================================================================ */

/*
 * Parse a size argument: a plain byte count ("1500") or a suffixed one
 * ("500K", "10M", "2G", "2GB", "1T"), case-insensitively, with an optional
 * trailing "B". Multipliers are 1024-based (K = 1024, M = 1024^2, ...).
 *
 * INTEGERS ONLY - "1.5G" is rejected, not truncated. A command line is typed
 * once and read many times in a --help screen; "whole units only" is a
 * simpler contract to document and to type than a fractional one, and
 * hog's sizes are coarse enough (10M steps and up) that a fraction buys
 * nothing.
 */
let parseSize = (s: string): option(int) => {
  let s = String.trim(s);
  let len = String.length(s);
  if (len == 0) {
    None;
  } else {
    /* Strip a trailing 'b'/'B' first ("2GB" -> "2G"), then the unit letter
       itself, so "B" alone (plain bytes, explicitly marked) also survives
       as an empty suffix. */
    let upper = String.uppercase_ascii(s);
    let (digits, mult) =
      if (String.length(upper) >= 2 && upper.[len - 1] == 'B'
          && (upper.[len - 2] == 'K' || upper.[len - 2] == 'M'
              || upper.[len - 2] == 'G' || upper.[len - 2] == 'T')) {
        (String.sub(s, 0, len - 2), upper.[len - 2]);
      } else {
        (String.sub(s, 0, len - 1), upper.[len - 1]);
      };
    let (numPart, multiplier) =
      switch (mult) {
      | 'K' => (digits, Some(1024))
      | 'M' => (digits, Some(1024 * 1024))
      | 'G' => (digits, Some(1024 * 1024 * 1024))
      | 'T' => (digits, Some(1024 * 1024 * 1024 * 1024))
      | 'B' => (digits, Some(1)) /* trailing "B" alone: plain bytes */
      | _ => (s, Some(1)) /* no recognised unit: try the whole string */
      };
    switch (multiplier) {
    | None => None
    | Some(m) =>
      if (String.length(numPart) == 0) {
        None;
      } else {
        /* int_of_string_opt already rejects a fractional value ("1.5"),
           a sign other than a leading digit, and anything non-numeric -
           which is exactly the "integers only" contract this function
           promises. It does accept a leading '+' or embedded '_', which is
           harmless enough not to special-case away. */
        switch (int_of_string_opt(numPart)) {
        | Some(n) when n >= 0 => Some(n * m)
        | _ => None
        };
      }
    };
  };
};

/* ============================================================================
 * Parsing
 * ============================================================================ */

let isHelpFlag = (a: string): bool => a == "-h" || a == "--help";

/* Does this argument LOOK like an option?
 *
 * A lone "-" does NOT - it stays a positional (conventionally "read stdin"
 * or "the previous thing" in tools that accept it; hog has no such meaning
 * today, but the shape of the rule is the same as bdiff's and is worth
 * keeping consistent). Everything else starting with "-" is treated as an
 * option, which means a PATH that begins with a dash needs the "--"
 * sentinel. */
let looksLikeOption = (a: string): bool =>
  String.length(a) > 1 && a.[0] == '-';

/* Is -h/--help anywhere in the argument list?
 *
 * Asking for help is answered with help, never with an error: `hog --bogus
 * --help` prints the usage rather than complaining about --bogus. The scan
 * stops at the "--" sentinel, after which "--help" is an ordinary
 * positional. */
let rec wantsHelp = (args: list(string)): bool =>
  switch (args) {
  | [] => false
  | ["--", ..._] => false
  | [a, ...rest] => isHelpFlag(a) || wantsHelp(rest)
  };

/*
 * The grammar, in one pass:
 *
 *   --                    ends option parsing; every later argument is a
 *                         positional, however many dashes it starts with.
 *   -i, --ignore GLOB     appends GLOB to ignores, in order.
 *   -m, --min-size SIZE   parses SIZE with parseSize; a bad SIZE is Invalid.
 *   -a, --all             sets showIgnored.
 *   --cross-devices       sets crossDevices.
 *   -h, --help            handled by the pre-scan above, so it never reaches
 *                         here.
 *   a flag needing a value with nothing left -> Invalid("--x requires a
 *                         value").
 *   any other -x          Invalid("unknown option: ...").
 *   anything else         the first bare argument is PATH; a second is
 *                         Invalid("too many arguments...").
 *
 * [argv] is Sys.argv, so element 0 is the program name and is dropped.
 */
let parse = (argv: array(string)): parsed => {
  let args =
    Array.length(argv) <= 1
      ? []
      : Array.to_list(Array.sub(argv, 1, Array.length(argv) - 1));

  if (wantsHelp(args)) {
    Help;
  } else {
    let rec go = (rest: list(string), sentinel: bool, cfg: config): parsed =>
      switch (rest) {
      | [] => Run(cfg)
      | ["--", ...tail] when !sentinel => go(tail, true, cfg)
      | [("-i" | "--ignore"), ...tail] when !sentinel =>
        switch (tail) {
        | [] => Invalid("--ignore requires a value\n" ++ hint)
        | [glob, ...tail'] =>
          go(tail', sentinel, {...cfg, ignores: cfg.ignores @ [glob]})
        }
      | [("-m" | "--min-size"), ...tail] when !sentinel =>
        switch (tail) {
        | [] => Invalid("--min-size requires a value\n" ++ hint)
        | [raw, ...tail'] =>
          switch (parseSize(raw)) {
          | Some(bytes) => go(tail', sentinel, {...cfg, minSize: bytes})
          | None =>
            Invalid(
              "invalid size: " ++ raw ++ " (try 500K, 10M, 2G)\n" ++ hint,
            )
          }
        }
      | [("-a" | "--all"), ...tail] when !sentinel =>
        go(tail, sentinel, {...cfg, showIgnored: true})
      | ["--cross-devices", ...tail] when !sentinel =>
        go(tail, sentinel, {...cfg, crossDevices: true})
      | [a, ..._] when !sentinel && looksLikeOption(a) =>
        Invalid("unknown option: " ++ a ++ "\n" ++ hint)
      | [a, ...tail] =>
        switch (cfg.pathArg) {
        | None => go(tail, sentinel, {...cfg, pathArg: Some(a)})
        | Some(_) =>
          Invalid(
            "too many arguments (at most one PATH): " ++ a ++ "\n" ++ hint,
          )
        }
      };
    go(args, false, defaultConfig);
  };
};
