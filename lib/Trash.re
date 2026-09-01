/*
 * Move to Trash, on macOS, through Finder.
 *
 * WHY FINDER AND NOT `mv ~/.Trash`
 * -------------------------------
 * A rename into ~/.Trash looks equivalent and is not. Finder records the
 * "Put Back" metadata, so the user can undo the decision from the Trash
 * window; a rename cannot, and the file is then only restorable by someone
 * who remembers where it came from. Finder also REFUSES a cross-volume move
 * instead of silently degrading it to a copy - which on a tool whose whole
 * purpose is reclaiming disk space would consume the space it was asked to
 * free before freeing any.
 *
 * WHY NO SHELL, EVER
 * ------------------
 * The path comes from a directory tree the user pointed us at, which is not
 * the same thing as a path the user typed. Names containing quotes, dollars,
 * backticks and newlines are legal on every filesystem hog walks, and a
 * single `Sys.command` here would turn one of them into arbitrary code
 * execution. `Unix.create_process` passes argv straight through with no
 * word splitting anywhere, so the only quoting that has to be right is the
 * AppleScript string literal built by [script] - which is pure, and pinned
 * by its own tests.
 *
 * [script] is separated from [move] precisely so the escaping can be tested
 * without ever moving a file: the test suite for this module runs nothing.
 */

/* The AppleScript string-literal escape. Inside a `"..."` literal only two
 * characters need escaping, and the backslash MUST be handled before the
 * quote - a two-pass replace that does the quote first turns `"` into `\"`
 * and then the second pass escapes the backslash it just wrote, yielding
 * `\\"`, which closes the literal. Rewriting every character in ONE pass,
 * as below, cannot get that order wrong: each input character is looked at
 * once and never re-examined. */
let escape = (s: string): string => {
  let buf = Buffer.create(String.length(s) + 8);
  String.iter(
    c =>
      switch (c) {
      | '\\' => Buffer.add_string(buf, "\\\\")
      | '"' => Buffer.add_string(buf, "\\\"")
      | c => Buffer.add_char(buf, c)
      },
    s,
  );
  Buffer.contents(buf);
};

/* The one-liner handed to `osascript -e`. Pure: builds a string, touches
 * nothing. `POSIX file` is what turns a slash path into the file reference
 * Finder's `delete` expects. */
let script = (path: string): string =>
  "tell application \"Finder\" to delete POSIX file \"" ++ escape(path) ++ "\"";

/* The TCC failure. macOS reports a missing Automation permission as an
 * ordinary osascript error, and the raw text names neither the setting nor
 * where it lives, so a user who hits it has no way to act on it. */
let tccNeedle = "Not authorized to send Apple events to Finder";

let tccMessage = "hog needs permission to control Finder. Grant it in System Settings \xe2\x80\xba Privacy & Security \xe2\x80\xba Automation, under the terminal application hog is running in.";

let containsSub = (haystack: string, needle: string): bool => {
  let hl = String.length(haystack);
  let nl = String.length(needle);
  let found = ref(false);
  let i = ref(0);
  while (! found^ && i^ + nl <= hl) {
    if (String.sub(haystack, i^, nl) == needle) {
      found := true;
    };
    incr(i);
  };
  found^;
};

/* The first line with anything on it. osascript puts the interesting part
 * of a failure on its first line and location noise after it. */
let firstNonEmptyLine = (s: string): string => {
  let lines = String.split_on_char('\n', s);
  switch (List.find_opt(l => String.trim(l) != "", lines)) {
  | Some(l) => String.trim(l)
  | None => ""
  };
};

/* Drain both pipes concurrently.
 *
 * Reading stdout to EOF and only then stderr is the obvious shape and it
 * deadlocks the moment the child writes more to stderr than the pipe buffer
 * holds while we are blocked on stdout. osascript's output is short today,
 * but "short today" is not a property this code can rely on, and a hung
 * child here would hang hog's UI thread. select() over both ends costs a
 * dozen lines and removes the failure mode entirely. */
let drain = (out: Unix.file_descr, err: Unix.file_descr): (string, string) => {
  let bo = Buffer.create(512);
  let be = Buffer.create(512);
  let chunk = Bytes.create(4096);
  let open_ = ref([out, err]);
  while (open_^ != []) {
    let (ready, _, _) =
      try(Unix.select(open_^, [], [], 1.0)) {
      | Unix.Unix_error(Unix.EINTR, _, _) => ([], [], [])
      };
    List.iter(
      fd => {
        let n =
          try(Unix.read(fd, chunk, 0, Bytes.length(chunk))) {
          | Unix.Unix_error(_, _, _) => 0
          };
        if (n == 0) {
          open_ := List.filter(f => f != fd, open_^);
        } else {
          Buffer.add_subbytes(fd == out ? bo : be, chunk, 0, n);
        };
      },
      ready,
    );
  };
  (Buffer.contents(bo), Buffer.contents(be));
};

/*
 * Move [path] to the Trash. Returns Ok() only when osascript exited 0.
 *
 * The existence re-check is not paranoia about races - it is that the
 * confirmation dialog names a path the user read some seconds ago, and the
 * entry may have been removed by something else in between. Without it
 * Finder's own "file not found" wording is what the user would see, which
 * reads like hog failed rather than like the job was already done.
 */
let move = (path: string): result(unit, string) =>
  if (path == "") {
    Error("no path given");
  } else if (! Sys.file_exists(path)) {
    Error("no longer there: " ++ path);
  } else {
    /* /dev/null first, so a failure to open it cannot leak a pipe pair.
     * osascript inherits it as stdin: with hog's own stdin (a terminal in
     * raw mode) the child could read the user's keystrokes out from under
     * the UI. */
    let devnull = Unix.openfile("/dev/null", [Unix.O_RDONLY], 0);
    let (outR, outW) = Unix.pipe();
    let (errR, errW) = Unix.pipe();
    switch (
      Unix.create_process(
        "osascript",
        [|"osascript", "-e", script(path)|],
        devnull,
        outW,
        errW,
      )
    ) {
    | exception (Unix.Unix_error(e, _, _)) =>
      List.iter(Unix.close, [outR, outW, errR, errW, devnull]);
      Error("could not run osascript: " ++ Unix.error_message(e));
    | pid =>
      /* The parent must drop its copies of the write ends, or the reads
         below never see EOF: the pipe stays open because WE still hold it. */
      Unix.close(outW);
      Unix.close(errW);
      let (_stdout, stderr) = drain(outR, errR);
      let (_, status) = Unix.waitpid([], pid);
      List.iter(Unix.close, [outR, errR, devnull]);
      switch (status) {
      | Unix.WEXITED(0) => Ok()
      | Unix.WEXITED(_) =>
        let line = firstNonEmptyLine(stderr);
        if (containsSub(stderr, tccNeedle)) {
          Error(tccMessage);
        } else if (line == "") {
          Error("Finder refused to move it, without saying why");
        } else {
          Error(line);
        };
      | Unix.WSIGNALED(n) =>
        Error("osascript killed by signal " ++ string_of_int(n))
      | Unix.WSTOPPED(n) =>
        Error("osascript stopped by signal " ++ string_of_int(n))
      };
    };
  };
