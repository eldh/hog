/*
 * hog's launcher.
 *
 * Everything real lives in the hogApp library so test/app_tests.re can start
 * the exact same component headlessly - this file is deliberately thin: it
 * owns exactly what a component cannot own itself, namely printing and exit
 * codes, and it owns them in a strict order.
 *
 * WHY EVERY FAILURE PATH RETURNS BEFORE Runtime.start: once the interactive
 * runtime starts it may switch to the alternate screen (Fullscreen mode).
 * An error discovered only after that point would flash the alt screen on
 * and back off around a message the user has to scroll to find. Resolving
 * the root, and checking that it exists and is a directory, both happen
 * here, on the ordinary terminal, with a plain exit(1) - never inside the
 * component tree.
 */

let printErr = (msg: string): unit => {
  prerr_endline("hog: " ++ msg);
};

/* ~ and ~/rest, expanded against $HOME. Only a LEADING ~ is special, exactly
   like a shell's own tilde expansion - "a/~/b" is left alone. No attempt is
   made to expand "~other-user" (Sys.getenv_opt has no way to look that up
   without a passwd lookup, and hog does not need it). */
let expandTilde = (path: string): string =>
  if (path == "~") {
    switch (Sys.getenv_opt("HOME")) {
    | Some(home) => home
    | None => path
    };
  } else if (String.length(path) >= 2 && path.[0] == '~' && path.[1] == '/') {
    switch (Sys.getenv_opt("HOME")) {
    | Some(home) => home ++ String.sub(path, 1, String.length(path) - 1)
    | None => path
    };
  } else {
    path;
  };

let () =
  switch (Cli.parse(Sys.argv)) {
  | Cli.Help =>
    /* Stdout, exit 0: `hog --help | less` works, and a wrapper script can
       tell "help was requested" apart from "something went wrong". */
    print_string(Cli.usage);
    exit(0);
  | Cli.Invalid(msg) =>
    printErr(msg);
    exit(1);
  | Cli.Run(config) =>
    let raw =
      switch (config.Cli.pathArg) {
      | Some(p) => Some(p)
      | None => Sys.getenv_opt("HOME")
      };
    switch (raw) {
    | None =>
      /* No PATH given and $HOME unset - nothing to default to. Rare (only
         a stripped-down or misconfigured environment hits this), but it
         must fail here rather than handing Scan an empty string. */
      printErr("no PATH given and $HOME is not set");
      exit(1);
    | Some(rawPath) =>
      let expanded = expandTilde(rawPath);
      switch (Unix.realpath(expanded)) {
      | exception _ =>
        /* realpath itself fails when the path (or a component of it) does
           not exist - report the same "no such directory" a missing leaf
           would get, rather than a raw Unix_error. */
        printErr("no such directory: " ++ rawPath);
        exit(1);
      | root =>
        if (!Sys.file_exists(root)) {
          printErr("no such directory: " ++ rawPath);
          exit(1);
        } else if (!Sys.is_directory(root)) {
          printErr("not a directory: " ++ root);
          exit(1);
        } else {
          /* TODO: once Ignore, Scan and HogApp land, this becomes:
           *   let ignoreList = Ignore.load(~extra=config.Cli.ignores);
           *   let handle = Scan.start(~root, ~ignores=ignoreList, ~config, ...);
           *   Matcha.Runtime.start(~screen=Fullscreen, HogApp.make(~handle, ...));
           * For now, print what was resolved so the CLI plumbing is
           * checkable end to end before any of those modules exist. */
          Printf.printf("root: %s\n", root);
          Printf.printf(
            "config: ignores=[%s] minSize=%d showIgnored=%b crossDevices=%b\n",
            String.concat(",", config.Cli.ignores),
            config.Cli.minSize,
            config.Cli.showIgnored,
            config.Cli.crossDevices,
          );
          exit(0);
        }
      };
    };
  };
