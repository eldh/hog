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
          let home =
            switch (Sys.getenv_opt("HOME")) {
            | Some(h) => h
            | None => root
            };

          /* The ignore file, created with the shipped defaults on first run.
             Its warning - an unreadable file, or a config directory that
             could not be created - is printed HERE, on the ordinary terminal,
             because under Fullscreen there is no scrollback to commit it to
             once the app is running. */
          let (rules, warning) = Ignore.load(~home);
          switch (warning) {
          | Some(msg) => printErr(msg)
          | None => ()
          };

          /* -i/--ignore rules are kept SEPARATE from the file's rules rather
             than merged into them. Both are consulted by the walk, but only
             the file's rules are handed to the app - and the app's `i` key
             rewrites exactly that set back to disk. Merging would silently
             persist a one-off command-line glob into the user's ignore file
             the first time they pressed `i`. */
          let cliRules =
            Ignore.compile(~home, Array.of_list(config.Cli.ignores));
          let shouldIgnore = (~name: string, ~path: string) =>
            Ignore.matches(rules, ~name, ~path)
            || Ignore.matches(cliRules, ~name, ~path);

          /* --min-size is a display and ranking floor, never a walk filter:
             filtering during the walk would corrupt every parent total. */
          let params = {...Rank.defaults, Rank.minBytes: config.Cli.minSize};
          let scan =
            Scan.start(
              ~root,
              ~home,
              ~shouldIgnore,
              ~crossDevices=config.Cli.crossDevices,
              ~params,
              (),
            );

          /* Fullscreen, not Inline: the root Flexes to fill the terminal, and
             an inline app that tall scrolls the user's shell history away for
             good. The alternate screen is restored exactly on exit. */
          Matcha.Runtime.start(
            ~screen=Fullscreen,
            HogApp.app(~scan, ~config, ~rules, ~trash=Trash.move),
          );
        }
      };
    };
  };
