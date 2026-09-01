/*
 * Cli.parse, against hand-built argv arrays. No filesystem is involved -
 * the whole grammar is decided before anything is resolved, which is the
 * point of keeping Cli pure.
 *
 * Each case is rendered to ONE string and compared whole, so a test that
 * fails says what it got rather than which of five fields disagreed.
 */

let showConfig = (c: Cli.config): string =>
  Printf.sprintf(
    "path=%s ignores=[%s] minSize=%d showIgnored=%b crossDevices=%b",
    switch (c.Cli.pathArg) {
    | Some(s) => s
    | None => "<none>"
    },
    String.concat(",", c.Cli.ignores),
    c.Cli.minSize,
    c.Cli.showIgnored,
    c.Cli.crossDevices,
  );

/* Parse [args] as if typed after the program name. */
let parse = (args: list(string)): Cli.parsed =>
  Cli.parse(Array.of_list(["hog", ...args]));

let show = (args: list(string)): string =>
  switch (parse(args)) {
  | Cli.Help => "help"
  | Cli.Invalid(msg) => "invalid: " ++ msg
  | Cli.Run(c) => showConfig(c)
  };

/* The message of an Invalid verdict; anything else fails loudly. */
let invalidMessage = (args: list(string)): string =>
  switch (parse(args)) {
  | Cli.Invalid(msg) => msg
  | Cli.Help => failwith("expected Invalid, got Help")
  | Cli.Run(c) => failwith("expected Invalid, got Run(" ++ showConfig(c) ++ ")")
  };

let defaultsLine =
  "path=<none> ignores=[] minSize=10485760 showIgnored=false crossDevices=false";

let run = () => {
  Test.group("cli: defaults and the PATH positional", () => {
    Test.run("no arguments means the default config", () =>
      Test.assertEqualStr(show([]), defaultsLine, "the bare default")
    );

    Test.run("one bare argument is PATH", () =>
      Test.assertEqualStr(
        show(["/tmp"]),
        "path=/tmp ignores=[] minSize=10485760 showIgnored=false crossDevices=false",
        "path only",
      )
    );

    Test.run("a lone dash is a path, not an option", () =>
      Test.assertEqualStr(
        show(["-"]),
        "path=- ignores=[] minSize=10485760 showIgnored=false crossDevices=false",
        "looksLikeOption requires length > 1",
      )
    );

    Test.run("a second positional is an error naming itself", () => {
      let msg = invalidMessage(["/tmp", "/var"]);
      Test.assertContains(msg, "too many arguments", "what went wrong");
      Test.assertContains(msg, "/var", "which argument was one too many");
      Test.assertContains(msg, "--help", "where to look");
    });
  });

  Test.group("cli: -i/--ignore", () => {
    Test.run("-i (short) appends one glob", () =>
      Test.assertEqualStr(
        show(["-i", "*.cache"]),
        "path=<none> ignores=[*.cache] minSize=10485760 showIgnored=false crossDevices=false",
        "short form",
      )
    );

    Test.run("--ignore (long) appends one glob", () =>
      Test.assertEqualStr(
        show(["--ignore", "*.cache"]),
        "path=<none> ignores=[*.cache] minSize=10485760 showIgnored=false crossDevices=false",
        "long form",
      )
    );

    Test.run("a repeated -i preserves order", () =>
      Test.assertEqualStr(
        show(["-i", "*.cache", "-i", "node_modules", "--ignore", ".git"]),
        "path=<none> ignores=[*.cache,node_modules,.git] minSize=10485760 showIgnored=false crossDevices=false",
        "first typed, first in the list",
      )
    );

    Test.run("-i with nothing left is an error", () => {
      let msg = invalidMessage(["-i"]);
      Test.assertContains(msg, "--ignore requires a value", "what went wrong");
      Test.assertContains(msg, "--help", "where to look");
    });
  });

  Test.group("cli: -m/--min-size", () => {
    Test.run("-m (short) sets minSize", () =>
      Test.assertEqualStr(
        show(["-m", "500K"]),
        "path=<none> ignores=[] minSize=512000 showIgnored=false crossDevices=false",
        "short form",
      )
    );

    Test.run("--min-size (long) sets minSize", () =>
      Test.assertEqualStr(
        show(["--min-size", "2G"]),
        Printf.sprintf(
          "path=<none> ignores=[] minSize=%d showIgnored=false crossDevices=false",
          2 * 1024 * 1024 * 1024,
        ),
        "long form",
      )
    );

    Test.run("--min-size with nothing left is an error", () =>
      Test.assertContains(
        invalidMessage(["--min-size"]),
        "--min-size requires a value",
        "what went wrong",
      )
    );

    Test.run("--min-size with an invalid size is an error", () => {
      let msg = invalidMessage(["--min-size", "10X"]);
      Test.assertContains(msg, "invalid size", "what went wrong");
      Test.assertContains(msg, "10X", "the offending value");
      Test.assertContains(msg, "--help", "where to look");
    });
  });

  Test.group("cli: -a/--all and --cross-devices", () => {
    Test.run("-a (short) sets showIgnored", () =>
      Test.assertEqualStr(
        show(["-a"]),
        "path=<none> ignores=[] minSize=10485760 showIgnored=true crossDevices=false",
        "short form",
      )
    );

    Test.run("--all (long) sets showIgnored", () =>
      Test.assertEqualStr(
        show(["--all"]),
        "path=<none> ignores=[] minSize=10485760 showIgnored=true crossDevices=false",
        "long form",
      )
    );

    Test.run("--cross-devices sets crossDevices", () =>
      Test.assertEqualStr(
        show(["--cross-devices"]),
        "path=<none> ignores=[] minSize=10485760 showIgnored=false crossDevices=true",
        "no short form for this one",
      )
    );

    Test.run("flags combine freely with a positional", () =>
      Test.assertEqualStr(
        show(["-a", "--cross-devices", "-m", "1500", "/data"]),
        "path=/data ignores=[] minSize=1500 showIgnored=true crossDevices=true",
        "position among flags does not matter",
      )
    );
  });

  Test.group("cli: parseSize", () => {
    Test.run("a plain byte count", () =>
      Test.assertEqualInt(
        Option.get(Cli.parseSize("1500")),
        1500,
        "no suffix means bytes",
      )
    );

    Test.run("K is 1024-based", () =>
      Test.assertEqualInt(Option.get(Cli.parseSize("500K")), 512000, "500 * 1024")
    );

    Test.run("M is 1024^2-based", () =>
      Test.assertEqualInt(
        Option.get(Cli.parseSize("10M")),
        10 * 1024 * 1024,
        "10 * 1024 * 1024",
      )
    );

    Test.run("GB (with the optional trailing B) is accepted", () =>
      Test.assertEqualInt(
        Option.get(Cli.parseSize("2GB")),
        2 * 1024 * 1024 * 1024,
        "the trailing B is optional and ignored",
      )
    );

    Test.run("zero is a legal size", () =>
      Test.assertEqualInt(Option.get(Cli.parseSize("0")), 0, "hides nothing")
    );

    Test.run("an unknown unit is rejected", () =>
      Test.assertTrue(Cli.parseSize("10X") == None, "X is not a unit")
    );

    Test.run("an empty string is rejected", () =>
      Test.assertTrue(Cli.parseSize("") == None, "nothing to parse")
    );

    Test.run("a negative number is rejected", () =>
      Test.assertTrue(Cli.parseSize("-5") == None, "sizes are not negative")
    );

    Test.run("a fractional value is rejected", () =>
      Test.assertTrue(
        Cli.parseSize("1.5G") == None,
        "integers only, by contract - see the comment on parseSize",
      )
    );
  });

  Test.group("cli: help", () => {
    Test.run("-h and --help both ask for help", () => {
      Test.assertEqualStr(show(["-h"]), "help", "-h");
      Test.assertEqualStr(show(["--help"]), "help", "--help");
    });

    Test.run("help beats an error - asking for help is never a failure", () =>
      Test.assertEqualStr(
        show(["--bogus", "--help"]),
        "help",
        "even next to an unknown option",
      )
    );

    Test.run("the usage text covers every documented piece", () => {
      Test.assertContains(Cli.usage, "hog [OPTIONS] [PATH]", "the synopsis");
      Test.assertContains(Cli.usage, "-i, --ignore", "the ignore flag");
      Test.assertContains(Cli.usage, "-m, --min-size", "the min-size flag");
      Test.assertContains(Cli.usage, "-a, --all", "the all flag");
      Test.assertContains(Cli.usage, "--cross-devices", "the cross-devices flag");
      Test.assertContains(Cli.usage, "-h, --help", "the help flag");
      Test.assertContains(Cli.usage, "~/.config/hog/ignore", "where the ignore file lives");
    });
  });

  Test.group("cli: unknown options and the -- sentinel", () => {
    Test.run("a long unknown option is named back", () => {
      let msg = invalidMessage(["--bogus"]);
      Test.assertContains(msg, "unknown option: --bogus", "the offending flag");
      Test.assertContains(msg, "--help", "where to look");
    });

    Test.run("-- ends option parsing", () =>
      Test.assertEqualStr(
        show(["--", "--all"]),
        "path=--all ignores=[] minSize=10485760 showIgnored=false crossDevices=false",
        "a path literally called --all",
      )
    );

    Test.run("--help after -- is a path, not a request for help", () =>
      Test.assertEqualStr(
        show(["--", "--help"]),
        "path=--help ignores=[] minSize=10485760 showIgnored=false crossDevices=false",
        "the pre-scan stops at the sentinel",
      )
    );
  });
};
