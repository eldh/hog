/*
 * Tests for lib/Ignore.re.
 *
 * The pure half (parse, compile, match) is tested with no filesystem at
 * all. The I/O half is tested against a throwaway config directory, by
 * pointing XDG_CONFIG_HOME at a temp dir - which is also a check on
 * Ignore.configDir honouring it, since every one of those tests would
 * otherwise scribble on the developer's real ~/.config/hog/ignore.
 */

let home = "/Users/tester";

/* ------------------------------------------------------------ pure tests */

let run = () =>
  Test.group("ignore: parsing", () => {
    Test.run("comments and blank lines carry no rule and no error", () =>
      List.iter(
        line => {
          let p = Ignore.parseLine(~home, line);
          Test.assertTrue(p.Ignore.rule == None, "\"" ++ line ++ "\" has no rule");
          Test.assertTrue(p.Ignore.error == None, "\"" ++ line ++ "\" has no error");
          Test.assertEqualStr(p.Ignore.text, line, "text round-trips verbatim");
        },
        ["", "   ", "# a comment", "   # indented comment"],
      )
    );

    Test.run("a bare word is a basename rule", () =>
      switch (Ignore.parseLine(~home, "node_modules").Ignore.rule) {
      | Some(Ignore.Name(n)) => Test.assertEqualStr(n, "node_modules", "name")
      | _ => Test.assertTrue(false, "expected a Name rule")
      }
    );

    Test.run("a word with a wildcard is a glob rule", () =>
      switch (Ignore.parseLine(~home, "*.log").Ignore.rule) {
      | Some(Ignore.Glob(g)) => Test.assertEqualStr(g, "*.log", "glob")
      | _ => Test.assertTrue(false, "expected a Glob rule")
      }
    );

    Test.run("a leading ~ expands against home", () =>
      switch (Ignore.parseLine(~home, "~/Library/Caches").Ignore.rule) {
      | Some(Ignore.Path(p)) =>
        Test.assertEqualStr(p, "/users/tester/library/caches", "expanded and lowercased")
      | _ => Test.assertTrue(false, "expected a Path rule")
      }
    );

    Test.run("an absolute path is a path rule", () =>
      switch (Ignore.parseLine(~home, "/Volumes").Ignore.rule) {
      | Some(Ignore.Path(p)) => Test.assertEqualStr(p, "/volumes", "lowercased")
      | _ => Test.assertTrue(false, "expected a Path rule")
      }
    );

    Test.run("a trailing slash is not a different rule", () => {
      let a = Ignore.parseLine(~home, "~/Code").Ignore.rule;
      let b = Ignore.parseLine(~home, "~/Code/").Ignore.rule;
      Test.assertTrue(a == b, "\"~/Code\" and \"~/Code/\" compile the same");
    });

    Test.run("a relative path with a slash is a reported error", () => {
      /* The important half of this is the SECOND assertion: a rule the user
         got wrong has to be visible. Silently dropping an ignore rule is
         the worst possible outcome - the user believes a directory is
         being skipped and it is not. */
      let p = Ignore.parseLine(~home, "Library/Caches");
      Test.assertTrue(p.Ignore.rule == None, "no rule is produced");
      switch (p.Ignore.error) {
      | None => Test.assertTrue(false, "expected an error")
      | Some(msg) =>
        Test.assertContains(msg, "absolute path", "error explains the rule");
        Test.assertContains(msg, "Library/Caches", "error quotes the offending line");
      };
    });

    Test.run("errors are collected for the UI", () => {
      let r = Ignore.compile(~home, [|"good", "bad/path", "also/bad", "/fine"|]);
      Test.assertEqualInt(List.length(Ignore.errors(r)), 2, "two error lines");
    });
  });

let runMatching = () =>
  Test.group("ignore: matching", () => {
    let rules =
      Ignore.compile(
        ~home,
        [|"node_modules", "*.log", "~/Library/Caches", "/Volumes", "# comment"|],
      );

    Test.run("a basename rule matches at any depth", () => {
      Test.assertTrue(
        Ignore.matches(rules, ~name="node_modules", ~path="/a/b/node_modules"),
        "deep",
      );
      Test.assertTrue(
        Ignore.matches(rules, ~name="node_modules", ~path="/node_modules"),
        "shallow",
      );
    });

    Test.run("matching is case-insensitive, because APFS is", () =>
      Test.assertTrue(
        Ignore.matches(rules, ~name="Node_Modules", ~path="/a/Node_Modules"),
        "mixed case still matches",
      )
    );

    Test.run("a glob matches basenames", () => {
      Test.assertTrue(Ignore.matchesName(rules, "debug.log"), "*.log matches");
      Test.assertFalse(Ignore.matchesName(rules, "log.txt"), "*.log does not match log.txt");
    });

    Test.run("a path rule matches only that exact directory", () => {
      Test.assertTrue(
        Ignore.matches(rules, ~name="Caches", ~path="/Users/tester/Library/Caches"),
        "the rule's own path",
      );
      /* Not a limitation - it is the pruning contract. We never SEE anything
         under an ignored directory, so a path rule never has to match a
         descendant, which is what lets this be one hashtable lookup instead
         of a prefix walk. */
      Test.assertFalse(
        Ignore.matches(
          rules,
          ~name="WebKit",
          ~path="/Users/tester/Library/Caches/WebKit",
        ),
        "a descendant is not matched (it is never reached)",
      );
    });

    Test.run("an unrelated entry matches nothing", () =>
      Test.assertFalse(
        Ignore.matches(rules, ~name="Documents", ~path="/Users/tester/Documents"),
        "no rule applies",
      )
    );

    Test.run("a comment line contributes no rule", () =>
      Test.assertFalse(Ignore.matchesName(rules, "comment"), "'# comment' is not a rule")
    );

    Test.run("globMatch handles * and ? and anchors both ends", () => {
      Test.assertTrue(Ignore.globMatch(~pat="*", "anything"), "* matches all");
      Test.assertTrue(Ignore.globMatch(~pat="*.log", "a.log"), "suffix");
      Test.assertTrue(Ignore.globMatch(~pat="a*z", "abcz"), "middle");
      Test.assertTrue(Ignore.globMatch(~pat="a?c", "abc"), "? is exactly one");
      Test.assertFalse(Ignore.globMatch(~pat="a?c", "ac"), "? is not zero");
      Test.assertFalse(Ignore.globMatch(~pat="a?c", "abbc"), "? is not two");
      Test.assertFalse(Ignore.globMatch(~pat="*.log", "a.log.bak"), "anchored at the end");
      Test.assertTrue(Ignore.globMatch(~pat="*", ""), "* matches empty");
      Test.assertTrue(Ignore.globMatch(~pat="", ""), "empty matches empty");
      Test.assertFalse(Ignore.globMatch(~pat="", "x"), "empty matches nothing else");
    });

    Test.run("the shipped defaults compile without errors", () => {
      let d = Ignore.compile(~home, Ignore.defaults);
      Test.assertEqualInt(
        List.length(Ignore.errors(d)),
        0,
        "no default line is malformed",
      );
      Test.assertTrue(
        Ignore.matchesPath(d, "/Volumes"),
        "/Volumes is ignored by default",
      );
      Test.assertTrue(
        Ignore.matchesPath(d, "/Users/tester/Library/CloudStorage"),
        "CloudStorage is ignored by default",
      );
      /* Deliberate: a full Trash is the single best finding this tool can
         produce, and DerivedData is huge and safe to delete. Both are the
         ANSWER, not the noise, so neither may be ignored by default. */
      Test.assertFalse(
        Ignore.matchesPath(d, "/Users/tester/.Trash")
        || Ignore.matchesName(d, ".Trash"),
        ".Trash is NOT ignored by default",
      );
      Test.assertFalse(
        Ignore.matchesName(d, "DerivedData"),
        "DerivedData is NOT ignored by default",
      );
      Test.assertFalse(
        Ignore.matchesName(d, "node_modules"),
        "node_modules ships commented out, so it is not active",
      );
    });
  });

/* -------------------------------------------------------------- I/O tests */

let withTempConfig = (f: unit => unit): unit => {
  let dir = Filename.temp_file("hog-cfg-", "");
  Sys.remove(dir);
  Unix.mkdir(dir, 0o755);
  let saved = Sys.getenv_opt("XDG_CONFIG_HOME");
  Unix.putenv("XDG_CONFIG_HOME", dir);
  Fun.protect(
    ~finally=() => {
      switch (saved) {
      | Some(v) => Unix.putenv("XDG_CONFIG_HOME", v)
      /* There is no unsetenv in OCaml's Unix; an empty value is treated as
         absent by Ignore.configDir, which is exactly why that check is
         written as `Some(d) when d != ""`. */
      | None => Unix.putenv("XDG_CONFIG_HOME", "")
      };
      let rec rm = p =>
        if (Sys.file_exists(p)) {
          if (Sys.is_directory(p)) {
            Array.iter(e => rm(Filename.concat(p, e)), Sys.readdir(p));
            Unix.rmdir(p);
          } else {
            Sys.remove(p);
          };
        };
      rm(dir);
    },
    f,
  );
};

let runFile = () =>
  Test.group("ignore: the file", () => {
    Test.run("the first load seeds the file with the defaults", () =>
      withTempConfig(() => {
        let path = Ignore.filePath();
        Test.assertFalse(Sys.file_exists(path), "no file before the first load");
        let (rules, err) = Ignore.load(~home);
        Test.assertTrue(err == None, "seeding reports no error");
        Test.assertTrue(Sys.file_exists(path), "the file now exists");
        Test.assertTrue(
          Ignore.matchesPath(rules, "/Volumes"),
          "the seeded rules are live",
        );
      })
    );

    Test.run("a second load reads the file rather than the built-ins", () =>
      withTempConfig(() => {
        let (_, _) = Ignore.load(~home);
        /* Replace the file wholesale. If load() still consulted the
           built-in defaults, /Volumes would come back. */
        let oc = open_out(Ignore.filePath());
        output_string(oc, "only_this\n");
        close_out(oc);
        let (rules, err) = Ignore.load(~home);
        Test.assertTrue(err == None, "no error");
        Test.assertTrue(Ignore.matchesName(rules, "only_this"), "the file's rule is live");
        Test.assertFalse(
          Ignore.matchesPath(rules, "/Volumes"),
          "the built-in defaults are NOT merged back in",
        );
      })
    );

    Test.run("addPath appends a tilde-shortened rule and re-reads", () =>
      withTempConfig(() => {
        let (rules, _) = Ignore.load(~home);
        let (rules, err) = Ignore.addPath(~home, rules, "/Users/tester/Downloads");
        Test.assertTrue(err == None, "no error");
        Test.assertTrue(
          Ignore.matchesPath(rules, "/Users/tester/Downloads"),
          "the new rule is live",
        );
        let ic = open_in(Ignore.filePath());
        let n = in_channel_length(ic);
        let body = really_input_string(ic, n);
        close_in(ic);
        Test.assertContains(body, "~/Downloads", "written tilde-shortened");
        Test.assertContains(body, "# added by hog", "under its own header");
        Test.assertContains(body, "/Volumes", "the existing content survives");
        Test.assertContains(body, "# hog ignore file", "comments survive verbatim");
      })
    );

    Test.run("addPath twice does not duplicate the rule", () =>
      withTempConfig(() => {
        let (rules, _) = Ignore.load(~home);
        let (rules, _) = Ignore.addPath(~home, rules, "/Users/tester/Downloads");
        let (rules, _) = Ignore.addPath(~home, rules, "/Users/tester/Downloads");
        let ic = open_in(Ignore.filePath());
        let body = really_input_string(ic, in_channel_length(ic));
        close_in(ic);
        Test.assertEqualInt(
          Test.countOccurrences(body, "~/Downloads"),
          1,
          "the rule appears exactly once",
        );
        Test.assertEqualInt(
          Test.countOccurrences(body, "# added by hog"),
          1,
          "the header appears exactly once",
        );
        Test.assertTrue(Ignore.matchesPath(rules, "/Users/tester/Downloads"), "still live");
      })
    );

    Test.run("removePath takes a rule back out", () =>
      withTempConfig(() => {
        let (rules, _) = Ignore.load(~home);
        let (rules, _) = Ignore.addPath(~home, rules, "/Users/tester/Downloads");
        let (rules, err) = Ignore.removePath(~home, rules, "/Users/tester/Downloads");
        Test.assertTrue(err == None, "no error");
        Test.assertFalse(
          Ignore.matchesPath(rules, "/Users/tester/Downloads"),
          "the rule is gone",
        );
        Test.assertTrue(
          Ignore.matchesPath(rules, "/Volumes"),
          "the other rules survive",
        );
      })
    );

    Test.run("the write is atomic and leaves no temp file behind", () =>
      withTempConfig(() => {
        let (rules, _) = Ignore.load(~home);
        let (_, _) = Ignore.addPath(~home, rules, "/Users/tester/Downloads");
        let dir = Filename.dirname(Ignore.filePath());
        let leftovers =
          Array.to_list(Sys.readdir(dir))
          |> List.filter(e => String.length(e) > 0 && e.[0] == '.');
        Test.assertEqualInt(
          List.length(leftovers),
          0,
          "no .ignore.tmp.<pid> survives the rename",
        );
      })
    );
  });
