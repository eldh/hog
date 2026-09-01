/*
 * Fuzzy.re as pure data - no filesystem, no runtime, no frame.
 *
 * Two kinds of assertion here, and they are different in kind:
 *
 *   - POSITIONS are a contract with the renderer. They must be cell indices,
 *     ascending, in range, and they must be the cells a reader would point
 *     at. These are asserted exactly.
 *   - SCORES are only ever compared to each other. No test pins an absolute
 *     score: the constants are a design in RATIOS, and a test that froze
 *     their sum would break on every tuning without saying anything about
 *     whether the ordering got better or worse.
 */

let matchOf = (~query: string, ~text: string): option(Fuzzy.m) =>
  Fuzzy.match_(~query, ~text);

/* The score, or a large negative when there is no match at all - so an
   ordering assertion reads the same whether the loser matched weakly or not
   at all. */
let scoreOf = (~query: string, ~text: string): int =>
  switch (matchOf(~query, ~text)) {
  | Some(m) => m.Fuzzy.score
  | None => min_int / 2
  };

let positionsOf = (~query: string, ~text: string): list(int) =>
  switch (matchOf(~query, ~text)) {
  | Some(m) => m.Fuzzy.positions
  | None => []
  };

let showPositions = (l: list(int)): string =>
  "[" ++ String.concat(",", List.map(string_of_int, l)) ++ "]";

let assertPositions =
    (~query: string, ~text: string, expected: list(int), msg: string): unit => {
  let actual = positionsOf(~query, ~text);
  Test.assertEqualStr(
    showPositions(actual),
    showPositions(expected),
    msg ++ " (" ++ query ++ " in " ++ text ++ ")",
  );
};

let assertNoMatch = (~query: string, ~text: string, msg: string): unit =>
  switch (matchOf(~query, ~text)) {
  | None => ()
  | Some(m) =>
    Test.assertTrue(
      false,
      msg
      ++ ": expected no match for \""
      ++ query
      ++ "\" in \""
      ++ text
      ++ "\", got "
      ++ showPositions(m.Fuzzy.positions),
    )
  };

/* [winner] must outscore [loser] for the same query. */
let assertBeats =
    (~query: string, ~winner: string, ~loser: string, msg: string): unit => {
  let w = scoreOf(~query, ~text=winner);
  let l = scoreOf(~query, ~text=loser);
  Test.assertTrue(
    w > l,
    Printf.sprintf(
      "%s: \"%s\" should score \"%s\" (%d) above \"%s\" (%d)",
      msg,
      query,
      winner,
      w,
      loser,
      l,
    ),
  );
};

let run = () => {
  Test.group("fuzzy: feasibility", () => {
    Test.run("an empty query matches everything, with nothing highlighted", () =>
      switch (matchOf(~query="", ~text="Downloads/old-project")) {
      | None => Test.assertTrue(false, "the empty query must match")
      | Some(m) =>
        Test.assertEqualInt(m.Fuzzy.score, 0, "and score exactly zero");
        Test.assertEqualStr(
          showPositions(m.Fuzzy.positions),
          "[]",
          "with no cells picked out - an unfiltered list draws no amber",
        );
      }
    );

    Test.run("out of order is not a match", () =>
      assertNoMatch(
        ~query="ba",
        ~text="abc",
        "a subsequence is ORDERED - 'ba' is not one of 'abc'",
      )
    );

    Test.run("a query longer than the text is not a match", () =>
      assertNoMatch(~query="abcd", ~text="ab", "four cells cannot fit in two")
    );

    Test.run("a missing character is not a match", () =>
      assertNoMatch(
        ~query="node_modules",
        ~text="src/lib/Card.re",
        "no 'n' anywhere in the path",
      )
    );
  });

  Test.group("fuzzy: positions", () => {
    Test.run("an exact prefix picks out exactly its own cells", () =>
      assertPositions(
        ~query="abc",
        ~text="abcdef",
        [0, 1, 2],
        "the first three cells",
      )
    );

    Test.run("gaps are allowed, and only the matched cells come back", () =>
      assertPositions(~query="ac", ~text="abc", [0, 2], "a, skip b, c")
    );

    Test.run("the basename is preferred over an equal spelling in the path", () =>
      assertPositions(
        ~query="app",
        ~text="app/build/app",
        [10, 11, 12],
        "the basename occurrence, not the leading directory",
      )
    );

    Test.run("positions are CELL indices, not byte offsets", () =>
      assertPositions(
        ~query="t",
        ~text="\xc3\xa9/t.log",
        [2],
        "the 't' is cell 2 but byte 3 - the accented cell counts once",
      )
    );

    Test.run("positions are ascending and in range over a battery", () => {
      let cases = [
        ("cache", "node_modules/.cache/babel-loader"),
        ("lg", "build/logs/latest.log"),
        ("dl", "Downloads"),
        ("s", "s"),
        ("aaa", "abababa"),
      ];
      List.iter(
        ((query, text)) => {
          let cells = Matcha.TextWidth.toCells(text);
          let n = Array.length(cells);
          switch (matchOf(~query, ~text)) {
          | None =>
            Test.assertTrue(
              false,
              "expected a match for " ++ query ++ " in " ++ text,
            )
          | Some(m) =>
            Test.assertEqualInt(
              List.length(m.Fuzzy.positions),
              Array.length(Matcha.TextWidth.toCells(query)),
              "one position per query cell for " ++ query ++ "/" ++ text,
            );
            ignore(
              List.fold_left(
                (prev, p) => {
                  Test.assertTrue(
                    p > prev,
                    Printf.sprintf(
                      "%s in %s: %s is not ascending",
                      query,
                      text,
                      showPositions(m.Fuzzy.positions),
                    ),
                  );
                  Test.assertTrue(
                    p >= 0 && p < n,
                    Printf.sprintf(
                      "%s in %s: cell %d is out of range 0..%d",
                      query,
                      text,
                      p,
                      n - 1,
                    ),
                  );
                  p;
                },
                (-1),
                m.Fuzzy.positions,
              ),
            );
          };
        },
        cases,
      );
    });
  });

  Test.group("fuzzy: smart case", () => {
    Test.run("a lower-case query matches either case", () => {
      assertPositions(
        ~query="d",
        ~text="Downloads",
        [0],
        "lower-case query, upper-case text",
      );
      assertPositions(~query="d", ~text="downloads", [0], "and lower-case text");
    });

    Test.run("an upper-case cell in the query makes the whole query exact", () => {
      assertPositions(~query="D", ~text="Downloads", [0], "the capital is found");
      assertNoMatch(
        ~query="D",
        ~text="downloads",
        "but a lower-case text no longer answers a capital",
      );
      assertNoMatch(
        ~query="aD",
        ~text="ad",
        "one capital anywhere makes every cell case-sensitive",
      );
    });
  });

  Test.group("fuzzy: ranking", () => {
    Test.run("a basename match outranks a path match", () =>
      assertBeats(
        ~query="cache",
        ~winner="build/tmp/cache",
        ~loser="cache/tmp/build",
        "a directory NAMED cache beats a build dir INSIDE a cache directory",
      )
    );

    Test.run("a deep filename still beats a shallow directory", () =>
      assertBeats(
        ~query="cache",
        ~winner="projects/app/target/cache",
        ~loser="cachefiles/tmp",
        "the leading penalty is CAPPED - depth is a nudge, not a verdict",
      )
    );

    Test.run("the whole query inside the basename is worth more than a scatter", () =>
      assertBeats(
        ~query="log",
        ~winner="var/log",
        ~loser="var/l_o_g",
        "contiguous in the basename",
      )
    );

    Test.run("consecutive cells beat scattered ones", () =>
      assertBeats(
        ~query="tmp",
        ~winner="tmpxxxx",
        ~loser="txmxpxx",
        "no gaps to pay for",
      )
    );

    Test.run("a longer query on the same path always outscores a shorter one", () =>
      Test.assertTrue(
        scoreOf(~query="cach", ~text="node_modules/.cache")
        > scoreOf(~query="cac", ~text="node_modules/.cache"),
        "matchScore is paid per cell, so more matched cells is more score",
      )
    );
  });
};
