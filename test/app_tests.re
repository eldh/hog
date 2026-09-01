/*
 * The real component, driven headlessly through Runtime.startHeadless - the
 * same module bin/main.re starts, with two seams swapped:
 *
 *   - the scan handle is Scan.scanSync (already finished) or Scan.stepped
 *     (parked between directories), never Scan.start, so nothing here sleeps
 *     or races;
 *   - ~trash is a RECORDING STUB. Nothing in this suite ever moves a real
 *     file, and the stub is what proves "Esc cancelled" means the move was
 *     never attempted rather than merely not observed.
 *
 * SIZES ARE DELIBERATELY NEVER 80x24. That is simultaneously Matcha's
 * constraints default, its headless default and its non-TTY fallback, so a
 * layout assertion that happens to hold there can hold by coincidence rather
 * than because anything computed it. Every handle below asks for 120x40
 * (split), 96x30 (stacked), 99x36 / 97x36 (the breakpoint pair) or 60x20
 * (the bar dropped).
 *
 * `i` writes an ignore file, so every test that presses it runs inside
 * Ignore_tests.withTempConfig - otherwise the suite would scribble on the
 * developer's own ~/.config/hog/ignore.
 */
open Matcha;

/* ============================================================================
 * Fixtures
 * ============================================================================ */

/*
 * The canonical tree. Sizes are exact and distinct so that every ordering
 * assertion is arithmetic rather than a coin flip between two equal numbers.
 *
 * It is shaped to exercise the FRONTIER, not just a directory listing:
 *
 *   big/     700,000  splits (two big children)      -> a.bin, b.bin are rows
 *   wide/    155,000  emitted WHOLE (fan-out 10 > 8,
 *                     largest 20,000 nowhere near
 *                     half of 155,000)               -> one directory row
 *   medium/  120,000  splits (one dominant child)    -> c.bin is a row
 *   small/     5,000  splits                         -> d.bin is a row
 *   loose.txt  1,000                                 -> a row
 *
 * so the landing view is
 *
 *   big/a.bin  big/b.bin  wide/  medium/c.bin  small/d.bin  loose.txt
 *
 * which is the whole thesis of the tool on one screen: the biggest things
 * ANYWHERE, at any depth, and `wide/` named as a folder because naming any
 * one of its ten similar files would say nothing.
 */
let wideFiles: list((string, int)) =
  List.init(10, i =>
    (Printf.sprintf("wide/f%02d.bin", i + 1), 20_000 - i * 1_000)
  );

let tree: list((string, int)) =
  [("big/a.bin", 400_000), ("big/b.bin", 300_000)]
  @ wideFiles
  @ [("medium/c.bin", 120_000), ("small/d.bin", 5_000), ("loose.txt", 1_000)];

/* A directory with more children than any viewport under test, for the
   scrolling and "no blank rows" groups. */
let manyTree: list((string, int)) =
  List.init(60, i => (Printf.sprintf("many/e%02d.bin", i), 60_000 - i * 500));

/* ============================================================================
 * The harness
 * ============================================================================ */

type harness = {
  h: Runtime.headlessHandle,
  scan: Scan.handle,
  /* Absolute paths the stub was asked to trash, most recent first. */
  trashed: ref(list(string)),
  /* What the stub answers. Flip it to Error to exercise the failure box. */
  answer: ref(result(unit, string)),
  root: string,
};

let ignoreNothing = (~name as _: string, ~path as _: string) => false;

/*
 * minSize defaults to 1, NOT to Cli.defaultConfig's 10 MiB.
 *
 * That is not a fudge, it is the only way to test the ranking at all on a
 * fixture: Rank.frontier's threshold is max(minBytes, total/1000), so with
 * the shipped 10 MiB floor a tree of a few hundred kilobytes has NO
 * above-threshold children, the frontier is empty, and every landing
 * assertion would be asserting against "nothing here". The floor itself is
 * Rank's business and rank_tests.re owns it.
 */
let testCli: Cli.config = {...Cli.defaultConfig, minSize: 1};

let startApp =
    (
      ~width: int,
      ~height: int,
      ~cli: Cli.config=testCli,
      ~stepped: bool=false,
      ~rules: Ignore.rules=Ignore.empty,
      root: string,
      f: harness => unit,
    )
    : unit => {
  let trashed = ref([]);
  let answer = ref(Ok());
  let trash = p => {
    trashed := [p, ...trashed^];
    answer^;
  };
  let params = {...Rank.defaults, Rank.minBytes: cli.Cli.minSize};
  /* ~home=root, so the header breadcrumb reads "~" for the scan root and
     every displayed path is stable regardless of whose machine this runs
     on. */
  let make = stepped ? Scan.stepped : Scan.scanSync;
  let scan =
    make(~root, ~home=root, ~shouldIgnore=ignoreNothing, ~params, ());
  let h =
    Runtime.startHeadless(
      ~config={width, height},
      HogApp.app(~scan, ~config=cli, ~rules, ~trash),
    );
  Fun.protect(
    ~finally=() => {
      /* Let a stepped walk unwind rather than leaving a thread parked in its
         gate for the rest of the run. */
      Scan.cancel(scan);
      Scan.finish(scan);
      h.quit();
    },
    () => f({h, scan, trashed, answer, root}),
  );
};

/* Build the tree and start the app over it, in one call. */
let withApp =
    (
      ~width: int,
      ~height: int,
      ~cli: Cli.config=testCli,
      ~stepped: bool=false,
      ~spec: list((string, int))=tree,
      f: harness => unit,
    )
    : unit =>
  Fixture.withTree(spec, root =>
    startApp(~width, ~height, ~cli, ~stepped, root, f)
  );

/* ============================================================================
 * Reading a frame
 * ============================================================================ */

let key = (t: harness, k: Key.t) => Input.pressKey(t.h, k);
let press = (t: harness, c: char) => t.h.sendKey(Key.Char(c), Key.noModifiers);
let ctrl = (t: harness, c: char) =>
  t.h.sendKey(Key.Char(c), {...Key.noModifiers, Key.ctrl: true});

let times = (n: int, f: unit => unit) =>
  for (_ in 1 to n) {
    f();
  };

/* The pane rule, which is also the only character that can separate the list
   column from the details column. */
let rule = "\xe2\x94\x82";

/* Everything on a frame line before the first pane rule. In the stacked
   layout there is no rule and this is the whole line. */
let leftOf = (s: string): string => {
  let rlen = String.length(rule);
  let slen = String.length(s);
  let rec go = i =>
    if (i > slen - rlen) {
      s;
    } else if (String.sub(s, i, rlen) == rule) {
      String.sub(s, 0, i);
    } else {
      go(i + 1);
    };
  go(0);
};

/* Everything AFTER the first pane rule: the details column, split layout. */
let rightOf = (s: string): string => {
  let rlen = String.length(rule);
  let slen = String.length(s);
  let rec go = i =>
    if (i > slen - rlen) {
      "";
    } else if (String.sub(s, i, rlen) == rule) {
      String.sub(s, i + rlen, slen - i - rlen);
    } else {
      go(i + 1);
    };
  go(0);
};

let lines = (t: harness): array(string) => t.h.getLines(true);
let rawLines = (t: harness): array(string) => t.h.getLines(false);

let headerLine = (t: harness): string => String.trim(lines(t)[0]);
let statusLine = (t: harness): string => String.trim(lines(t)[1]);
let hintLine = (t: harness): string => {
  let ls = lines(t);
  String.trim(ls[Array.length(ls) - 1]);
};

/*
 * The frame row the list starts on.
 *
 * Rows 0 and 1 are the header and the status, ALWAYS, in every scan state -
 * that constant geometry is the reason this is a number and not a function
 * of the phase. The filter input, when it is on screen, takes the row under
 * them and pushes the list down by one.
 *
 * ~filter is passed explicitly rather than sniffed off the frame. A detector
 * would have to tell "the filter row showing the query `1`" from "a list row
 * whose size column starts with 1", and a test helper that can be wrong
 * about where it is looking is worse than one the test has to be honest
 * with.
 */
let listTop = (~filter: bool): int => filter ? 3 : 2;

/* Row `i` of the list as the user reads it: stripped, and left of the pane
   rule so the details column cannot leak into a list assertion. */
let listRow = (t: harness, ~filter: bool=false, i: int): string => {
  let ls = lines(t);
  let y = listTop(~filter) + i;
  y < Array.length(ls) ? String.trim(leftOf(ls[y])) : "";
};

/* The list, top to bottom, as far as `count` rows. */
let listRows = (t: harness, ~filter: bool=false, count: int): list(string) =>
  List.init(count, i => listRow(t, ~filter, i));

/* The filter input row, which is only on screen when it is on screen. */
let filterRow = (t: harness): string => String.trim(leftOf(lines(t)[2]));

/* The selected row, read the way a user sees it: the one carrying the caret.
   Returns "" when nothing is selected. */
let selectedRow = (~filter: bool=false, t: harness): string => {
  let n = Array.length(lines(t));
  let found = ref("");
  for (i in 0 to n - 1) {
    let r = listRow(t, ~filter, i);
    if (found^ == "" && String.length(r) > 0 && r.[0] == '>') {
      found := r;
    };
  };
  found^;
};

/* The details column (split layout) or strip (stacked), joined. */
let detailsText = (t: harness, ~wide: bool): string => {
  let ls = lines(t);
  if (wide) {
    String.concat("\n", Array.to_list(Array.map(rightOf, ls)));
  } else {
    let n = Array.length(ls);
    String.concat("\n", [ls[n - 4], ls[n - 3], ls[n - 2]]);
  };
};

let anyLine = (t: harness, needle: string): bool =>
  Array.exists(l => Test.contains(l, needle), lines(t));

/* ============================================================================
 * Tests
 * ============================================================================ */

/* The whole frame as one string, for the two "nothing moved" assertions. */
let frame = (t: harness): string =>
  String.concat("\n", Array.to_list(lines(t)));

/*
 * The file count out of the status row ("scanning… 685K · 3 files · …").
 *
 * Read off the frame rather than out of Scan, because the claim under test is
 * that the ROW moves while the walk runs - a progress line that has stopped
 * being repainted is exactly the bug, and a snapshot read straight from the
 * scan handle could not see it. -1 when the row does not carry a count.
 */
let filesInStatus = (t: harness): int => {
  let s = statusLine(t);
  let needle = " files";
  let slen = String.length(s);
  let nlen = String.length(needle);
  let rec find = i =>
    if (i > slen - nlen) {
      (-1);
    } else if (String.sub(s, i, nlen) == needle) {
      i;
    } else {
      find(i + 1);
    };
  let at = find(0);
  if (at < 0) {
    (-1);
  } else {
    let isDigitish = c => c >= '0' && c <= '9' || c == ',';
    let start = ref(at);
    while (start^ > 0 && isDigitish(s.[start^ - 1])) {
      decr(start);
    };
    let digits =
      String.concat(
        "",
        List.filter_map(
          c => c == ',' ? None : Some(String.make(1, c)),
          List.init(at - start^, i => s.[start^ + i]),
        ),
      );
    switch (int_of_string_opt(digits)) {
    | Some(n) => n
    | None => (-1)
    };
  };
};

let run = () => {
  Test.group("app: the first frame", () => {
    Test.run("header names the scan root and its apparent total", () =>
      withApp(~width=120, ~height=40, t => {
        Test.assertContains(headerLine(t), "~", "breadcrumb shows the root");
        Test.assertContains(
          headerLine(t),
          "apparent",
          "the header labels the size as apparent",
        );
        Test.assertContains(
          headerLine(t),
          "biggest anywhere",
          "the landing view says what it is showing",
        );
      })
    );

    Test.run("status row reports a finished scan", () =>
      withApp(~width=120, ~height=40, t => {
        Test.assertContains(statusLine(t), "scanned", "phase is reported");
        Test.assertContains(statusLine(t), "files", "file count is reported");
      })
    );

    Test.run("hint row names the keys", () =>
      withApp(~width=120, ~height=40, t => {
        Test.assertContains(hintLine(t), "filter", "/ filter is offered");
        Test.assertContains(hintLine(t), "trash", "d trash is offered");
        Test.assertContains(hintLine(t), "quit", "q quit is offered");
      })
    );

    Test.run("the app is running before anything is pressed", () =>
      withApp(~width=120, ~height=40, t =>
        Test.assertTrue(t.h.isRunning(), "still running")
      )
    );

    /* The one place the SHIPPED config is used rather than testCli. A tree of
       a few hundred kilobytes ranks to nothing against the default 10 MiB
       floor, and the whole point of this case is that the empty state names
       the floor instead of implying the disk is empty. */
    Test.run("an empty ranking names the floor that emptied it", () =>
      withApp(~width=120, ~height=40, ~cli=Cli.defaultConfig, t => {
        Test.assertContains(
          listRow(t, 0),
          "nothing above the",
          "the empty state says why it is empty",
        );
        Test.assertContains(
          listRow(t, 0),
          /* Fmt.humanSize drops the decimal at 10 and above, so the shipped
             10 MiB floor reads "10M", not "10.0M". */
          "10M",
          "naming the floor that did it",
        );
        Test.assertContains(
          listRow(t, 0),
          "--min-size",
          "and the flag that moves it",
        );
      })
    );
  });

  Test.group("app: the landing ranking", () => {
    Test.run("the biggest things at any depth are the rows", () =>
      withApp(~width=120, ~height=40, t => {
        let rows = listRows(t, 6);
        let joined = String.concat(" | ", rows);
        Test.assertContains(joined, "big/a.bin", "the biggest file is a row");
        Test.assertContains(joined, "big/b.bin", "the second is a row");
        Test.assertContains(
          joined,
          "wide/",
          "a wide folder is named as a folder, not by its files",
        );
        Test.assertContains(joined, "medium/c.bin", "a dominant child is a row");
      })
    );

    Test.run("no ancestor of an emitted row is also a row", () =>
      withApp(~width=120, ~height=40, t => {
        let joined = String.concat(" | ", listRows(t, 8));
        /* big/ splits, so the FOLDER big/ must not appear on its own. */
        Test.assertFalse(
          Test.contains(joined, "big/ "),
          "big/ is not emitted alongside its children",
        );
      })
    );

    Test.run("rows are size-ordered, largest first", () =>
      withApp(~width=120, ~height=40, t => {
        let r0 = listRow(t, 0);
        let r1 = listRow(t, 1);
        Test.assertContains(r0, "big/a.bin", "row 0 is the 400K file");
        Test.assertContains(r1, "big/b.bin", "row 1 is the 300K file");
      })
    );

    Test.run("row 0 is selected on the first frame", () =>
      withApp(~width=120, ~height=40, t =>
        Test.assertContains(
          selectedRow(t),
          "big/a.bin",
          "the caret starts on the biggest entry",
        )
      )
    );
  });

  /* ==========================================================================
   * Navigation
   * ==========================================================================
   *
   * The landing rows are
   *
   *   0 big/a.bin   1 big/b.bin   2 wide/   3 medium/c.bin   4 small/d.bin
   *   5 loose.txt
   *
   * so `j` twice puts the caret on the one DIRECTORY row, which is the only
   * row Enter is allowed to do anything with.
   */
  Test.group("app: navigation", () => {
    Test.run("enter on a directory row descends into it", () =>
      withApp(~width=120, ~height=40, t => {
        press(t, 'j');
        press(t, 'j');
        Test.assertContains(selectedRow(t), "wide/", "the caret is on wide/");
        key(t, Key.Enter);
        Test.assertContains(
          headerLine(t),
          "wide",
          "the breadcrumb names the directory we descended into",
        );
        Test.assertContains(
          listRow(t, 0),
          "f01.bin",
          "row 0 is wide/'s biggest child",
        );
        Test.assertContains(listRow(t, 9), "f10.bin", "row 9 is its smallest");
        Test.assertEqualStr(
          listRow(t, 10),
          "",
          "and there is no eleventh row - a drill lists the children, all of them and only them",
        );
      })
    );

    Test.run("enter on a file row does nothing", () =>
      withApp(~width=120, ~height=40, t => {
        /* Row 0 is big/a.bin, a file. */
        let before = frame(t);
        key(t, Key.Enter);
        Test.assertEqualStr(
          frame(t),
          before,
          "a file cannot be opened, so the frame is unchanged",
        );
      })
    );

    Test.run("backspace ascends, and lands the caret on what we came out of", () =>
      withApp(~width=120, ~height=40, t => {
        press(t, 'j');
        press(t, 'j');
        key(t, Key.Enter);
        Test.assertContains(headerLine(t), "wide", "we are inside wide/");
        key(t, Key.Backspace);
        Test.assertContains(
          selectedRow(t),
          "wide/",
          "the caret is back on the directory we just left",
        );
        Test.assertContains(
          listRow(t, 0),
          "big/",
          "and the list is the parent's children again",
        );
      })
    );

    Test.run("backspace at the landing view is a no-op", () =>
      withApp(~width=120, ~height=40, t => {
        let before = frame(t);
        key(t, Key.Backspace);
        Test.assertEqualStr(
          frame(t),
          before,
          "there is nowhere up from the landing view, and nothing surprising happens",
        );
      })
    );

    Test.run("the breadcrumb names the current directory, not the scan root", () =>
      withApp(~width=120, ~height=40, t => {
        Test.assertContains(
          headerLine(t),
          "biggest anywhere",
          "the landing view is tagged as the ranked view",
        );
        press(t, 'j');
        press(t, 'j');
        key(t, Key.Enter);
        Test.assertContains(
          headerLine(t),
          "\xe2\x80\xba wide",
          "the breadcrumb has descended a segment",
        );
        Test.assertNotContains(
          headerLine(t),
          "biggest anywhere",
          "a drill is a listing, not a ranking, and says so",
        );
      })
    );
  });

  /* ==========================================================================
   * Selection and scrolling
   * ==========================================================================
   *
   * manyTree at 120x24: `many/` is one landing row (ten-plus similar children,
   * none dominant), Enter drills into its 60 files, and the list column shows
   * 21 of them - so the window has somewhere to go.
   */
  Test.group("app: selection and scrolling", () => {
    Test.run("j and k move the caret one row at a time", () =>
      withApp(~width=120, ~height=24, ~spec=manyTree, t => {
        key(t, Key.Enter);
        Test.assertContains(selectedRow(t), "e00.bin", "starts on the first row");
        press(t, 'j');
        Test.assertContains(selectedRow(t), "e01.bin", "j moves down one");
        press(t, 'j');
        Test.assertContains(selectedRow(t), "e02.bin", "and again");
        press(t, 'k');
        Test.assertContains(selectedRow(t), "e01.bin", "k moves back up one");
      })
    );

    Test.run("the selection stops at both ends rather than wrapping", () =>
      withApp(~width=120, ~height=24, ~spec=manyTree, t => {
        key(t, Key.Enter);
        times(3, () => press(t, 'k'));
        Test.assertContains(
          selectedRow(t),
          "e00.bin",
          "k at the top stays on the first row",
        );
        press(t, 'G');
        times(3, () => press(t, 'j'));
        Test.assertContains(
          selectedRow(t),
          "e59.bin",
          "j at the bottom stays on the last row",
        );
      })
    );

    Test.run("G/End jump to the last row and g/Home to the first", () =>
      withApp(~width=120, ~height=24, ~spec=manyTree, t => {
        key(t, Key.Enter);
        press(t, 'G');
        Test.assertContains(selectedRow(t), "e59.bin", "G is the last row");
        press(t, 'g');
        Test.assertContains(selectedRow(t), "e00.bin", "g is the first");
        key(t, Key.End);
        Test.assertContains(selectedRow(t), "e59.bin", "End is the last row");
        key(t, Key.Home);
        Test.assertContains(selectedRow(t), "e00.bin", "Home is the first");
      })
    );

    Test.run("a wheel notch moves the window but never the selection", () =>
      withApp(~width=120, ~height=24, ~spec=manyTree, t => {
        key(t, Key.Enter);
        /* Ten rows down, well clear of the two-row margin, so the selected row
           is still on screen after the notch and selectedRow can see it. */
        times(10, () => press(t, 'j'));
        Test.assertContains(listRow(t, 0), "e00.bin", "the window starts at the top");
        Test.assertContains(selectedRow(t), "e10.bin", "the caret is ten rows down");
        Input.wheelAt(t.h, ~x=5, ~y=10, ~up=false);
        Test.assertContains(
          listRow(t, 0),
          "e03.bin",
          "the notch scrolled the window",
        );
        Test.assertContains(
          selectedRow(t),
          "e10.bin",
          "and left the selection exactly where it was - scrolling must not retarget the d key",
        );
      })
    );

    Test.run("a click selects the row under the pointer and nothing else", () =>
      withApp(~width=120, ~height=24, ~spec=manyTree, t => {
        key(t, Key.Enter);
        /* Frame row 7 is list row 5 (the header and status own rows 0 and 1). */
        Input.clickAt(t.h, ~x=5, ~y=7);
        Test.assertContains(selectedRow(t), "e05.bin", "the clicked row is selected");
        Test.assertContains(
          listRow(t, 0),
          "e00.bin",
          "and the window did not move under it",
        );
        Test.assertContains(
          detailsText(t, ~wide=true),
          "e05.bin",
          "the details followed the click",
        );
      })
    );
  });

  /* ==========================================================================
   * No blank rows
   * ==========================================================================
   *
   * The offset the runtime paints and the window Rows.ensureRange bakes are
   * computed in two places. When they drift the symptom is a BLANK row, not a
   * stale colour, so the assertion is that every visible list line carries an
   * entry - checked at the top, mid-list, and at the very end.
   */
  Test.group("app: no blank rows", () => {
    Test.run("every visible list row carries an entry at every offset", () => {
      let allFilled = (t, label) =>
        /* 21 list rows at 120x24, and manyTree has 60 entries, so at no
           offset can the viewport legitimately run out. */
        for (i in 0 to 20) {
          Test.assertContains(
            listRow(t, i),
            ".bin",
            Printf.sprintf("%s: list row %d is a real entry, not a blank", label, i),
          );
        };
      withApp(~width=120, ~height=24, ~spec=manyTree, t => {
        key(t, Key.Enter);
        allFilled(t, "at the top");
        times(3, () => Input.wheelAt(t.h, ~x=5, ~y=10, ~up=false));
        allFilled(t, "after three wheel notches");
        key(t, Key.Page_down);
        allFilled(t, "after a page down");
        press(t, 'G');
        allFilled(t, "at the end");
      });
    });
  });

  /* ==========================================================================
   * Responsive layout
   * ==========================================================================
   *
   * 98 columns is the split threshold, derived in HogApp from the row's own
   * chrome, so 99 and 97 are the pair that pins it. None of these sizes is
   * 80x24, which is Matcha's constraints default AND its headless default AND
   * its non-TTY fallback all at once.
   */
  Test.group("app: responsive layout", () => {
    Test.run("at 99 columns the details are a column beside the list", () =>
      withApp(~width=99, ~height=36, t => {
        Test.assertContains(
          lines(t)[2],
          rule,
          "a pane rule separates the list from the details",
        );
        Test.assertContains(
          rightOf(lines(t)[2]),
          "a.bin",
          "and the selected entry is named on the far side of it",
        );
      })
    );

    Test.run("at 97 columns the details are a strip under the list", () =>
      withApp(~width=97, ~height=36, t => {
        Test.assertNotContains(
          lines(t)[2],
          rule,
          "one column below the breakpoint there is no pane rule",
        );
        Test.assertContains(
          detailsText(t, ~wide=false),
          "~/big/a.bin",
          "the details moved to the three-row strip above the hint",
        );
        Test.assertContains(
          listRow(t, 0),
          "big/a.bin",
          "and the list got the whole width",
        );
      })
    );

    Test.run("below 56 columns of list width the bar is dropped", () =>
      /* 56 columns of frame is 55 of list, one under Rows.barMinWidth. */
      withApp(~width=56, ~height=20, t => {
        Test.assertNotContains(
          listRow(t, 0),
          "\xe2\x96\x88",
          "the bar is gone rather than crushing the path",
        );
        Test.assertContains(
          listRow(t, 0),
          "big/a.bin",
          "and the path is still readable",
        );
        Test.assertContains(listRow(t, 5), "loose.txt", "every row still renders");
        Test.assertContains(hintLine(t), "move", "the hint row survives too");
      })
    );
  });

  /* ==========================================================================
   * The details pane
   * ========================================================================== */
  Test.group("app: the details pane", () => {
    Test.run("the details follow the selection", () =>
      withApp(~width=120, ~height=40, t => {
        Test.assertContains(
          detailsText(t, ~wide=true),
          "~/big/a.bin",
          "the first frame describes the first row",
        );
        press(t, 'j');
        Test.assertContains(
          detailsText(t, ~wide=true),
          "~/big/b.bin",
          "and the second row after one j",
        );
        Test.assertNotContains(
          detailsText(t, ~wide=true),
          "~/big/a.bin",
          "the pane describes one entry, not a history of them",
        );
      })
    );

    Test.run("a directory shows items and no access time; a file shows one", () =>
      withApp(~width=120, ~height=40, t => {
        let fileDetails = detailsText(t, ~wide=true);
        Test.assertContains(fileDetails, "accessed", "a file reports its atime");
        Test.assertNotContains(fileDetails, "items", "a file has no item count");
        press(t, 'j');
        press(t, 'j');
        let dirDetails = detailsText(t, ~wide=true);
        Test.assertContains(dirDetails, "items", "a directory reports its item count");
        /* Our own readdir counts as an access, so a directory's atime would
           read "just now" on every second run and mean nothing. */
        Test.assertNotContains(
          dirDetails,
          "accessed",
          "and deliberately does not report an access time",
        );
      })
    );

    Test.run("the ctime field is labelled changed", () =>
      withApp(~width=120, ~height=40, t =>
        /* st_ctime is inode-change time. Calling it "created" would be a lie
           the user would act on - there IS a separate created line, from
           stat -f %B, and this is the one beside it. */
        Test.assertContains(
          detailsText(t, ~wide=true),
          "changed",
          "the inode-change time is named for what it is",
        )
      )
    );
  });

  /* ==========================================================================
   * The filter
   * ==========================================================================
   *
   * Everything here is really one property: while the filter is open the
   * keyboard belongs to the text field, and the chords that would otherwise
   * navigate are claimed rather than shared.
   */
  Test.group("app: the filter", () => {
    Test.run("/ opens the input and pushes the list down a row", () =>
      withApp(~width=120, ~height=40, t => {
        press(t, '/');
        Test.assertContains(
          filterRow(t),
          "filter",
          "the input shows its placeholder",
        );
        Test.assertContains(
          listRow(t, ~filter=true, 0),
          "big/a.bin",
          "and the list starts one row lower than before",
        );
      })
    );

    Test.run("typing narrows the list", () =>
      withApp(~width=120, ~height=40, t => {
        press(t, '/');
        press(t, 'w');
        Test.assertContains(filterRow(t), "w", "the query is echoed");
        Test.assertContains(
          listRow(t, ~filter=true, 0),
          "wide/",
          "the one matching entry is the only row",
        );
        Test.assertEqualStr(
          listRow(t, ~filter=true, 1),
          "",
          "everything else is gone",
        );
      })
    );

    Test.run("q types a q rather than quitting", () =>
      withApp(~width=120, ~height=40, t => {
        press(t, '/');
        press(t, 'q');
        Test.assertTrue(
          t.h.isRunning(),
          "the app is still running - q reached the text field, not the quit binding",
        );
        Test.assertContains(filterRow(t), "q", "and the q is in the query");
      })
    );

    Test.run("backspace edits the query rather than ascending", () =>
      withApp(~width=120, ~height=40, t => {
        press(t, 'j');
        press(t, 'j');
        key(t, Key.Enter);
        let breadcrumb = headerLine(t);
        press(t, '/');
        press(t, 'f');
        press(t, '0');
        press(t, '1');
        Test.assertContains(filterRow(t), "f01", "three characters typed");
        key(t, Key.Backspace);
        Test.assertContains(filterRow(t), "f0", "the query got one shorter");
        Test.assertNotContains(filterRow(t), "f01", "the last character is gone");
        Test.assertEqualStr(
          headerLine(t),
          breadcrumb,
          "and we did not ascend out of the directory",
        );
      })
    );

    Test.run("enter keeps the query and hands the keyboard back", () =>
      withApp(~width=120, ~height=40, t => {
        press(t, '/');
        press(t, 'w');
        key(t, Key.Enter);
        Test.assertContains(filterRow(t), "w", "the query survived");
        press(t, 'k');
        Test.assertContains(filterRow(t), "w", "and k did not reach the text field");
        Test.assertNotContains(filterRow(t), "wk", "no second character was typed");
        Test.assertContains(
          listRow(t, ~filter=true, 0),
          "wide/",
          "the list is still narrowed",
        );
      })
    );

    Test.run("esc clears the query and restores the selection it found", () =>
      withApp(~width=120, ~height=40, t => {
        press(t, 'j');
        press(t, 'j');
        Test.assertContains(selectedRow(t), "wide/", "the caret is on wide/");
        press(t, '/');
        press(t, 'l');
        Test.assertNotContains(
          selectedRow(~filter=true, t),
          "wide/",
          "the query moved the caret off it",
        );
        key(t, Key.Escape);
        Test.assertContains(
          listRow(t, 0),
          "big/a.bin",
          "the filter row is gone and the list is whole again",
        );
        Test.assertContains(
          selectedRow(t),
          "wide/",
          "and the caret is back where the filter found it",
        );
      })
    );
  });

  /* ==========================================================================
   * Ignore
   * ==========================================================================
   *
   * `i` WRITES A FILE, so both of these run inside a temp XDG_CONFIG_HOME.
   * Without it the suite edits the developer's own ~/.config/hog/ignore.
   */
  Test.group("app: ignore", () => {
    Test.run("i drops the selected entry out of the list", () =>
      Ignore_tests.withTempConfig(() =>
        withApp(~width=120, ~height=40, t => {
          Test.assertContains(listRow(t, 0), "big/a.bin", "before: the top row");
          press(t, 'i');
          Test.assertContains(
            listRow(t, 0),
            "big/b.bin",
            "after: the ignored entry is gone and the next one moved up",
          );
          Test.assertNotContains(
            String.concat(" | ", listRows(t, 6)),
            "big/a.bin",
            "and it is nowhere else in the list either",
          );
        })
      )
    );

    Test.run("I reveals what was ignored", () =>
      Ignore_tests.withTempConfig(() =>
        withApp(~width=120, ~height=40, t => {
          press(t, 'i');
          Test.assertContains(listRow(t, 0), "big/b.bin", "it is hidden");
          press(t, 'I');
          Test.assertContains(
            listRow(t, 0),
            "big/a.bin",
            "and I brings it back at its own place in the ranking",
          );
        })
      )
    );
  });

  /* ==========================================================================
   * The trash dialog
   * ==========================================================================
   *
   * ~trash is a recording stub. "Esc cancelled" therefore means the move was
   * never ATTEMPTED, which is a different and much stronger claim than "the
   * row is still there".
   */
  Test.group("app: the trash dialog", () => {
    Test.run("d names the absolute path and the size of what it would move", () =>
      withApp(~width=120, ~height=40, t => {
        press(t, 'd');
        Test.assertTrue(
          anyLine(t, "Move this file to the Trash?"),
          "the dialog says what it is about to do",
        );
        /* The absolute path is wrapped over up to two lines inside a box 56%
           of 120 columns wide, so the tail is what a single line can carry. */
        Test.assertTrue(
          anyLine(t, "big/a.bin"),
          "and names the entry it would move",
        );
        Test.assertTrue(anyLine(t, "391K"), "together with its size");
        Test.assertContains(
          hintLine(t),
          "y move to trash",
          "the hint row belongs to the dialog while it is open",
        );
      })
    );

    Test.run("esc cancels without ever calling trash", () =>
      withApp(~width=120, ~height=40, t => {
        press(t, 'd');
        key(t, Key.Escape);
        Test.assertTrue(
          t.trashed^ == [],
          "the stub was never called - the move was not attempted, not merely undone",
        );
        Test.assertFalse(
          anyLine(t, "Move this file to the Trash?"),
          "and the dialog is closed",
        );
        Test.assertContains(listRow(t, 0), "big/a.bin", "the row is still there");
      })
    );

    Test.run("y moves exactly the selected entry and drops its row", () =>
      withApp(~width=120, ~height=40, t => {
        press(t, 'd');
        press(t, 'y');
        Test.assertTrue(
          t.trashed^ == [Filename.concat(t.root, "big/a.bin")],
          "exactly one absolute path was handed to trash",
        );
        Test.assertContains(
          listRow(t, 0),
          "big/b.bin",
          "and the trashed row left the list on the keystroke",
        );
        Test.assertNotContains(
          String.concat(" | ", listRows(t, 6)),
          "big/a.bin",
          "it is not further down the list either",
        );
      })
    );

    Test.run("a failed move keeps the dialog open and shows why", () =>
      withApp(~width=120, ~height=40, t => {
        t.answer := Error("nope");
        press(t, 'd');
        press(t, 'y');
        Test.assertTrue(
          anyLine(t, "Could not move it to the Trash"),
          "the dialog stays up and reports the failure",
        );
        Test.assertTrue(anyLine(t, "nope"), "quoting the message it was given");
        Test.assertContains(
          listRow(t, 0),
          "big/a.bin",
          "and the row is still in the list, because nothing moved",
        );
      })
    );

    Test.run("ctrl+c quits while the dialog is open", () =>
      withApp(~width=120, ~height=40, t => {
        press(t, 'd');
        Test.assertTrue(t.h.isRunning(), "running, with the dialog up");
        ctrl(t, 'c');
        /* Raw mode disables ISIG, so Ctrl+C is an ordinary keypress. It is
           bound with useKeyDown and not useInput precisely so the open layer
           cannot suppress it - a useInput binding would make the app
           unquittable behind this dialog. */
        Test.assertFalse(
          t.h.isRunning(),
          "ctrl+c is not suppressed by the modal layer",
        );
      })
    );
  });

  /* ==========================================================================
   * A live scan
   * ==========================================================================
   *
   * ~stepped parks the walk between directories. Scan.step lets it finish one
   * more, and advanceTime drives the useInterval poll on Matcha's headless
   * VIRTUAL clock - nothing here sleeps.
   */
  Test.group("app: a live scan", () => {
    Test.run("the progress row moves as the walk proceeds", () =>
      withApp(~width=120, ~height=40, ~stepped=true, t => {
        let before = statusLine(t);
        let filesBefore = filesInStatus(t);
        Test.assertContains(before, "scanning", "the walk is running");
        Test.assertTrue(filesBefore >= 0, "the row carries a file count");
        Scan.step(t.scan);
        t.h.advanceTime(100);
        let after = statusLine(t);
        Test.assertFalse(after == before, "the row was repainted");
        Test.assertTrue(
          filesInStatus(t) > filesBefore,
          Printf.sprintf(
            "the file count grew (%d then %d)",
            filesBefore,
            filesInStatus(t),
          ),
        );
      })
    );

    Test.run("the list still takes keys while the walk runs", () =>
      withApp(~width=120, ~height=40, ~stepped=true, t => {
        times(3, () => {
          Scan.step(t.scan);
          t.h.advanceTime(100);
        });
        Test.assertContains(statusLine(t), "scanning", "the walk is still parked mid-scan");
        let first = selectedRow(t);
        Test.assertFalse(first == "", "something is selected");
        Test.assertFalse(listRow(t, 1) == "", "and there is a second row to move to");
        press(t, 'j');
        Test.assertFalse(
          selectedRow(t) == first,
          "j moved the caret, so the UI is responsive while the walk runs",
        );
      })
    );

    Test.run("the phase word changes from scanning to scanned", () =>
      withApp(~width=120, ~height=40, ~stepped=true, t => {
        Test.assertContains(
          statusLine(t),
          "scanning",
          "parked mid-walk, the row says so",
        );
        Scan.finish(t.scan);
        t.h.advanceTime(100);
        Test.assertContains(statusLine(t), "scanned", "and reports a finished walk");
        Test.assertNotContains(
          statusLine(t),
          "scanning",
          "the progressive wording is gone",
        );
      })
    );
  });

  /* ==========================================================================
   * Unreadable directories
   * ==========================================================================
   *
   * macOS returns EPERM for ~/Library/Mail and friends even to the owning
   * user without Full Disk Access, silently under-reporting by tens of
   * gigabytes. A tool that does that without saying so is worse than no tool.
   *
   * The chmod has to be in force while the WALK runs, so it wraps startApp
   * and not just the assertion.
   */
  Test.group("app: unreadable directories", () => {
    Test.run("the status row warns, and names the fix", () =>
      Fixture.withTree(
        [("locked/x.bin", 40_000), ("plain.txt", 1_000)], root =>
        Fixture.withUnreadable([Filename.concat(root, "locked")], () =>
          /* 120 columns so the warning is not truncated away before the words
             under test. */
          startApp(~width=120, ~height=40, root, t => {
            Test.assertContains(
              statusLine(t),
              "unreadable",
              "the count of directories the walk could not open is reported",
            );
            Test.assertContains(
              statusLine(t),
              "Full Disk Access",
              "with the one thing the user can actually do about it",
            );
          })
        )
      )
    );
  });
};
