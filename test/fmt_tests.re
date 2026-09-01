/*
 * Tests for lib/Fmt.re.
 *
 * The load-bearing assertion in here is the humanSize WIDTH contract: the
 * list row reserves Fmt.sizeWidth cells for the size column, so a change
 * that lets humanSize return a sixth cell would silently push every later
 * column one to the right on exactly the rows with the biggest numbers -
 * which are the rows this whole application exists to show.
 */

let run = () =>
  Test.group("fmt: sizes", () => {
    Test.run("bytes below 1000 are shown plainly", () => {
      Test.assertEqualStr(Fmt.humanSize(0), "0B", "zero");
      Test.assertEqualStr(Fmt.humanSize(1), "1B", "one");
      Test.assertEqualStr(Fmt.humanSize(999), "999B", "the last plain byte count");
    });

    Test.run("1000..1023 bytes still scale to K", () => {
      /* Not a rounding accident: the switch is at 1000 so the plain form
         never exceeds 4 cells, while the unit divisor stays 1024. */
      Test.assertEqualStr(Fmt.humanSize(1000), "1.0K", "1000 bytes");
      Test.assertEqualStr(Fmt.humanSize(1024), "1.0K", "one kibibyte");
    });

    Test.run("one decimal below ten, none above", () => {
      Test.assertEqualStr(Fmt.humanSize(1024 * 9 + 900), "9.9K", "9.9K");
      Test.assertEqualStr(Fmt.humanSize(1024 * 12), "12K", "12K");
      Test.assertEqualStr(Fmt.humanSize(1024 * 999), "999K", "999K");
    });

    Test.run("units climb through K M G T P", () => {
      Test.assertEqualStr(Fmt.humanSize(1024 * 1024), "1.0M", "mebibyte");
      Test.assertEqualStr(Fmt.humanSize(1024 * 1024 * 1024), "1.0G", "gibibyte");
      Test.assertEqualStr(Fmt.humanSize(1024 * 1024 * 1024 * 1024), "1.0T", "tebibyte");
    });

    Test.run("never wider than Fmt.sizeWidth cells", () => {
      /* Sweep the boundaries where a carry could add a cell, plus a decade
         of magnitudes, and assert the contract the row layout depends on. */
      let cases = ref([0, 1, 999, 1000, 1023, 1024]);
      let v = ref(1024);
      for (_ in 1 to 6) {
        v := v^ * 1024;
        cases := [v^ - 1, v^, v^ + 1, v^ * 1023, ...cases^];
      };
      List.iter(
        n => {
          let s = Fmt.humanSize(n);
          Test.assertTrue(
            Matcha.Element.visibleLength(s) <= Fmt.sizeWidth,
            "humanSize("
            ++ string_of_int(n)
            ++ ") = \""
            ++ s
            ++ "\" is "
            ++ string_of_int(Matcha.Element.visibleLength(s))
            ++ " cells, over the "
            ++ string_of_int(Fmt.sizeWidth)
            ++ "-cell contract",
          );
        },
        cases^,
      );
    });

    Test.run("counts get thousands separators", () => {
      Test.assertEqualStr(Fmt.humanCount(0), "0", "zero");
      Test.assertEqualStr(Fmt.humanCount(999), "999", "under a thousand");
      Test.assertEqualStr(Fmt.humanCount(1000), "1,000", "exactly a thousand");
      Test.assertEqualStr(Fmt.humanCount(412881), "412,881", "six figures");
      Test.assertEqualStr(Fmt.humanCount(1234567), "1,234,567", "seven figures");
    });
  });

let runBars = () =>
  Test.group("fmt: bars", () => {
    Test.run("a bar is always exactly `cells` cells wide", () =>
      List.iter(
        ((value, max)) =>
          List.iter(
            cells => {
              let b = Fmt.bar(~value, ~max, ~cells);
              Test.assertEqualInt(
                Matcha.Element.visibleLength(b),
                cells,
                "bar(~value="
                ++ string_of_int(value)
                ++ ", ~max="
                ++ string_of_int(max)
                ++ ", ~cells="
                ++ string_of_int(cells)
                ++ ") width",
              );
            },
            [1, 4, 8, 20],
          ),
        [(0, 100), (1, 100), (50, 100), (99, 100), (100, 100), (150, 100)],
      )
    );

    Test.run("zero and full are the extremes", () => {
      Test.assertEqualStr(Fmt.bar(~value=0, ~max=100, ~cells=4), "    ", "empty");
      Test.assertEqualStr(
        Fmt.bar(~value=100, ~max=100, ~cells=4),
        "\xe2\x96\x88\xe2\x96\x88\xe2\x96\x88\xe2\x96\x88",
        "full",
      );
    });

    Test.run("a value above max clamps rather than overflowing", () =>
      Test.assertEqualStr(
        Fmt.bar(~value=500, ~max=100, ~cells=3),
        Fmt.bar(~value=100, ~max=100, ~cells=3),
        "over-max clamps to full",
      )
    );

    Test.run("max <= 0 is an empty bar, not a division by zero", () => {
      Test.assertEqualStr(Fmt.bar(~value=5, ~max=0, ~cells=3), "   ", "max zero");
      Test.assertEqualStr(Fmt.bar(~value=5, ~max=-1, ~cells=3), "   ", "max negative");
    });

    Test.run("half of one cell is the four-eighths glyph", () =>
      Test.assertEqualStr(
        Fmt.bar(~value=1, ~max=2, ~cells=1),
        "\xe2\x96\x8c",
        "one half",
      )
    );
  });

let runPaths = () =>
  Test.group("fmt: paths", () => {
    let home = "/Users/x";

    Test.run("tildify rewrites only a real home prefix", () => {
      Test.assertEqualStr(Fmt.tildify(~home, "/Users/x"), "~", "home itself");
      Test.assertEqualStr(Fmt.tildify(~home, "/Users/x/Code"), "~/Code", "under home");
      /* The trap this guards: a plain prefix test would rewrite the home
         directory of a DIFFERENT user whose name starts with the same
         letters. The segment boundary is what stops it. */
      Test.assertEqualStr(
        Fmt.tildify(~home, "/Users/xavier/Code"),
        "/Users/xavier/Code",
        "a longer sibling name is not under home",
      );
      Test.assertEqualStr(Fmt.tildify(~home, "/etc"), "/etc", "elsewhere");
    });

    Test.run("relativeTo strips a base with its slash", () => {
      Test.assertEqualStr(
        Fmt.relativeTo(~base="/a/b", "/a/b/c/d"),
        "c/d",
        "under base",
      );
      Test.assertEqualStr(Fmt.relativeTo(~base="/a/b", "/a/b"), ".", "the base itself");
      Test.assertEqualStr(
        Fmt.relativeTo(~base="/a/b", "/a/bc/d"),
        "/a/bc/d",
        "a longer sibling is not under base",
      );
    });

    Test.run("breadcrumb elides from the front", () => {
      Test.assertEqualStr(
        Fmt.breadcrumb(~home, "/Users/x/Library/Caches"),
        "~ \xe2\x80\xba Library \xe2\x80\xba Caches",
        "short enough to show whole",
      );
      let long =
        Fmt.breadcrumb(~home, ~maxSegments=3, "/Users/x/a/b/c/d");
      Test.assertContains(long, "\xe2\x80\xa6", "elided crumb keeps an ellipsis");
      Test.assertContains(long, "d", "elided crumb keeps the last segment");
      Test.assertFalse(
        Test.contains(long, "Users"),
        "elided crumb drops the leading segments",
      );
    });
  });
