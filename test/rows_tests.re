/*
 * Tests for lib/Rows.re - the list row builder.
 *
 * No filesystem: every arena in here is built by hand, the same way
 * store_tests.re does it, because Rows only ever reads the arena.
 *
 * THE ASSERTION THAT MATTERS MOST is that every row measures EXACTLY the
 * width it was asked for, at several widths including narrow ones. A row
 * that comes out one cell wide or one cell narrow does not look broken on
 * its own - it looks like the column below it is slightly ragged, and only
 * on the rows with the longest names or a double-width character in them.
 * That one equality catches every off-by-one in the eliding, the padding
 * and the bar at once, which is why it is applied across the whole entry
 * set rather than to one hand-picked row.
 *
 * The second theme is the ScrollView `~rows` contract: a row must open every
 * style it uses and close it, because the runtime starts painting at row N
 * without ever reading the rows above it. A violation is invisible until
 * someone scrolls, so it is asserted directly.
 */
open Matcha;

let home = "/Users/tester";
let root = home ++ "/scan";

let addFile = (t, ~parent, ~name, ~size, ~flags=0, ()) => {
  let id = Store.alloc(t, ~name, ~parent, ~kind=Store.File, ~size, ~flags, ());
  Store.addUp(t, id, ~bytes=size, ~items=1);
  id;
};

let addDir = (t, ~parent, ~name, ~flags=0, ()) => {
  let id = Store.alloc(t, ~name, ~parent, ~kind=Store.Dir, ~flags, ());
  Store.addUp(t, id, ~bytes=0, ~items=1);
  id;
};

/* A tree with one of everything the row builder branches on: a deep
 * directory, a plain file, a sparse suspect, an ignored directory, an
 * off-volume directory, and two names that are not ASCII. */
type fixture = {
  store: Store.t,
  webkit: Store.id,
  cache: Store.id,
  clip: Store.id,
  sparse: Store.id,
  ignored: Store.id,
  otherDev: Store.id,
  cjk: Store.id,
  emoji: Store.id,
  small: Store.id,
  all: array(Store.id),
};

let fixture = () => {
  let t = Store.create(~root, ~home, ());
  let library = addDir(t, ~parent=Store.rootId, ~name="Library", ());
  let caches = addDir(t, ~parent=library, ~name="Caches", ());
  let webkit = addDir(t, ~parent=caches, ~name="com.apple.WebKit", ());
  let cache = addFile(t, ~parent=webkit, ~name="Cache.data", ~size=1300000000, ());
  let movies = addDir(t, ~parent=Store.rootId, ~name="Movies", ());
  let summer = addDir(t, ~parent=movies, ~name="2019-summer", ());
  let clip = addFile(t, ~parent=summer, ~name="clip.mov", ~size=432000000, ());
  let sparse =
    addFile(
      t,
      ~parent=summer,
      ~name="disk.sparseimage",
      ~size=9000,
      ~flags=Store.fSparseSuspect,
      (),
    );
  let ignored = addDir(t, ~parent=library, ~name="Developer", ~flags=Store.fIgnored, ());
  let otherDev = addDir(t, ~parent=Store.rootId, ~name="Volumes", ~flags=Store.fOtherDev, ());
  /* Every cell of this name is double-width, so a budget that ends on an odd
     boundary cannot be filled exactly - which is the case the padding has to
     cover. */
  let cjk = addDir(t, ~parent=summer, ~name="\xe5\x86\x99\xe7\x9c\x9f\xe3\x83\x95\xe3\x82\xa9\xe3\x83\xab\xe3\x83\x80", ());
  let emoji = addFile(t, ~parent=summer, ~name="\xf0\x9f\x8e\xac-final.mov", ~size=77000000, ());
  let small = addFile(t, ~parent=Store.rootId, ~name="notes.txt", ~size=12, ());
  {
    store: t,
    webkit,
    cache,
    clip,
    sparse,
    ignored,
    otherDev,
    cjk,
    emoji,
    small,
    all: [|webkit, cache, clip, sparse, ignored, otherDev, cjk, emoji, small|],
  };
};

let row =
    (
      f: fixture,
      ~id,
      ~width,
      ~query="",
      ~selected=false,
      ~scopePath=root,
      ~maxSize=1300000000,
      (),
    ) =>
  Rows.rowText(
    ~store=f.store,
    ~id,
    ~width,
    ~maxSize,
    ~query,
    ~scopePath,
    ~home,
    ~selected,
  );

let plain = Element.stripAnsi;

/* The text of every run the row painted Bold, in order. renderPieces emits
 * `open styles, text, reset`, so a highlighted run is exactly the bytes
 * between a Bold escape and the next reset. This is how "and ONLY on the
 * matched cells" is asserted rather than merely "something is bold". */
let boldRuns = (s: string): list(string) => {
  let openTag = Element.styleToAnsi(Element.Bold);
  let close = Element.resetAnsi;
  let ol = String.length(openTag);
  let cl = String.length(close);
  let out = ref([]);
  let i = ref(0);
  let n = String.length(s);
  while (i^ + ol <= n) {
    if (String.sub(s, i^, ol) == openTag) {
      let start = i^ + ol;
      let j = ref(start);
      while (j^ + cl <= n && String.sub(s, j^, cl) != close) {
        incr(j);
      };
      out := [String.sub(s, start, j^ - start), ...out^];
      i := j^ + cl;
    } else {
      incr(i);
    };
  };
  List.rev(out^);
};

let endsWith = (s: string, suffix: string): bool => {
  let sl = String.length(s);
  let fl = String.length(suffix);
  sl >= fl && String.sub(s, sl - fl, fl) == suffix;
};

/* Deliberately includes widths below the chrome itself (1, 5), the exact
 * bar breakpoint pair (55, 56) and nothing equal to 80 - matcha's default
 * everywhere - so a defaulted value can never masquerade as a computed one. */
let widths = [|1, 5, 8, 9, 13, 21, 34, 40, 55, 56, 57, 63, 96, 121|];

let run = () => {
  Test.group("rows: the width contract", () => {
    Test.run("every row measures exactly the width it was given", () => {
      let f = fixture();
      Array.iter(
        w =>
          Array.iter(
            id =>
              List.iter(
                ((query, selected)) => {
                  let r = row(f, ~id, ~width=w, ~query, ~selected, ());
                  Test.assertEqualInt(
                    Element.visibleLength(r),
                    w,
                    Printf.sprintf(
                      "id %d at width %d (query %S, selected %b)",
                      id,
                      w,
                      query,
                      selected,
                    ),
                  );
                },
                [("", false), ("", true), ("ca", false), ("mov", true)],
              ),
            f.all,
          ),
        widths,
      );
    });

    Test.run("a width of zero or less yields an empty row", () => {
      let f = fixture();
      Test.assertEqualStr(row(f, ~id=f.clip, ~width=0, ()), "", "width 0");
      Test.assertEqualStr(row(f, ~id=f.clip, ~width=-7, ()), "", "negative width");
    });

    Test.run("every row ends with a reset", () => {
      /* The ScrollView paints from an arbitrary row without reading the ones
         above it. A row that leaves a style open corrupts every row after it
         the moment the window starts below it - and looks perfect until
         someone scrolls. */
      let f = fixture();
      Array.iter(
        w =>
          Array.iter(
            id =>
              List.iter(
                selected => {
                  let r = row(f, ~id, ~width=w, ~query="ca", ~selected, ());
                  Test.assertTrue(
                    endsWith(r, Element.resetAnsi),
                    Printf.sprintf("id %d at width %d closes its styles", id, w),
                  );
                },
                [false, true],
              ),
            f.all,
          ),
        widths,
      );
    });
  });

  Test.group("rows: the columns", () => {
    Test.run("the size is right-aligned in its field and never overflows", () => {
      let f = fixture();
      let field = r => String.sub(plain(r), Rows.caretWidth, Rows.sizeField);
      let big = field(row(f, ~id=f.cache, ~width=96, ()));
      let tiny = field(row(f, ~id=f.small, ~width=96, ()));
      Test.assertEqualStr(big, "  1.2G", "the biggest number, right-aligned");
      Test.assertEqualStr(tiny, "   12B", "a small number, right-aligned");
      Test.assertEqualInt(String.length(big), Rows.sizeField, "field width");
    });

    Test.run("the size field is wide enough for the widest humanSize", () => {
      /* Pins Rows.sizeField to Fmt.sizeWidth plus the sparse marker, so a
         humanSize that grew a sixth cell fails here rather than silently
         pushing the bar and the path one column right on exactly the rows
         this application exists to show. */
      Test.assertEqualInt(Rows.sizeField, Fmt.sizeWidth + 1, "one marker cell");
      Test.assertTrue(
        Element.visibleLength(Fmt.humanSize(max_int)) <= Fmt.sizeWidth,
        "the largest representable size still fits",
      );
    });

    Test.run("a directory gets a trailing slash and a file does not", () => {
      let f = fixture();
      Test.assertContains(
        plain(row(f, ~id=f.webkit, ~width=96, ())),
        "Library/Caches/com.apple.WebKit/",
        "the directory is marked",
      );
      Test.assertContains(
        plain(row(f, ~id=f.clip, ~width=96, ())),
        "Movies/2019-summer/clip.mov",
        "the file is not",
      );
      Test.assertNotContains(
        plain(row(f, ~id=f.clip, ~width=96, ())),
        "clip.mov/",
        "no slash after a file",
      );
    });

    Test.run("a sparse suspect is marked with a tilde before its size", () => {
      let f = fixture();
      Test.assertContains(
        plain(row(f, ~id=f.sparse, ~width=96, ())),
        "~8.8K",
        "the number is an upper bound and says so",
      );
    });

    Test.run("an unmeasured entry shows a dash, not a zero", () => {
      /* "0B" would read as "this is empty, why is it in my way"; the entry
         was never measured at all. */
      let f = fixture();
      let field = r => String.trim(String.sub(plain(r), Rows.caretWidth, Rows.sizeField));
      Test.assertEqualStr(field(row(f, ~id=f.ignored, ~width=96, ())), "-", "ignored");
      Test.assertEqualStr(field(row(f, ~id=f.otherDev, ~width=96, ())), "-", "other volume");
    });

    Test.run("the caret marks the selection and the ignored entries", () => {
      let f = fixture();
      let caret = r => String.sub(plain(r), 0, 1);
      Test.assertEqualStr(caret(row(f, ~id=f.clip, ~width=96, ())), " ", "an ordinary row");
      Test.assertEqualStr(
        caret(row(f, ~id=f.clip, ~width=96, ~selected=true, ())),
        ">",
        "the selected row",
      );
      Test.assertEqualStr(caret(row(f, ~id=f.ignored, ~width=96, ())), "x", "an ignored row");
      Test.assertEqualStr(
        caret(row(f, ~id=f.ignored, ~width=96, ~selected=true, ())),
        ">",
        "selection wins over ignored",
      );
    });

    Test.run("the path is rendered relative to the scope", () => {
      let f = fixture();
      let r = plain(row(f, ~id=f.clip, ~width=96, ~scopePath=root ++ "/Movies", ()));
      Test.assertContains(r, "2019-summer/clip.mov", "relative to the scope");
      Test.assertNotContains(r, "Movies/2019-summer", "and not to the root");
    });

    Test.run("an entry outside the scope keeps an absolute, tilde-shortened path", () => {
      /* A rescan can re-root the tree under the UI, which leaves entries that
         are not below the current scope. Fmt.relativeTo hands those back
         unchanged, and an absolute path is far more useful shortened. */
      let f = fixture();
      let r = plain(row(f, ~id=f.clip, ~width=96, ~scopePath="/nowhere", ()));
      Test.assertContains(r, "~/scan/Movies/2019-summer/clip.mov", "tildified");
    });
  });

  Test.group("rows: the bar", () => {
    Test.run("the largest entry fills the bar and a small one barely marks it", () => {
      let f = fixture();
      let full = "\xe2\x96\x88";
      Test.assertEqualInt(
        Test.countOccurrences(plain(row(f, ~id=f.cache, ~width=96, ())), full),
        Rows.barCells,
        "the entry the bar is scaled to",
      );
      Test.assertEqualInt(
        Test.countOccurrences(plain(row(f, ~id=f.small, ~width=96, ())), full),
        0,
        "12 bytes against 1.2 GB",
      );
    });

    Test.run("the bar is dropped below the breakpoint and the row still fits", () => {
      let f = fixture();
      let full = "\xe2\x96\x88";
      let wide = row(f, ~id=f.cache, ~width=Rows.barMinWidth, ());
      let narrow = row(f, ~id=f.cache, ~width=Rows.barMinWidth - 1, ());
      Test.assertTrue(Test.contains(plain(wide), full), "the bar is drawn at the breakpoint");
      Test.assertFalse(
        Test.contains(plain(narrow), full),
        "and dropped one cell below it",
      );
      Test.assertEqualInt(Element.visibleLength(wide), Rows.barMinWidth, "wide row width");
      Test.assertEqualInt(
        Element.visibleLength(narrow),
        Rows.barMinWidth - 1,
        "narrow row width",
      );
      /* The path gains the nine cells the bar and its gap gave up, rather
         than the row simply losing them. */
      Test.assertContains(
        plain(narrow),
        "com.apple.WebKit/Cache.data",
        "the path got the room",
      );
    });
  });

  Test.group("rows: eliding and unicode", () => {
    Test.run("a path too long for the row keeps its tail", () => {
      /* TruncateStart, not Truncate: the informative end of a path is the
         basename, and a row reading "Library/Caches/com.appl..." answers
         nothing the reader did not already know from the row above. */
      let f = fixture();
      let r = plain(row(f, ~id=f.cache, ~width=40, ()));
      Test.assertContains(r, "Cache.data", "the basename survived");
      Test.assertContains(r, "\xe2\x80\xa6", "and the front was elided");
      Test.assertNotContains(r, "Library", "the leading directories went");
    });

    Test.run("a double-width name is never cut mid-character", () => {
      let f = fixture();
      /* U+FFFD is what a byte-wise cut through a UTF-8 sequence decodes to,
         so its absence at every width is the assertion. */
      Array.iter(
        w => {
          let r = row(f, ~id=f.cjk, ~width=w, ());
          Test.assertNotContains(
            plain(r),
            "\xef\xbf\xbd",
            Printf.sprintf("no replacement character at width %d", w),
          );
          Test.assertEqualInt(Element.visibleLength(r), w, "and still exactly the width");
        },
        widths,
      );
    });

    Test.run("a name carrying control bytes cannot inject styling", () => {
      /* Every byte but '/' and NUL is legal in a filename, and hog is
         pointed at directories nobody has read. A name holding an ESC would
         set the SGR state for the rest of the row and for every row after
         it; one holding a newline would turn one row into two and put the
         whole list out of step with its own offsets. */
      let f = fixture();
      let hostile =
        addFile(f.store, ~parent=Store.rootId, ~name="ev\027[31mil\nname", ~size=5, ());
      let r = row(f, ~id=hostile, ~width=96, ());
      Test.assertEqualInt(Element.visibleLength(r), 96, "still exactly the width");
      Test.assertContains(plain(r), "ev?[31mil?name", "the control bytes are inert");
      Test.assertEqualInt(
        Test.countOccurrences(r, "\027[31m"),
        0,
        "no foreign colour reached the row",
      );
      Test.assertEqualInt(Test.countOccurrences(r, "\n"), 0, "and it is still one row");
    });

    Test.run("an emoji name survives the same treatment", () => {
      let f = fixture();
      Array.iter(
        w => {
          let r = row(f, ~id=f.emoji, ~width=w, ());
          Test.assertNotContains(plain(r), "\xef\xbf\xbd", "no replacement character");
          Test.assertEqualInt(Element.visibleLength(r), w, "exactly the width");
        },
        widths,
      );
      Test.assertContains(
        plain(row(f, ~id=f.emoji, ~width=96, ())),
        "\xf0\x9f\x8e\xac-final.mov",
        "and is shown intact when it fits",
      );
    });
  });

  Test.group("rows: styling", () => {
    Test.run("the fuzzy match is marked, and only on the matched cells", () => {
      let f = fixture();
      let r = row(f, ~id=f.webkit, ~width=96, ~query="webkit", ());
      let marked = String.concat("", boldRuns(r));
      Test.assertEqualStr(
        String.lowercase_ascii(marked),
        "webkit",
        "exactly the cells the query matched",
      );
    });

    Test.run("nothing is marked without a query", () => {
      let f = fixture();
      Test.assertEqualInt(
        List.length(boldRuns(row(f, ~id=f.webkit, ~width=96, ()))),
        0,
        "an empty query highlights nothing",
      );
    });

    Test.run("a query that does not match marks nothing", () => {
      let f = fixture();
      Test.assertEqualInt(
        List.length(boldRuns(row(f, ~id=f.webkit, ~width=96, ~query="zzqq", ()))),
        0,
        "no match, no marks",
      );
    });

    Test.run("the selected row is inverted throughout", () => {
      let f = fixture();
      let inverted = Element.styleToAnsi(Element.Inverted);
      let sel = row(f, ~id=f.clip, ~width=96, ~selected=true, ());
      let unsel = row(f, ~id=f.clip, ~width=96, ());
      Test.assertContains(sel, inverted, "the selected row carries Inverted");
      Test.assertNotContains(unsel, inverted, "an ordinary row does not");
      /* Every piece carries the whole-row style, because each piece resets
         when it closes - opening Inverted once at the start of the row would
         survive exactly until the first styled column. */
      Test.assertTrue(
        Test.countOccurrences(sel, inverted) > 1,
        "carried by every piece, not opened once",
      );
      /* And the tiers collapse: no grey directory next to its own basename
         under inverted video. */
      Test.assertNotContains(
        sel,
        Element.styleToAnsi(Element.FgColor(Element.BrightBlack)),
        "the directory tier collapses into the selection",
      );
    });

    Test.run("an unselected row keeps its tiers", () => {
      let f = fixture();
      let r = row(f, ~id=f.clip, ~width=96, ());
      Test.assertContains(
        r,
        Element.styleToAnsi(Element.FgColor(Element.BrightBlack)),
        "the leading directories are grey",
      );
      Test.assertContains(r, Element.styleToAnsi(Element.Dim), "the size and bar are dim");
    });
  });

  Test.group("rows: lazy baking", () => {
    /* A baked row is width cells wide and an unbaked slot is the empty
       string, so counting non-empty slots counts baked rows - without
       reaching into the flags, and measuring the thing that actually
       reaches the ScrollView. */
    let bakedCount = t =>
      Array.fold_left((n, r) => r == "" ? n : n + 1, 0, Rows.rows(t));

    let built = (~width=96, ~entries=?, ()) => {
      let f = fixture();
      let entries =
        switch (entries) {
        | Some(e) => e
        | None => f.all
        };
      (f, Rows.build(~store=f.store, ~entries, ~width, ~query="", ~scopePath=root, ~home));
    };

    Test.run("build bakes nothing", () => {
      /* The whole point. The row set is rebuilt on every scan snapshot - ten
         times a second - and on every filter keystroke; baking a hundred
         thousand rows each time freezes the UI for the whole scan. */
      let (_, t) = built();
      Test.assertEqualInt(Rows.count(t), 9, "the entries are all there");
      Test.assertEqualInt(bakedCount(t), 0, "and none of them is baked");
    });

    Test.run("ensureRange bakes its window and nothing else", () => {
      let (_, t) = built();
      Rows.ensureRange(t, ~fromRow=2, ~toRow=5);
      Test.assertEqualInt(bakedCount(t), 3, "three rows baked");
      let rows = Rows.rows(t);
      Test.assertEqualStr(rows[1], "", "the row before the window");
      Test.assertEqualStr(rows[5], "", "the row after it");
      Test.assertEqualInt(Element.visibleLength(rows[2]), 96, "and the window is real");
    });

    Test.run("ensureRange clamps a window running past either end", () => {
      let (_, t) = built();
      Rows.ensureRange(t, ~fromRow=-20, ~toRow=200);
      Test.assertEqualInt(bakedCount(t), 9, "everything baked, nothing raised");
    });

    Test.run("ensureRange twice changes nothing", () => {
      let (_, t) = built();
      Rows.ensureRange(t, ~fromRow=0, ~toRow=4);
      let snapshot = Array.copy(Rows.rows(t));
      Rows.ensureRange(t, ~fromRow=0, ~toRow=4);
      Array.iteri(
        (i, r) => Test.assertEqualStr(Rows.rows(t)[i], r, "row " ++ string_of_int(i)),
        snapshot,
      );
    });

    Test.run("the rows array identity is stable", () => {
      /* ScrollView re-reads the same array every frame; handing it a new one
         would be a rebuild, which is exactly what the laziness avoids. */
      let (_, t) = built();
      let before = Rows.rows(t);
      Rows.ensureRange(t, ~fromRow=0, ~toRow=4);
      Rows.ensureSelected(t, ~sel=1);
      Test.assertTrue(Rows.rows(t) === before, "same array throughout");
    });

    Test.run("ensureSelected twice is byte-identical", () => {
      /* THE MEASURE-PASS GUARD. matcha renders the tree twice per frame and
         both passes run the render body, so both calls land. A second call
         that unbaked the row it had just selected would leave the paint pass
         showing no selection at all. */
      let (_, t) = built();
      Rows.ensureRange(t, ~fromRow=0, ~toRow=9);
      Rows.ensureSelected(t, ~sel=3);
      let snapshot = Array.copy(Rows.rows(t));
      Rows.ensureSelected(t, ~sel=3);
      Array.iteri(
        (i, r) => Test.assertEqualStr(Rows.rows(t)[i], r, "row " ++ string_of_int(i)),
        snapshot,
      );
      Test.assertContains(
        Rows.rows(t)[3],
        Element.styleToAnsi(Element.Inverted),
        "and it is still the selected one",
      );
    });

    Test.run("moving the selection restores the row it left", () => {
      let (f, t) = built();
      let plainRow3 =
        row(f, ~id=f.all[3], ~width=96, ~maxSize=Store.get(f.store, f.cache).Store.size, ());
      Rows.ensureRange(t, ~fromRow=0, ~toRow=9);
      Rows.ensureSelected(t, ~sel=3);
      Rows.ensureSelected(t, ~sel=4);
      Test.assertEqualStr(Rows.rows(t)[3], plainRow3, "back to its plain bytes");
      Test.assertContains(
        Rows.rows(t)[4],
        Element.styleToAnsi(Element.Inverted),
        "and the new row is selected",
      );
    });

    Test.run("ensureSelected bakes a row the window never reached", () => {
      let (_, t) = built();
      Rows.ensureRange(t, ~fromRow=0, ~toRow=2);
      Rows.ensureSelected(t, ~sel=7);
      Test.assertEqualInt(bakedCount(t), 3, "the window plus the selection");
      Test.assertContains(
        Rows.rows(t)[7],
        Element.styleToAnsi(Element.Inverted),
        "selected",
      );
    });

    Test.run("ensureRange leaves the selected row alone", () => {
      /* ensureSelected always runs after ensureRange, so the ordering is safe
         only because a baked row - selected included - is skipped. */
      let (_, t) = built();
      Rows.ensureSelected(t, ~sel=2);
      Rows.ensureRange(t, ~fromRow=0, ~toRow=9);
      Test.assertContains(
        Rows.rows(t)[2],
        Element.styleToAnsi(Element.Inverted),
        "the selection survived the window bake",
      );
    });

    Test.run("a selection outside the list is harmless", () => {
      let (_, t) = built();
      Rows.ensureRange(t, ~fromRow=0, ~toRow=9);
      Rows.ensureSelected(t, ~sel=-1);
      Rows.ensureSelected(t, ~sel=99);
      Test.assertEqualInt(bakedCount(t), 9, "nothing baked, nothing raised");
    });

    Test.run("an empty entry set is safe", () => {
      /* The list is empty on the first frame of every scan and after a
         filter that matches nothing, so this is the common case, not the
         edge case. */
      let (_, t) = built(~entries=[||], ());
      Test.assertEqualInt(Rows.count(t), 0, "no rows");
      Test.assertEqualInt(Array.length(Rows.rows(t)), 0, "and no array to paint");
      Rows.ensureRange(t, ~fromRow=0, ~toRow=40);
      Rows.ensureSelected(t, ~sel=0);
      Test.assertEqualInt(Rows.count(t), 0, "still no rows");
    });

    Test.run("a width of zero or less is safe", () => {
      let (_, t) = built(~width=0, ());
      Rows.ensureRange(t, ~fromRow=0, ~toRow=9);
      Rows.ensureSelected(t, ~sel=2);
      Array.iter(r => Test.assertEqualStr(r, "", "empty row"), Rows.rows(t));
      let (_, t2) = built(~width=-4, ());
      Rows.ensureRange(t2, ~fromRow=0, ~toRow=9);
      Array.iter(r => Test.assertEqualStr(r, "", "empty row"), Rows.rows(t2));
    });

    Test.run("the bar is scaled to the entries in the list, not the tree", () => {
      /* After descending or filtering, the comparison the reader is making is
         between the rows in front of them - so a list whose largest entry is
         small still fills its bar. */
      let f = fixture();
      let t =
        Rows.build(
          ~store=f.store,
          ~entries=[|f.small|],
          ~width=96,
          ~query="",
          ~scopePath=root,
          ~home,
        );
      Rows.ensureRange(t, ~fromRow=0, ~toRow=1);
      Test.assertEqualInt(
        Test.countOccurrences(plain(Rows.rows(t)[0]), "\xe2\x96\x88"),
        Rows.barCells,
        "the only entry is the biggest entry",
      );
    });
  });
};
