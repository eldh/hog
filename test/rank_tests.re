/*
 * Tests for lib/Rank.re - the frontier and the drill-down sort.
 *
 * No filesystem anywhere in here. Every tree is a nested literal turned into
 * an arena by `build` below, which is the whole reason Rank is a pure
 * function of (store, root, params): the interesting inputs are 60-way
 * fan-outs and seven-link pass-through chains, and building those on disk to
 * test an arithmetic decision would be slow, flaky and no more truthful.
 *
 * The frontier is a POLICY, not a formula, so every behavioural case here
 * asserts the REASON as well as the shape - the fan-out count and the
 * dominance ratio that made the decision, via `Rank.surveyChildren`. A test
 * that only pins the output would still pass if a future predicate reached
 * the same rows for the wrong reason, and would then be pinning a
 * coincidence.
 *
 * The four cases the plan calls out by name are marked "worked case N".
 */

let home = "/Users/tester";
let root = home ++ "/scan";

let mb = (n: int): int => n * 1024 * 1024;
let gb = (n: int): int => n * 1024 * 1024 * 1024;

/* ------------------------------------------------------------- the builder */

/* A tree as a nested literal. `Fx`/`Dx` are the same thing carrying flags,
   which is how ignored, other-device and hard-link nodes get into a tree. */
type spec =
  | F(string, int) /* file: name, size */
  | Fx(string, int, int) /* file: name, size, flags */
  | D(string, list(spec)) /* directory: name, children */
  | Dx(string, int, list(spec)); /* directory: name, flags, children */

/* The arena's contiguous-child-range contract says a directory's entries must
   be allocated in ONE uninterrupted run, and getting that right by hand in
   twenty test cases is exactly the kind of fiddly bookkeeping that produces a
   silently reparented subtree. So it lives here, once: allocate this level's
   children as a batch, set the range, propagate the sizes, and only THEN
   recurse - because a recursive call would allocate a grandchild in the
   middle of the batch and break the range for good. */
let rec addChildren = (t: Store.t, parent: Store.id, specs: list(spec)): unit => {
  let first = t.Store.count;
  let ids = ref([]);
  List.iter(
    spec => {
      let id =
        switch (spec) {
        | F(name, size) =>
          Store.alloc(t, ~name, ~parent, ~kind=Store.File, ~size, ())
        | Fx(name, size, flags) =>
          Store.alloc(t, ~name, ~parent, ~kind=Store.File, ~size, ~flags, ())
        | D(name, _) => Store.alloc(t, ~name, ~parent, ~kind=Store.Dir, ())
        | Dx(name, flags, _) =>
          Store.alloc(t, ~name, ~parent, ~kind=Store.Dir, ~flags, ())
        };
      ids := [id, ...ids^];
    },
    specs,
  );
  let ids = Array.of_list(List.rev(ids^));
  Store.setChildRange(t, parent, ~first, ~count=Array.length(ids));
  /* Propagate exactly as Walk does: a file contributes its bytes and itself,
     a directory contributes only itself. */
  List.iteri(
    (i, spec) => {
      let id = ids[i];
      switch (spec) {
      | F(_, size)
      | Fx(_, size, _) => Store.addUp(t, id, ~bytes=size, ~items=1)
      | D(_, _)
      | Dx(_, _, _) => Store.addUp(t, id, ~bytes=0, ~items=1)
      };
    },
    specs,
  );
  List.iteri(
    (i, spec) =>
      switch (spec) {
      | D(_, kids)
      | Dx(_, _, kids) => addChildren(t, ids[i], kids)
      | F(_, _)
      | Fx(_, _, _) => ()
      },
    specs,
  );
};

let build = (specs: list(spec)): Store.t => {
  let t = Store.create(~root, ~home, ());
  addChildren(t, Store.rootId, specs);
  t;
};

/* ------------------------------------------------------------- assertions */

/* A node's path relative to the scan root, which is what the cases below are
   written in terms of - "Library/Caches" reads as the tree literal does. */
let rel = (t: Store.t, id: Store.id): string => {
  let p = Store.path(t, id);
  let rl = String.length(t.Store.root);
  String.length(p) > rl ? String.sub(p, rl + 1, String.length(p) - rl - 1) : "";
};

let paths = (t: Store.t, ids: array(Store.id)): list(string) =>
  Array.to_list(Array.map(rel(t), ids));

let show = (t: Store.t, ids: array(Store.id)): string =>
  "[" ++ String.concat(", ", paths(t, ids)) ++ "]";

let idOf = (t: Store.t, relPath: string): Store.id =>
  switch (Store.resolve(t, root ++ "/" ++ relPath)) {
  | Some(id) => id
  | None => failwith("no such node in the test tree: " ++ relPath)
  };

let assertRow = (t, ids, p, msg) =>
  Test.assertTrue(List.mem(p, paths(t, ids)), msg ++ " - got " ++ show(t, ids));

let assertNoRow = (t, ids, p, msg) =>
  Test.assertFalse(
    List.mem(p, paths(t, ids)),
    msg ++ " - got " ++ show(t, ids),
  );

let assertRows = (t, ids, expected: list(string), msg) =>
  Test.assertTrue(
    paths(t, ids) == expected,
    msg
    ++ "\n      expected: ["
    ++ String.concat(", ", expected)
    ++ "]\n      actual:   "
    ++ show(t, ids),
  );

/* --------------------------------------------------------------- fixtures */

/* worked case 1: ~/Library/Caches, sixty caches of about two gigabytes. */
let cachesTree = () =>
  build([
    D(
      "Library",
      [
        D(
          "Caches",
          List.init(60, i =>
            D(
              Printf.sprintf("cache%02d", i),
              [F("data.bin", gb(2))],
            )
          ),
        ),
      ],
    ),
  ]);

let run = () => {
  Test.group("rank: the frontier, basics", () => {
    Test.run("an empty tree ranks to nothing", () => {
      let t = build([]);
      let rows = Rank.frontier(t, ~root=Store.rootId, ~params=Rank.defaults);
      Test.assertEqualInt(Array.length(rows), 0, "no rows");
    });

    Test.run("a tree with one big file returns that file", () => {
      let t = build([F("disk.img", gb(6))]);
      let rows = Rank.frontier(t, ~root=Store.rootId, ~params=Rank.defaults);
      assertRows(t, rows, ["disk.img"], "the only file is the only row");
    });

    Test.run("nothing below the threshold is returned", () => {
      /* T = max(64 MiB, total/1000), and total here is about 1.1 GB, so the
         64 MiB floor is what bites. Both the small file and the directory
         whose whole subtree is small are under it. */
      let t =
        build([
          F("big.bin", gb(1)),
          F("small.bin", mb(10)),
          D("scraps", List.init(10, i => F(Printf.sprintf("s%d", i), mb(5)))),
        ]);
      let rows = Rank.frontier(t, ~root=Store.rootId, ~params=Rank.defaults);
      assertRows(t, rows, ["big.bin"], "only the one above the threshold");
    });

    Test.run("the root is never a row, however wide it is", () => {
      let t =
        build(List.init(60, i => F(Printf.sprintf("f%02d.bin", i), gb(2))));
      let rows = Rank.frontier(t, ~root=Store.rootId, ~params=Rank.defaults);
      Test.assertEqualInt(Array.length(rows), 60, "every child is a row");
      Test.assertFalse(
        Array.exists(id => id == Store.rootId, rows),
        "the root itself was emitted",
      );
    });

    Test.run("rows come back size-descending", () => {
      let t =
        build([
          F("a.bin", gb(1)),
          F("b.bin", gb(9)),
          F("c.bin", gb(4)),
          F("d.bin", gb(7)),
        ]);
      let rows = Rank.frontier(t, ~root=Store.rootId, ~params=Rank.defaults);
      assertRows(
        t,
        rows,
        ["b.bin", "d.bin", "c.bin", "a.bin"],
        "largest first",
      );
    });
  });

  Test.group("rank: the four worked cases", () => {
    Test.run("worked case 1: wide fan-out wins over depth", () => {
      /* Library holds one thing, so it is a pass-through and splits. Caches
         holds sixty similar caches, so naming one of them says nothing:
         it is the row, and Library never appears at all. */
      let t = cachesTree();
      let rows = Rank.frontier(t, ~root=Store.rootId, ~params=Rank.defaults);
      assertRows(t, rows, ["Library/Caches"], "the caches folder, whole");
      assertNoRow(t, rows, "Library", "Library is a pass-through, not a row");

      /* And for the reason claimed, not by accident: fan-out over the cap,
         with no child anywhere near half the total. */
      let caches = idOf(t, "Library/Caches");
      let total = Store.get(t, caches).Store.size;
      let (a, m) =
        Rank.surveyChildren(
          t,
          caches,
          ~threshold=
            Rank.threshold(
              ~total=Store.total(t),
              ~params=Rank.defaults,
            ),
        );
      Test.assertEqualInt(a, 60, "sixty children above the threshold");
      Test.assertTrue(a > Rank.defaults.fanoutCap, "fan-out is over the cap");
      Test.assertTrue(m * 2 < total, "and nothing dominates, so no override");
    });

    Test.run("worked case 2: dominance splits a pass-through", () => {
      /* Movies is 5 GB of which one video is 4 GB. The fan-out is 11, over
         the cap of 8 - only the dominance override gets this split, and the
         video ends up as its own row. */
      let t =
        build([
          D(
            "Movies",
            [
              F("holiday.mov", gb(4)),
              ...List.init(10, i =>
                   F(Printf.sprintf("clip%02d.mov", i), mb(100))
                 ),
            ],
          ),
        ]);
      let rows = Rank.frontier(t, ~root=Store.rootId, ~params=Rank.defaults);
      assertRow(t, rows, "Movies/holiday.mov", "the big video is its own row");
      assertNoRow(t, rows, "Movies", "Movies was split, not emitted");
      Test.assertEqualInt(Array.length(rows), 11, "every child became a row");

      let movies = idOf(t, "Movies");
      let size = Store.get(t, movies).Store.size;
      let (a, m) =
        Rank.surveyChildren(
          t,
          movies,
          ~threshold=Rank.threshold(~total=size, ~params=Rank.defaults),
        );
      Test.assertTrue(
        a > Rank.defaults.fanoutCap,
        "the fan-out alone would have refused the split",
      );
      Test.assertTrue(m * 2 >= size, "dominance is what carried it");
    });

    Test.run("worked case 3: a wide directory of similar files is one row", () => {
      /* Twenty-one similar videos: no dominance, fan-out well over the cap.
         Right for a tool whose thesis is "I already found the files". */
      let t =
        build([
          D(
            "Movies",
            List.init(21, i =>
              F(Printf.sprintf("ep%02d.mov", i), mb(950) + mb(i))
            ),
          ),
        ]);
      let rows = Rank.frontier(t, ~root=Store.rootId, ~params=Rank.defaults);
      assertRows(t, rows, ["Movies"], "the folder, whole");
      assertNoRow(t, rows, "Movies/ep20.mov", "no individual video is a row");

      let movies = idOf(t, "Movies");
      let size = Store.get(t, movies).Store.size;
      let (a, m) =
        Rank.surveyChildren(
          t,
          movies,
          ~threshold=Rank.threshold(~total=size, ~params=Rank.defaults),
        );
      Test.assertEqualInt(a, 21, "twenty-one children above the threshold");
      Test.assertTrue(a > Rank.defaults.fanoutCap, "over the fan-out cap");
      Test.assertTrue(m * 2 < size, "and no child holds half, so it is emitted");
    });

    Test.run("worked case 4: a pass-through chain costs iterations, not rows", () => {
      /* Every link holds exactly one big thing, so every link splits and none
         of them is ever a row. Seven splits, one row. */
      let t =
        build([
          D(
            "Library",
            [
              D(
                "Containers",
                [
                  D(
                    "com.docker.docker",
                    [
                      D(
                        "Data",
                        [
                          D(
                            "vms",
                            [D("0", [D("data", [F("Docker.raw", gb(40))])])],
                          ),
                        ],
                      ),
                    ],
                  ),
                ],
              ),
            ],
          ),
          D("Documents", [F("thesis.pdf", gb(2))]),
        ]);
      let rows = Rank.frontier(t, ~root=Store.rootId, ~params=Rank.defaults);
      let leaf = "Library/Containers/com.docker.docker/Data/vms/0/data/Docker.raw";
      assertRow(t, rows, leaf, "the image at the end of the chain");
      List.iter(
        p => assertNoRow(t, rows, p, "an intermediate link became a row"),
        [
          "Library",
          "Library/Containers",
          "Library/Containers/com.docker.docker",
          "Library/Containers/com.docker.docker/Data",
          "Library/Containers/com.docker.docker/Data/vms",
          "Library/Containers/com.docker.docker/Data/vms/0",
          "Library/Containers/com.docker.docker/Data/vms/0/data",
        ],
      );
      assertRows(t, rows, [leaf, "Documents/thesis.pdf"], "two rows in all");
    });
  });

  Test.group("rank: the frontier's invariants", () => {
    /* One mixed tree that exercises both split reasons and both emit reasons
       at once, so the antichain claim is tested against something with real
       depth rather than a flat list. */
    let mixed = () =>
      build([
        D(
          "Library",
          [
            D(
              "Caches",
              List.init(12, i =>
                D(Printf.sprintf("cache%02d", i), [F("data.bin", gb(2))])
              ),
            ),
            D(
              "Developer",
              [F("DerivedData.bin", gb(8)), F("notes.bin", mb(70))],
            ),
          ],
        ),
        D("Movies", [F("a.mov", gb(4)), F("b.mov", gb(1))]),
        F("disk.img", gb(6)),
      ]);

    Test.run("no returned node is an ancestor of another", () => {
      let t = mixed();
      let rows = Rank.frontier(t, ~root=Store.rootId, ~params=Rank.defaults);
      Test.assertTrue(
        Array.length(rows) > 3,
        "the tree must produce several rows or this proves nothing",
      );
      Array.iter(
        a =>
          Array.iter(
            b =>
              if (a != b) {
                Test.assertFalse(
                  Store.isAncestor(t, ~ancestor=a, b),
                  "the frontier is not an antichain: "
                  ++ rel(t, a)
                  ++ " contains "
                  ++ rel(t, b),
                );
              },
            rows,
          ),
        rows,
      );
    });

    Test.run("the same store ranks identically twice", () => {
      /* The frontier is recomputed on every published snapshot, so two calls
         that disagree would move a row under the user's cursor. */
      let t = mixed();
      let a = Rank.frontier(t, ~root=Store.rootId, ~params=Rank.defaults);
      let b = Rank.frontier(t, ~root=Store.rootId, ~params=Rank.defaults);
      Test.assertTrue(a == b, "two calls, one answer: " ++ show(t, a));
    });

    Test.run("ties are broken by name, not by allocation order", () => {
      let forward = build([F("a.bin", gb(1)), F("b.bin", gb(1))]);
      let reversed = build([F("b.bin", gb(1)), F("a.bin", gb(1))]);
      let rowsF =
        Rank.frontier(forward, ~root=Store.rootId, ~params=Rank.defaults);
      let rowsR =
        Rank.frontier(reversed, ~root=Store.rootId, ~params=Rank.defaults);
      assertRows(forward, rowsF, ["a.bin", "b.bin"], "allocated a then b");
      assertRows(reversed, rowsR, ["a.bin", "b.bin"], "allocated b then a");
    });

    Test.run("maxRows caps the result", () => {
      let t =
        build(
          List.init(20, i => F(Printf.sprintf("f%02d.bin", i), mb(100 * (i + 1)))),
        );
      let rows =
        Rank.frontier(
          t,
          ~root=Store.rootId,
          ~params={...Rank.defaults, maxRows: 5},
        );
      assertRows(
        t,
        rows,
        ["f19.bin", "f18.bin", "f17.bin", "f16.bin", "f15.bin"],
        "the five largest and nothing else",
      );
    });

    Test.run("the row budget can veto a split", () => {
      /* Five children, fan-out under the cap - the only thing that can refuse
         this split is the budget, so the same tree at maxRows 200 must give
         the opposite answer. */
      let t =
        build([
          D("Big", List.init(5, i => F(Printf.sprintf("p%d.bin", i), gb(1)))),
        ]);
      let tight =
        Rank.frontier(
          t,
          ~root=Store.rootId,
          ~params={...Rank.defaults, maxRows: 3},
        );
      assertRows(t, tight, ["Big"], "no room to split, so emit whole");
      let roomy = Rank.frontier(t, ~root=Store.rootId, ~params=Rank.defaults);
      Test.assertEqualInt(
        Array.length(roomy),
        5,
        "with room, the same directory splits",
      );
    });

    Test.run("maxDepth stops the descent", () => {
      let t = build([D("a", [D("b", [D("c", [F("d.bin", gb(4))])])])]);
      let shallow =
        Rank.frontier(
          t,
          ~root=Store.rootId,
          ~params={...Rank.defaults, maxDepth: 2},
        );
      assertRows(t, shallow, ["a/b"], "descent stops at depth 2");
      let deep = Rank.frontier(t, ~root=Store.rootId, ~params=Rank.defaults);
      assertRows(t, deep, ["a/b/c/d.bin"], "and the depth cap is what did it");
    });

    Test.run("ignored, other-device and hard-link nodes are never rows", () => {
      /* The hard-link node carries a size on purpose. A real walk forces it
         to zero, which would make this test pass for the wrong reason - it
         has to be the FLAG that excludes it. */
      let t =
        build([
          Fx("cloud.bin", gb(50), Store.fIgnored),
          Fx("other.bin", gb(40), Store.fOtherDev),
          Fx("clone.bin", gb(30), Store.fHardLink),
          F("real.bin", gb(1)),
        ]);
      let rows = Rank.frontier(t, ~root=Store.rootId, ~params=Rank.defaults);
      assertRows(t, rows, ["real.bin"], "only the ordinary file");
    });

    Test.run("an excluded child does not count toward the fan-out", () => {
      /* The ignored file is the biggest thing in the directory by a wide
         margin, so a survey that forgot the flag would report twelve children
         and a fifty-gigabyte maximum - and the dominance test would then fire
         on bytes we are not even allowed to offer to delete. */
      let t =
        build([
          D(
            "Library",
            [
              Fx("cloud.bin", gb(50), Store.fIgnored),
              F("real.bin", gb(4)),
              ...List.init(10, i => F(Printf.sprintf("x%d.bin", i), mb(200)))
            ],
          ),
        ]);
      let library = idOf(t, "Library");
      let (a, m) = Rank.surveyChildren(t, library, ~threshold=mb(100));
      Test.assertEqualInt(a, 11, "the ignored file is not one of them");
      Test.assertEqualInt(m, gb(4), "and it is not the largest, either");
      let rows = Rank.frontier(t, ~root=Store.rootId, ~params=Rank.defaults);
      assertNoRow(t, rows, "Library/cloud.bin", "and it is never a row");
    });

    Test.run("a directory of many small children is emitted whole", () => {
      /* node_modules: three hundred packages, every one of them under T, so
         there is nothing to split into and the folder is the answer. */
      let t =
        build([
          D(
            "node_modules",
            List.init(300, i => F(Printf.sprintf("pkg%03d.bin", i), mb(5))),
          ),
        ]);
      let modules = idOf(t, "node_modules");
      let (a, _) =
        Rank.surveyChildren(
          t,
          modules,
          ~threshold=Rank.threshold(~total=Store.total(t), ~params=Rank.defaults),
        );
      Test.assertEqualInt(a, 0, "no child clears the threshold");
      let rows = Rank.frontier(t, ~root=Store.rootId, ~params=Rank.defaults);
      assertRows(t, rows, ["node_modules"], "so the folder is the row");
    });

    Test.run("the frontier can be rooted anywhere, not just at the scan root", () => {
      /* This is what `r` does in the UI: re-run the ranking from the
         directory the user is standing in. Rooted at Caches, the sixty
         caches are the seed, each one a pass-through over its own file. */
      let t = cachesTree();
      let rows =
        Rank.frontier(t, ~root=idOf(t, "Library/Caches"), ~params=Rank.defaults);
      Test.assertEqualInt(Array.length(rows), 60, "one row per cache");
      Test.assertTrue(
        List.for_all(
          p => Filename.basename(p) == "data.bin",
          paths(t, rows),
        ),
        "each cache was a pass-through over its file: " ++ show(t, rows),
      );
    });
  });

  Test.group("rank: surveyChildren", () => {
    Test.run("counts the children at or above the threshold, and the largest", () => {
      let t =
        build([
          D(
            "d",
            [
              F("a.bin", mb(300)),
              F("b.bin", mb(100)),
              F("c.bin", mb(99)),
              F("d.bin", mb(1)),
            ],
          ),
        ]);
      let (a, m) = Rank.surveyChildren(t, idOf(t, "d"), ~threshold=mb(100));
      Test.assertEqualInt(a, 2, "at-or-above is inclusive of the boundary");
      Test.assertEqualInt(m, mb(300), "the largest of those two");
    });

    Test.run("a leaf surveys to nothing", () => {
      let t = build([F("f.bin", gb(1))]);
      let (a, m) = Rank.surveyChildren(t, idOf(t, "f.bin"), ~threshold=1);
      Test.assertEqualInt(a, 0, "no children");
      Test.assertEqualInt(m, 0, "and no largest");
    });
  });

  Test.group("rank: sortedChildren", () => {
    let tree = () =>
      build([
        D(
          "d",
          [
            F("z.bin", mb(100)),
            F("a.bin", mb(100)),
            F("big.bin", mb(500)),
            F("tiny.bin", 12),
            D("empty", []),
          ],
        ),
      ]);

    Test.run("orders by size, then by name", () => {
      let t = tree();
      let rows = Rank.sortedChildren(t, idOf(t, "d"), ~showIgnored=false);
      assertRows(
        t,
        rows,
        ["d/big.bin", "d/a.bin", "d/z.bin", "d/tiny.bin", "d/empty"],
        "size descending, equal sizes by name",
      );
    });

    Test.run("keeps entries the frontier would have thresholded away", () => {
      /* The drill-down is a file manager, not an opinion: a twelve-byte file
         is still an entry in the directory the user opened. */
      let t = tree();
      let rows = Rank.sortedChildren(t, idOf(t, "d"), ~showIgnored=false);
      assertRow(t, rows, "d/tiny.bin", "a tiny file is still listed");
      assertRow(t, rows, "d/empty", "and so is an empty directory");
    });

    Test.run("hides ignored entries unless asked for them", () => {
      let t =
        build([
          D(
            "d",
            [
              F("keep.bin", mb(10)),
              Fx("skip.bin", mb(900), Store.fIgnored),
            ],
          ),
        ]);
      let hidden = Rank.sortedChildren(t, idOf(t, "d"), ~showIgnored=false);
      assertRows(t, hidden, ["d/keep.bin"], "ignored entries are hidden");
      let shown = Rank.sortedChildren(t, idOf(t, "d"), ~showIgnored=true);
      assertRows(
        t,
        shown,
        ["d/skip.bin", "d/keep.bin"],
        "revealed, and sorted with everything else",
      );
    });

    Test.run("keeps other-device and hard-link entries either way", () => {
      /* Those two are excluded from RANKING because their bytes are not
         reclaimable here, but a zero-sized row that explains itself is
         exactly what the user needs when they open the directory. */
      let t =
        build([
          D(
            "d",
            [
              Fx("mounted", 0, Store.fOtherDev),
              Fx("clone.bin", 0, Store.fHardLink),
              F("real.bin", mb(10)),
            ],
          ),
        ]);
      let rows = Rank.sortedChildren(t, idOf(t, "d"), ~showIgnored=false);
      assertRows(
        t,
        rows,
        ["d/real.bin", "d/clone.bin", "d/mounted"],
        "all three are listed",
      );
    });

    Test.run("a leaf has no children", () => {
      let t = build([F("f.bin", gb(1))]);
      let rows = Rank.sortedChildren(t, idOf(t, "f.bin"), ~showIgnored=true);
      Test.assertEqualInt(Array.length(rows), 0, "a file lists nothing");
    });
  });
};
