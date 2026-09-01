/*
 * Tests for lib/Store.re - the node arena. No filesystem: every tree in here
 * is built by hand, which is the point of keeping Store pure.
 *
 * Two of these guard failure modes that are silent rather than loud:
 *
 *   - the contiguous child range. iterChildren visiting one node too few or
 *     one too many does not crash, it just quietly loses or steals a
 *     subtree, so the range is asserted against explicitly known ids.
 *
 *   - isAncestor. Implemented as a string prefix test it would answer "yes"
 *     for a sibling called "Lib" against "Library", and every scope check in
 *     the UI would be subtly wrong on exactly the paths people have.
 */

let home = "/Users/tester";
let root = home ++ "/scan";

let store = () => Store.create(~root, ~home, ());

/* Add a file and propagate it, which is what Walk does for every entry. */
let addFile = (t, ~parent, ~name, ~size) => {
  let id = Store.alloc(t, ~name, ~parent, ~kind=Store.File, ~size, ());
  Store.addUp(t, id, ~bytes=size, ~items=1);
  id;
};

/* Add a directory the way Walk does: zero size of its own, one item in each
 * ancestor's count. Children are attached by the caller. */
let addDir = (t, ~parent, ~name) => {
  let id = Store.alloc(t, ~name, ~parent, ~kind=Store.Dir, ());
  Store.addUp(t, id, ~bytes=0, ~items=1);
  id;
};

let run = () => {
  Test.group("store: arena", () => {
    Test.run("create allocates the root node", () => {
      let t = store();
      Test.assertEqualInt(t.Store.count, 1, "node count");
      let r = Store.get(t, Store.rootId);
      Test.assertTrue(r.Store.kind == Store.Dir, "root is a directory");
      Test.assertEqualInt(r.Store.depth, 0, "root depth");
      Test.assertEqualInt(r.Store.parent, Store.noId, "root has no parent");
      Test.assertEqualInt(Store.total(t), 0, "empty root total");
    });

    Test.run("alloc returns dense ids and derives depth from the parent", () => {
      let t = store();
      let a = addDir(t, ~parent=Store.rootId, ~name="a");
      let b = addDir(t, ~parent=a, ~name="b");
      let c = addFile(t, ~parent=b, ~name="c", ~size=1);
      Test.assertEqualInt(a, 1, "first child id");
      Test.assertEqualInt(b, 2, "second id");
      Test.assertEqualInt(c, 3, "third id");
      Test.assertEqualInt(Store.get(t, a).Store.depth, 1, "depth of a");
      Test.assertEqualInt(Store.get(t, b).Store.depth, 2, "depth of b");
      Test.assertEqualInt(Store.get(t, c).Store.depth, 3, "depth of c");
    });

    Test.run("the arena grows past its initial capacity", () => {
      /* Capacity 2 means the very first alloc already has to reallocate, so
         a blit that dropped or duplicated a node shows up immediately. */
      let t = Store.create(~root, ~home, ~capacity=2, ());
      let ids = Array.init(100, i => addFile(t, ~parent=Store.rootId, ~name="f" ++ string_of_int(i), ~size=i + 1));
      Test.assertEqualInt(t.Store.count, 101, "root plus 100 files");
      Array.iteri(
        (i, id) =>
          Test.assertEqualStr(
            Store.get(t, id).Store.name,
            "f" ++ string_of_int(i),
            "name survived the growth",
          ),
        ids,
      );
      /* 1 + 2 + ... + 100 */
      Test.assertEqualInt(Store.total(t), 5050, "total after growth");
    });

    Test.run("get rejects an id outside the arena", () => {
      let t = store();
      let bad =
        switch (Store.get(t, 7)) {
        | _ => false
        | exception (Invalid_argument(_)) => true
        };
      Test.assertTrue(bad, "out of range id raises");
    });

    Test.run("the flag constants are distinct single bits", () => {
      let all = [
        Store.fIgnored,
        Store.fUnreadable,
        Store.fHardLink,
        Store.fOtherDev,
        Store.fSparseSuspect,
      ];
      let combined = List.fold_left((acc, f) => acc lor f, 0, all);
      let sum = List.fold_left((acc, f) => acc + f, 0, all);
      Test.assertEqualInt(combined, sum, "no two flags share a bit");
      Test.assertTrue(
        Store.hasFlag(Store.fIgnored lor Store.fHardLink, Store.fHardLink),
        "hasFlag finds a set bit",
      );
      Test.assertFalse(
        Store.hasFlag(Store.fIgnored lor Store.fHardLink, Store.fOtherDev),
        "hasFlag rejects an unset bit",
      );
    });

    Test.run("childLimit is a usable positive bound", () =>
      Test.assertTrue(Store.childLimit > 1000, "childLimit is positive and large")
    );
  });

  Test.group("store: propagation", () => {
    Test.run("addUp makes every ancestor the sum of its descendants", () => {
      let t = store();
      let a = addDir(t, ~parent=Store.rootId, ~name="a");
      let b = addDir(t, ~parent=a, ~name="b");
      let c = addDir(t, ~parent=Store.rootId, ~name="c");
      ignore(addFile(t, ~parent=a, ~name="f1", ~size=100));
      ignore(addFile(t, ~parent=b, ~name="f2", ~size=250));
      ignore(addFile(t, ~parent=b, ~name="f3", ~size=3));
      ignore(addFile(t, ~parent=c, ~name="f4", ~size=7000));
      Test.assertEqualInt(Store.get(t, b).Store.size, 253, "b = 250 + 3");
      Test.assertEqualInt(Store.get(t, a).Store.size, 353, "a = 100 + b");
      Test.assertEqualInt(Store.get(t, c).Store.size, 7000, "c = f4");
      Test.assertEqualInt(Store.total(t), 7353, "root = a + c");
    });

    Test.run("a directory contributes no bytes of its own", () => {
      let t = store();
      let a = addDir(t, ~parent=Store.rootId, ~name="a");
      Test.assertEqualInt(Store.get(t, a).Store.size, 0, "empty dir");
      Test.assertEqualInt(Store.total(t), 0, "root unchanged by a dir");
    });

    Test.run("items counts every descendant entry, not just files", () => {
      let t = store();
      let a = addDir(t, ~parent=Store.rootId, ~name="a");
      let b = addDir(t, ~parent=a, ~name="b");
      ignore(addFile(t, ~parent=b, ~name="f1", ~size=1));
      ignore(addFile(t, ~parent=b, ~name="f2", ~size=2));
      Test.assertEqualInt(Store.get(t, b).Store.items, 2, "b holds two files");
      Test.assertEqualInt(Store.get(t, a).Store.items, 3, "a holds b and its two files");
      Test.assertEqualInt(
        Store.get(t, Store.rootId).Store.items,
        4,
        "root holds everything below it",
      );
    });
  });

  Test.group("store: child ranges", () => {
    Test.run("iterChildren visits exactly the contiguous range", () => {
      let t = store();
      let a = addDir(t, ~parent=Store.rootId, ~name="a");
      /* Allocate a's children in one uninterrupted run, exactly as Walk
         does, then hand the range over. */
      let first = t.Store.count;
      let x = addFile(t, ~parent=a, ~name="x", ~size=1);
      let y = addFile(t, ~parent=a, ~name="y", ~size=2);
      let z = addFile(t, ~parent=a, ~name="z", ~size=4);
      Store.setChildRange(t, a, ~first, ~count=3);
      /* And a sibling allocated AFTER the range, which must not be visited. */
      let outside = addFile(t, ~parent=Store.rootId, ~name="outside", ~size=8);
      Store.setChildRange(t, Store.rootId, ~first=a, ~count=1);

      let seen = ref([]);
      Store.iterChildren(t, a, id => seen := [id, ...seen^]);
      Test.assertTrue(List.rev(seen^) == [x, y, z], "a's children, in order");
      Test.assertFalse(List.mem(outside, seen^), "the later sibling is not a's child");
    });

    Test.run("iterChildren visits nothing for a leaf", () => {
      let t = store();
      let f = addFile(t, ~parent=Store.rootId, ~name="f", ~size=1);
      let n = ref(0);
      Store.iterChildren(t, f, _ => incr(n));
      Test.assertEqualInt(n^, 0, "a file has no children");
    });

    Test.run("setChildRange with count 0 clears childFirst", () => {
      let t = store();
      let a = addDir(t, ~parent=Store.rootId, ~name="a");
      Store.setChildRange(t, a, ~first=99, ~count=0);
      Test.assertEqualInt(Store.get(t, a).Store.childCount, 0, "no children");
      Test.assertEqualInt(
        Store.get(t, a).Store.childFirst,
        Store.noId,
        "an empty range never points anywhere",
      );
    });
  });

  Test.group("store: paths", () => {
    Test.run("the root's path is the root string itself", () => {
      let t = store();
      Test.assertEqualStr(Store.path(t, Store.rootId), root, "root path");
      Test.assertEqualStr(
        Store.displayPath(t, Store.rootId),
        "~/scan",
        "root display path",
      );
    });

    Test.run("a nested path joins the basenames on the way up", () => {
      let t = store();
      let a = addDir(t, ~parent=Store.rootId, ~name="Library");
      let b = addDir(t, ~parent=a, ~name="Caches");
      let c = addFile(t, ~parent=b, ~name="big.bin", ~size=10);
      Test.assertEqualStr(
        Store.path(t, c),
        root ++ "/Library/Caches/big.bin",
        "nested path",
      );
      Test.assertEqualStr(
        Store.displayPath(t, c),
        "~/scan/Library/Caches/big.bin",
        "nested display path",
      );
    });

    Test.run("a root of \"/\" does not produce a doubled slash", () => {
      let t = Store.create(~root="/", ~home, ());
      let a = addDir(t, ~parent=Store.rootId, ~name="private");
      let b = addFile(t, ~parent=a, ~name="swap", ~size=1);
      Test.assertEqualStr(Store.path(t, a), "/private", "first level under /");
      Test.assertEqualStr(Store.path(t, b), "/private/swap", "second level under /");
    });

    Test.run("displayPath leaves a path outside home alone", () => {
      let t = Store.create(~root="/opt/data", ~home, ());
      let a = addDir(t, ~parent=Store.rootId, ~name="cache");
      Test.assertEqualStr(
        Store.displayPath(t, a),
        "/opt/data/cache",
        "no tilde outside home",
      );
    });
  });

  Test.group("store: resolve and isAncestor", () => {
    /* One tree reused by the cases below:
     *
     *   root/Library/Caches/big.bin   900 bytes
     *   root/Lib/small.bin              5 bytes
     *
     * Built the way Walk builds it - one directory's entries allocated in a
     * single uninterrupted run, then setChildRange - so the contiguous-range
     * invariant holds here too and resolve is exercised against a realistic
     * arena rather than a convenient one. */
    let build = () => {
      let t = store();

      let first = t.Store.count;
      let library = addDir(t, ~parent=Store.rootId, ~name="Library");
      let lib = addDir(t, ~parent=Store.rootId, ~name="Lib");
      Store.setChildRange(t, Store.rootId, ~first, ~count=2);

      let caches = addDir(t, ~parent=library, ~name="Caches");
      Store.setChildRange(t, library, ~first=caches, ~count=1);

      let small = addFile(t, ~parent=lib, ~name="small.bin", ~size=5);
      Store.setChildRange(t, lib, ~first=small, ~count=1);

      let big = addFile(t, ~parent=caches, ~name="big.bin", ~size=900);
      Store.setChildRange(t, caches, ~first=big, ~count=1);

      (t, library, caches, big, lib, small);
    };

    Test.run("resolve finds a nested path", () => {
      let (t, library, caches, big, lib, small) = build();
      Test.assertTrue(
        Store.resolve(t, root ++ "/Library") == Some(library),
        "one level",
      );
      Test.assertTrue(
        Store.resolve(t, root ++ "/Library/Caches") == Some(caches),
        "two levels",
      );
      Test.assertTrue(
        Store.resolve(t, root ++ "/Library/Caches/big.bin") == Some(big),
        "three levels",
      );
      /* The second child of the root, which only a correct range reaches. */
      Test.assertTrue(Store.resolve(t, root ++ "/Lib") == Some(lib), "sibling");
      Test.assertTrue(
        Store.resolve(t, root ++ "/Lib/small.bin") == Some(small),
        "under the sibling",
      );
    });

    Test.run("resolve answers Some for the root itself", () => {
      let (t, _, _, _, _, _) = build();
      Test.assertTrue(Store.resolve(t, root) == Some(Store.rootId), "the root path");
      Test.assertTrue(
        Store.resolve(t, root ++ "/") == Some(Store.rootId),
        "a trailing slash is not a segment",
      );
    });

    Test.run("resolve returns None outside the tree", () => {
      let (t, _, _, _, _, _) = build();
      Test.assertTrue(Store.resolve(t, "/etc/passwd") == None, "another tree");
      /* The prefix trap: "/Users/tester/scanner" starts with the root string
         but is not underneath it. */
      Test.assertTrue(
        Store.resolve(t, root ++ "ner/thing") == None,
        "a sibling whose name extends the root",
      );
      Test.assertTrue(
        Store.resolve(t, root ++ "/Library/Nope") == None,
        "a name that was never scanned",
      );
    });

    Test.run("isAncestor walks parent links, never string prefixes", () => {
      let (t, library, caches, big, lib, small) = build();
      Test.assertTrue(
        Store.isAncestor(t, ~ancestor=library, big),
        "a grandparent is an ancestor",
      );
      Test.assertTrue(
        Store.isAncestor(t, ~ancestor=Store.rootId, big),
        "the root is an ancestor of everything",
      );
      Test.assertFalse(
        Store.isAncestor(t, ~ancestor=big, big),
        "a node is not its own ancestor",
      );
      Test.assertFalse(
        Store.isAncestor(t, ~ancestor=caches, library),
        "a child is not an ancestor of its parent",
      );
      /* The one that a prefix test gets wrong: "Lib" is a prefix of
         "Library", so small.bin under Lib would look like it lived under
         Library. */
      Test.assertFalse(
        Store.isAncestor(t, ~ancestor=library, small),
        "a sibling whose name prefixes another is not an ancestor",
      );
      Test.assertFalse(
        Store.isAncestor(t, ~ancestor=lib, big),
        "and not the other way round either",
      );
    });
  });
};
