/*
 * Tests for lib/Walk.re, against real directory trees built by Fixture.
 *
 * These are the tests that cannot be replaced by pure ones: every case in
 * here is about what the OPERATING SYSTEM reports, and each of the four big
 * ones guards a failure that is silent rather than loud -
 *
 *   a symlink to an ancestor      -> a walk that never terminates
 *   a hard link counted twice     -> a total larger than the disk
 *   an unreadable directory       -> a scan that stops at the first EACCES
 *   an ignored directory descended-> the entire pruning win, gone
 *
 * File sizes are exact and distinct so that every assertion below is about
 * arithmetic rather than about which of two equal numbers won.
 */

let home = "/Users/tester";

/* The default for cases that are not about ignoring. */
let ignoreNothing = (~name as _: string, ~path as _: string) => false;

/* Walk a tree and hand back everything a case might want to assert on. */
let walk =
    (
      ~shouldIgnore=ignoreNothing,
      ~options=Walk.defaultOptions,
      root: string,
    )
    : (Store.t, Walk.stats) => {
  let store = Walk.makeStore(~root, ~home);
  let stats = Walk.makeStats();
  Walk.run(~store, ~stats, ~shouldIgnore, ~options, ());
  (store, stats);
};

let sizeOf = (store, root, rel) =>
  switch (Store.resolve(store, Filename.concat(root, rel))) {
  | Some(id) => Store.get(store, id).Store.size
  | None => raise(Test.AssertionFailed("no node for " ++ rel))
  };

let nodeAt = (store, root, rel) =>
  switch (Store.resolve(store, Filename.concat(root, rel))) {
  | Some(id) => Store.get(store, id)
  | None => raise(Test.AssertionFailed("no node for " ++ rel))
  };

/* The tree the size and count cases share. Distinct sizes, and nothing is a
 * round number, so an off-by-one directory shows up as a wrong total rather
 * than as a coincidence. */
let sizedTree = [
  ("a/f1.txt", 100),
  ("a/b/f2.txt", 250),
  ("a/b/f3.txt", 3),
  ("c/f4.txt", 7000),
  ("d/", 0),
  ("top.txt", 11),
];

let sizedTotal = 100 + 250 + 3 + 7000 + 11;

let run = () => {
  Test.group("walk: sizes", () => {
    Test.run("recursive sizes are exact at every level", () =>
      Fixture.withTree(sizedTree, root => {
        let (store, _) = walk(root);
        Test.assertEqualInt(sizeOf(store, root, "a/b"), 253, "a/b = 250 + 3");
        Test.assertEqualInt(sizeOf(store, root, "a"), 353, "a = 100 + a/b");
        Test.assertEqualInt(sizeOf(store, root, "c"), 7000, "c = f4");
        Test.assertEqualInt(sizeOf(store, root, "d"), 0, "d is empty");
        Test.assertEqualInt(sizeOf(store, root, "top.txt"), 11, "a loose file");
        Test.assertEqualInt(Store.total(store), sizedTotal, "the root total");
      })
    );

    Test.run("item counts include directories", () =>
      Fixture.withTree(sizedTree, root => {
        let (store, stats) = walk(root);
        /* a, a/f1, a/b, a/b/f2, a/b/f3, c, c/f4, d, top */
        Test.assertEqualInt(
          Store.get(store, Store.rootId).Store.items,
          9,
          "every entry underneath the root",
        );
        Test.assertEqualInt(nodeAt(store, root, "a").Store.items, 4, "a holds four");
        Test.assertEqualInt(stats.Walk.files, 5, "five files");
        Test.assertEqualInt(stats.Walk.dirs, 4, "four directories");
      })
    );

    Test.run("a zero-byte file still gets a node and an item", () =>
      Fixture.withTree([("empty.bin", 0), ("other.bin", 40)], root => {
        let (store, _) = walk(root);
        let n = nodeAt(store, root, "empty.bin");
        Test.assertEqualInt(n.Store.size, 0, "no bytes");
        Test.assertTrue(n.Store.kind == Store.File, "still a file");
        Test.assertEqualInt(Store.total(store), 40, "it adds nothing");
        Test.assertEqualInt(
          Store.get(store, Store.rootId).Store.items,
          2,
          "but it is still an item",
        );
      })
    );

    Test.run("an empty directory has no children and no size", () =>
      Fixture.withTree([("nothing/", 0)], root => {
        let (store, _) = walk(root);
        let n = nodeAt(store, root, "nothing");
        Test.assertTrue(n.Store.kind == Store.Dir, "a directory");
        Test.assertEqualInt(n.Store.childCount, 0, "no children");
        Test.assertEqualInt(n.Store.size, 0, "no size of its own");
        Test.assertEqualInt(n.Store.flags, 0, "and no flags: it was readable");
      })
    );

    Test.run("an empty root walks to zero rather than failing", () =>
      Fixture.withTmp(root => {
        let (store, stats) = walk(root);
        Test.assertEqualInt(store.Store.count, 1, "only the root node");
        Test.assertEqualInt(Store.total(store), 0, "nothing in it");
        Test.assertEqualInt(stats.Walk.errorCount, 0, "and no errors");
      })
    );

    Test.run("children occupy one contiguous range per directory", () =>
      Fixture.withTree(sizedTree, root => {
        let (store, _) = walk(root);
        /* Every node is a child of exactly one range. Checking it globally is
           what catches a mid-directory allocation that the per-directory
           assertion inside Walk somehow let through. */
        let owner = Array.make(store.Store.count, Store.noId);
        for (id in 0 to store.Store.count - 1) {
          Store.iterChildren(store, id, child => {
            Test.assertEqualInt(
              owner[child],
              Store.noId,
              "no node appears in two child ranges",
            );
            owner[child] = id;
            Test.assertEqualInt(
              Store.get(store, child).Store.parent,
              id,
              "the range agrees with the parent link",
            );
          });
        };
        for (id in 1 to store.Store.count - 1) {
          Test.assertFalse(owner[id] == Store.noId, "every node has an owner");
        };
      })
    );
  });

  Test.group("walk: symlinks", () => {
    Test.run("a symlink to its own ancestor terminates and is not descended", () =>
      Fixture.withTree([("real/f.txt", 500)], root => {
        let (before, _) = walk(root);
        let baseTotal = Store.total(before);

        /* Pointing at the root itself: with stat instead of lstat this walk
           never returns. Reaching the assertions below IS the test. */
        let link = Filename.concat(root, "loop");
        Fixture.symlink(~target=root, ~link);

        let (store, _) = walk(root);
        let n = nodeAt(store, root, "loop");
        Test.assertTrue(n.Store.kind == Store.Link, "kind is Link");
        Test.assertEqualInt(n.Store.childCount, 0, "never opened");
        Test.assertEqualInt(
          n.Store.size,
          String.length(root),
          "a symlink's own size is the length of its target",
        );
        Test.assertEqualInt(
          Store.total(store),
          baseTotal + String.length(root),
          "the tree behind the link is not counted again",
        );
      })
    );

    Test.run("a symlink to a big file is counted as the link, not the file", () =>
      Fixture.withTree([("big.bin", 9000)], root => {
        Fixture.symlink(
          ~target=Filename.concat(root, "big.bin"),
          ~link=Filename.concat(root, "shortcut"),
        );
        let (store, _) = walk(root);
        let n = nodeAt(store, root, "shortcut");
        Test.assertTrue(n.Store.size < 9000, "not the size of its target");
        Test.assertEqualInt(
          Store.total(store),
          9000 + n.Store.size,
          "the target's bytes are counted exactly once",
        );
      })
    );

    Test.run("a dangling symlink is a node, not an error", () =>
      Fixture.withTree([("keep.bin", 20)], root => {
        Fixture.symlink(
          ~target=Filename.concat(root, "does-not-exist"),
          ~link=Filename.concat(root, "dangling"),
        );
        let (store, stats) = walk(root);
        let n = nodeAt(store, root, "dangling");
        Test.assertTrue(n.Store.kind == Store.Link, "still a link");
        Test.assertEqualInt(stats.Walk.errorCount, 0, "lstat succeeded");
        Test.assertEqualInt(stats.Walk.skipped, 0, "and nothing was skipped");
      })
    );
  });

  Test.group("walk: hard links", () => {
    Test.run("the second name for an inode contributes nothing", () =>
      Fixture.withTree([("h1.bin", 500), ("other.bin", 7)], root => {
        Fixture.hardlink(
          ~target=Filename.concat(root, "h1.bin"),
          ~link=Filename.concat(root, "h2.bin"),
        );
        let (store, _) = walk(root);
        /* Entries are walked in sorted order, so h1 is seen first. */
        let a = nodeAt(store, root, "h1.bin");
        let b = nodeAt(store, root, "h2.bin");
        Test.assertEqualInt(a.Store.size, 500, "the first name carries the bytes");
        Test.assertFalse(
          Store.hasFlag(a.Store.flags, Store.fHardLink),
          "and is not flagged",
        );
        Test.assertEqualInt(b.Store.size, 0, "the second name is worth nothing");
        Test.assertTrue(
          Store.hasFlag(b.Store.flags, Store.fHardLink),
          "and says so with fHardLink",
        );
        Test.assertEqualInt(Store.total(store), 507, "counted exactly once");
      })
    );

    Test.run("a hard link in another directory is deduped too", () =>
      Fixture.withTree([("a/h.bin", 800), ("b/", 0)], root => {
        Fixture.hardlink(
          ~target=Filename.concat(root, "a/h.bin"),
          ~link=Filename.concat(root, "b/copy.bin"),
        );
        let (store, _) = walk(root);
        Test.assertEqualInt(sizeOf(store, root, "a"), 800, "a keeps the bytes");
        Test.assertEqualInt(sizeOf(store, root, "b"), 0, "b gets none of them");
        Test.assertEqualInt(Store.total(store), 800, "and the root is not doubled");
      })
    );

    Test.run("directories are never treated as hard links", () =>
      /* Every directory on macOS has st_nlink >= 2, so a dedup guarded on
         nlink alone would eventually match a previously seen (dev, ino) and
         zero a whole subtree. Two sibling directories with content is the
         cheapest tree that would notice. */
      Fixture.withTree(
        [("one/x.bin", 111), ("two/y.bin", 222), ("two/z/w.bin", 333)],
        root => {
          let (store, _) = walk(root);
          Test.assertEqualInt(sizeOf(store, root, "one"), 111, "one");
          Test.assertEqualInt(sizeOf(store, root, "two"), 555, "two");
          Test.assertEqualInt(Store.total(store), 666, "nothing was zeroed");
          Test.assertFalse(
            Store.hasFlag(nodeAt(store, root, "two").Store.flags, Store.fHardLink),
            "no directory is flagged as a hard link",
          );
        },
      )
    );
  });

  Test.group("walk: unreadable directories", () => {
    Test.run("an unreadable directory does not abort the rest of the scan", () =>
      Fixture.withTree(
        [("locked/hidden.bin", 4000), ("open/visible.bin", 1234), ("loose.bin", 9)],
        root => {
          let locked = Filename.concat(root, "locked");
          Fixture.withUnreadable([locked], () => {
            let (store, stats) = walk(root);
            let n = nodeAt(store, root, "locked");
            Test.assertTrue(
              Store.hasFlag(n.Store.flags, Store.fUnreadable),
              "the directory is flagged",
            );
            Test.assertTrue(n.Store.kind == Store.Dir, "and still exists as a node");
            Test.assertEqualInt(n.Store.childCount, 0, "with no children");
            Test.assertEqualInt(stats.Walk.unreadable, 1, "counted once");

            /* The whole point: the sibling subtree still has its real size. */
            Test.assertEqualInt(
              sizeOf(store, root, "open"),
              1234,
              "the sibling subtree was still walked",
            );
            Test.assertEqualInt(sizeOf(store, root, "loose.bin"), 9, "and so was the loose file");
            Test.assertEqualInt(
              Store.total(store),
              1243,
              "the total is a lower bound, not a failure",
            );
          });
        },
      )
    );

    Test.run("an unreadable directory appends one bounded error", () =>
      Fixture.withTree([("locked/x.bin", 10)], root => {
        let locked = Filename.concat(root, "locked");
        Fixture.withUnreadable([locked], () => {
          let (_, stats) = walk(root);
          Test.assertEqualInt(stats.Walk.errorCount, 1, "one error");
          Test.assertEqualInt(List.length(Walk.errorList(stats)), 1, "one entry");
          switch (Walk.errorList(stats)) {
          | [(p, _msg)] => Test.assertEqualStr(p, locked, "named the directory")
          | _ => Test.assertTrue(false, "expected exactly one error")
          };
          /* ENOENT has its own counter; a permission failure must not land
             in it, or the UI cannot tell "files churned" from "I am lying
             about your Library folder". */
          Test.assertEqualInt(stats.Walk.skipped, 0, "nothing was skipped");
        });
      })
    );

    Test.run("the error list is bounded while the count is not", () => {
      /* Walk.errorLimit + 10 unreadable directories: the list stops growing,
         the counter does not, and the UI can still say how bad it is. */
      let n = Walk.errorLimit + 10;
      let spec = List.init(n, i => (Printf.sprintf("d%03d/", i), 0));
      Fixture.withTree(spec, root => {
        let dirs = List.init(n, i => Filename.concat(root, Printf.sprintf("d%03d", i)));
        Fixture.withUnreadable(dirs, () => {
          let (_, stats) = walk(root);
          Test.assertEqualInt(stats.Walk.unreadable, n, "every one was counted");
          Test.assertEqualInt(stats.Walk.errorCount, n, "every one raised an error");
          Test.assertEqualInt(
            List.length(Walk.errorList(stats)),
            Walk.errorLimit,
            "but the list is capped",
          );
        });
      });
    });
  });

  Test.group("walk: ignoring", () => {
    Test.run("an ignored directory is flagged, empty, and worth nothing", () =>
      Fixture.withTree(
        [("skipme/huge.bin", 50000), ("keep/small.bin", 60)],
        root => {
          let shouldIgnore = (~name: string, ~path as _: string) => name == "skipme";
          let (store, _) = walk(~shouldIgnore, root);
          let n = nodeAt(store, root, "skipme");
          Test.assertTrue(
            Store.hasFlag(n.Store.flags, Store.fIgnored),
            "flagged as ignored",
          );
          Test.assertEqualInt(n.Store.childCount, 0, "never opened");
          Test.assertEqualInt(n.Store.size, 0, "no size, because we never looked");
          Test.assertEqualInt(Store.total(store), 60, "only the kept subtree counts");
        },
      )
    );

    Test.run("nothing inside an ignored directory is ever asked about", () =>
      Fixture.withTree(
        [("skipme/a/b/deep.bin", 90000), ("keep/small.bin", 60)],
        root => {
          /* Recording what the rules were consulted about is the only direct
             evidence that pruning happened at ENTRY CREATION rather than
             after the descend - a walk that opened the directory and then
             discarded the result would pass every size assertion above. */
          let asked = ref([]);
          let shouldIgnore = (~name: string, ~path: string) => {
            asked := [path, ...asked^];
            name == "skipme";
          };
          let (store, _) = walk(~shouldIgnore, root);
          let inside = Filename.concat(root, "skipme") ++ "/";
          List.iter(
            p =>
              Test.assertFalse(
                Test.contains(p, inside),
                "the rules were asked about " ++ p ++ ", which is inside the ignored tree",
              ),
            asked^,
          );
          Test.assertTrue(
            List.mem(Filename.concat(root, "skipme"), asked^),
            "the ignored directory itself was asked about",
          );
          Test.assertEqualInt(
            Store.get(store, Store.rootId).Store.items,
            3,
            "skipme, keep and keep/small.bin - and nothing under skipme",
          );
        },
      )
    );

    Test.run("the rules see the basename and the absolute path", () =>
      Fixture.withTree([("a/target.bin", 5)], root => {
        let seen = ref([]);
        let shouldIgnore = (~name: string, ~path: string) => {
          seen := [(name, path), ...seen^];
          false;
        };
        ignore(walk(~shouldIgnore, root));
        Test.assertTrue(
          List.mem(("target.bin", Filename.concat(root, "a/target.bin")), seen^),
          "name is the basename and path is absolute",
        );
      })
    );

    Test.run("an ignored file keeps the size we already know", () =>
      /* A file cannot be descended, so ignoring one costs nothing and there
         is no reason to throw away a number we were handed. It is excluded
         from the ranking by its flag, not by lying about its size. */
      Fixture.withTree([("noise.log", 700), ("keep.bin", 3)], root => {
        let shouldIgnore = (~name: string, ~path as _: string) => name == "noise.log";
        let (store, _) = walk(~shouldIgnore, root);
        let n = nodeAt(store, root, "noise.log");
        Test.assertTrue(Store.hasFlag(n.Store.flags, Store.fIgnored), "flagged");
        Test.assertEqualInt(n.Store.size, 700, "size intact");
      })
    );
  });

  Test.group("walk: limits and control", () => {
    Test.run("descent stops at the depth cap", () => {
      /* Deeper than the cap, with a file at the bottom that must therefore
         never be reached. */
      let depth = Walk.maxDepth + 6;
      let deep = String.concat("/", List.init(depth, i => Printf.sprintf("l%d", i)));
      Fixture.withTree([(deep ++ "/bottom.bin", 42)], root => {
        let (store, _) = walk(root);
        let deepest = ref(0);
        for (id in 0 to store.Store.count - 1) {
          deepest := max(deepest^, Store.get(store, id).Store.depth);
        };
        Test.assertEqualInt(deepest^, Walk.maxDepth, "nothing past the cap");
        Test.assertEqualInt(
          Store.total(store),
          0,
          "the file below the cap was never reached",
        );
      });
    });

    Test.run("onDir fires once per directory that was visited", () =>
      Fixture.withTree(sizedTree, root => {
        let calls = ref(0);
        let options = {...Walk.defaultOptions, onDir: () => incr(calls)};
        ignore(walk(~options, root));
        /* root, a, a/b, c, d */
        Test.assertEqualInt(calls^, 5, "one call per directory");
      })
    );

    Test.run("cancel stops the walk and leaves the arena consistent", () =>
      Fixture.withTree(sizedTree, root => {
        let options = {...Walk.defaultOptions, cancel: () => true};
        let (store, _) = walk(~options, root);
        /* Cancelled after the very first directory: the root's own children
           exist, nothing below them does, and every parent link still
           resolves. */
        Test.assertEqualInt(
          Store.get(store, Store.rootId).Store.childCount,
          4,
          "the root's entries were still allocated",
        );
        Test.assertEqualInt(sizeOf(store, root, "a"), 0, "but nothing below was walked");
        Test.assertEqualInt(Store.total(store), 11, "only the loose file counted");
      })
    );

    /* The two pure predicates no fixture can reach: nothing is going to
       write a four-gigabyte file to make a test pass. */
    Test.run("fSparseSuspect needs both a big size and an image extension", () => {
      let big = Walk.sparseThreshold + 1;
      Test.assertTrue(
        Walk.looksSparse(~kind=Store.File, ~size=big, ~name="Docker.raw"),
        "a big .raw is suspect",
      );
      Test.assertTrue(
        Walk.looksSparse(~kind=Store.File, ~size=big, ~name="Win11.QCOW2"),
        "the extension test is case-insensitive",
      );
      Test.assertFalse(
        Walk.looksSparse(~kind=Store.File, ~size=big, ~name="movie.mp4"),
        "a big ordinary file is not suspect",
      );
      Test.assertFalse(
        Walk.looksSparse(~kind=Store.File, ~size=1000, ~name="tiny.raw"),
        "a small image file is not worth a warning",
      );
      Test.assertFalse(
        Walk.looksSparse(~kind=Store.Dir, ~size=big, ~name="thing.dmg"),
        "and a directory is never suspect, whatever it is called",
      );
    });

    Test.run("an int64 size is clamped into the arena's int", () => {
      Test.assertEqualInt(Walk.bytesOfInt64(0L), 0, "zero");
      Test.assertEqualInt(Walk.bytesOfInt64(4096L), 4096, "an ordinary size");
      /* Neither can happen on a real filesystem; both would become a
         negative total, which the ranking would sort to the top. */
      Test.assertEqualInt(Walk.bytesOfInt64(-1L), 0, "negative clamps to zero");
      Test.assertEqualInt(
        Walk.bytesOfInt64(Int64.max_int),
        max_int,
        "past 63 bits clamps to max_int",
      );
    });

    Test.run("the store's root path is the path it was given", () =>
      Fixture.withTree([("x.bin", 1)], root => {
        let (store, _) = walk(root);
        Test.assertEqualStr(
          Store.path(store, Store.rootId),
          root,
          "no canonicalization behind the caller's back",
        );
        Test.assertTrue(
          Store.resolve(store, Filename.concat(root, "x.bin")) != None,
          "and resolve agrees with it",
        );
      })
    );
  });
};
