/*
 * Tests for lib/Scan.re.
 *
 * Scan is the only module in hog that runs anything on a second thread, so
 * these tests are as much about DETERMINISM as about behaviour. Two rules
 * govern the whole file:
 *
 *   1. Nothing here sleeps. A test that waits a fixed number of
 *      milliseconds for a background thread is a flake waiting for a slow
 *      machine. `stepped` hands control back and forth explicitly, so
 *      every assertion below runs while the walk is parked and touching
 *      nothing.
 *
 *   2. There is exactly ONE test of the genuinely threaded path, and it
 *      waits on a bounded retry rather than a timer. Its only job is to
 *      prove that `start` and `scanSync` agree - if the two traversals
 *      ever diverge, every other test in this file is testing something
 *      the binary does not do.
 */

let home = "/Users/tester";

let ignoreNothing = (~name as _: string, ~path as _: string) => false;

/* A small tree with exact, distinct sizes, so every total below is
   arithmetic rather than a coincidence between two equal numbers. */
let tree = [
  ("big/a.bin", 400_000),
  ("big/b.bin", 300_000),
  ("small/c.bin", 5_000),
  ("loose.txt", 1_000),
];

let treeTotal = 400_000 + 300_000 + 5_000 + 1_000;

let sizeAt = (store, root, rel) =>
  switch (Store.resolve(store, Filename.concat(root, rel))) {
  | Some(id) => Store.get(store, id).Store.size
  | None => raise(Test.AssertionFailed("no node for " ++ rel))
  };

let run = () =>
  Test.group("scan: synchronous", () => {
    Test.run("scanSync finishes and totals the tree", () =>
      Fixture.withTree(tree, root => {
        let h =
          Scan.scanSync(~root, ~home, ~shouldIgnore=ignoreNothing, ());
        let snap = Scan.published(h);
        Test.assertTrue(snap.Scan.phase == Scan.Done, "phase is Done");
        Test.assertEqualInt(
          Store.total(Scan.store(h)),
          treeTotal,
          "root total",
        );
        Test.assertEqualInt(
          sizeAt(Scan.store(h), root, "big"),
          700_000,
          "big/ totals its two files",
        );
        Test.assertFalse(Scan.isScanning(h), "not scanning any more");
      })
    );

    Test.run("the final snapshot reports progress", () =>
      Fixture.withTree(tree, root => {
        let h = Scan.scanSync(~root, ~home, ~shouldIgnore=ignoreNothing, ());
        let p = Scan.published(h).Scan.progress;
        Test.assertEqualInt(p.Scan.files, 4, "four files");
        Test.assertEqualInt(p.Scan.bytes, treeTotal, "bytes match the total");
        Test.assertTrue(p.Scan.finishedAt != None, "a finish time is recorded");
      })
    );

    Test.run("the generation advances past the seed", () =>
      Fixture.withTree(tree, root => {
        let h = Scan.scanSync(~root, ~home, ~shouldIgnore=ignoreNothing, ());
        Test.assertTrue(Scan.generation(h) > 0, "generation moved off zero");
        Test.assertEqualInt(
          Scan.published(h).Scan.gen,
          Scan.generation(h),
          "the snapshot's gen and the counter agree",
        );
      })
    );

    Test.run("nodeCount is a watermark into the arena", () =>
      Fixture.withTree(tree, root => {
        let h = Scan.scanSync(~root, ~home, ~shouldIgnore=ignoreNothing, ());
        let snap = Scan.published(h);
        Test.assertEqualInt(
          snap.Scan.nodeCount,
          Scan.store(h).Store.count,
          "watermark covers the whole arena once done",
        );
      })
    );

    Test.run("an exception in the walk becomes a visible Failed phase", () =>
      /* The single most likely way to ship a broken version of this program
         is an exception escaping the scan thread: it dies silently and the
         UI sits on "scanning..." forever. shouldIgnore is the easiest place
         to inject one, and it proves the try in `traverse` is real. */
      Fixture.withTree(tree, root => {
        let boom = (~name: string, ~path as _: string) =>
          if (name == "big") {
            failwith("injected");
          } else {
            false;
          };
        let h = Scan.scanSync(~root, ~home, ~shouldIgnore=boom, ());
        switch (Scan.published(h).Scan.phase) {
        | Scan.Failed(msg) =>
          Test.assertContains(msg, "injected", "the message survives")
        | _ => Test.assertTrue(false, "expected a Failed phase")
        };
        Test.assertFalse(Scan.isScanning(h), "a failed scan is not scanning");
      })
    );

    Test.run("ignored directories are pruned, not just hidden", () =>
      Fixture.withTree(tree, root => {
        let asked = ref([]);
        let skipBig = (~name: string, ~path: string) => {
          asked := [path, ...asked^];
          name == "big";
        };
        let h = Scan.scanSync(~root, ~home, ~shouldIgnore=skipBig, ());
        Test.assertEqualInt(
          Store.total(Scan.store(h)),
          treeTotal - 700_000,
          "the ignored subtree contributes nothing",
        );
        /* The pruning contract: we were never even asked about anything
           INSIDE big/, because it was never opened. */
        Test.assertFalse(
          List.exists(
            p => Test.contains(p, "/big/"),
            asked^,
          ),
          "nothing inside big/ was ever considered",
        );
      })
    );
  });

let runEntries = () =>
  Test.group("scan: entries and resolve", () => {
    Test.run("the landing view is the ranked frontier", () =>
      Fixture.withTree(tree, root => {
        let h = Scan.scanSync(~root, ~home, ~shouldIgnore=ignoreNothing, ());
        let landing =
          Scan.entries(
            h,
            ~snapshot=Scan.published(h),
            ~scope=None,
            ~showIgnored=false,
          );
        Test.assertTrue(
          landing == Scan.published(h).Scan.ranked,
          "scope=None is the published ranking",
        );
      })
    );

    Test.run("a scope gives that directory's children, largest first", () =>
      Fixture.withTree(tree, root => {
        let h = Scan.scanSync(~root, ~home, ~shouldIgnore=ignoreNothing, ());
        let kids =
          Scan.entries(
            h,
            ~snapshot=Scan.published(h),
            ~scope=Some(Filename.concat(root, "big")),
            ~showIgnored=false,
          );
        Test.assertEqualInt(Array.length(kids), 2, "two children");
        let store = Scan.store(h);
        Test.assertEqualStr(
          Store.get(store, kids[0]).Store.name,
          "a.bin",
          "the larger file comes first",
        );
      })
    );

    Test.run("an unknown scope is empty rather than an error", () =>
      Fixture.withTree(tree, root => {
        let h = Scan.scanSync(~root, ~home, ~shouldIgnore=ignoreNothing, ());
        Test.assertEqualInt(
          Array.length(
            Scan.entries(
              h,
              ~snapshot=Scan.published(h),
              ~scope=Some(root ++ "/nope"),
              ~showIgnored=false,
            ),
          ),
          0,
          "empty",
        );
      })
    );

    Test.run("applyIgnore subtracts the subtree from every ancestor", () =>
      /* This is what makes the `i` key honest. Hiding the row is easy; the
         part that is easy to get wrong is the header still counting the
         bytes the user just said they did not want to see. */
      Fixture.withTree(tree, root => {
        let h = Scan.scanSync(~root, ~home, ~shouldIgnore=ignoreNothing, ());
        let store = Scan.store(h);
        let gen = Scan.generation(h);
        Test.assertTrue(
          Scan.applyIgnore(h, ~path=Filename.concat(root, "big")),
          "applied",
        );
        Test.assertEqualInt(
          Store.total(store),
          treeTotal - 700_000,
          "the root total drops by exactly the subtree",
        );
        /* The item count has to fall too, and by the subtree's node count
           INCLUDING the node itself - `items` counts what is below a node,
           but every ancestor was charged one for the node as well. Asserting
           only the bytes lets that off-by-one through, which is exactly what
           happened the first time this was written. */
        Test.assertEqualInt(
          Store.get(store, Store.rootId).Store.items,
          4 + 2 - 3, /* four files and two dirs, less big/ and its two files */
          "and so does the item count",
        );
        Test.assertTrue(
          Scan.generation(h) > gen,
          "a new snapshot is published so the UI repaints",
        );
        Test.assertFalse(
          Array.exists(
            id => Store.get(store, id).Store.name == "big",
            Scan.published(h).Scan.ranked,
          ),
          "and it leaves the ranking",
        );
      })
    );

    Test.run("applyIgnore is idempotent, and unapplyIgnore reverses it", () =>
      /* Subtracting twice would drive the ancestors negative, which is why
         the second call has to be refused rather than merely wasteful. */
      Fixture.withTree(tree, root => {
        let h = Scan.scanSync(~root, ~home, ~shouldIgnore=ignoreNothing, ());
        let store = Scan.store(h);
        let p = Filename.concat(root, "big");
        Test.assertTrue(Scan.applyIgnore(h, ~path=p), "first call applies");
        Test.assertFalse(Scan.applyIgnore(h, ~path=p), "second call refuses");
        Test.assertEqualInt(
          Store.total(store),
          treeTotal - 700_000,
          "still subtracted exactly once",
        );
        Test.assertTrue(Scan.unapplyIgnore(h, ~path=p), "reversed");
        Test.assertEqualInt(
          Store.total(store),
          treeTotal,
          "and the total comes back",
        );
      })
    );

    Test.run("applyIgnore refuses while a scan is running", () =>
      /* The single-writer invariant: addUp is a read-modify-write over every
         ancestor, so running it from this thread while the walk runs its own
         would silently lose updates. Refusing is the whole point. */
      Fixture.withTree(tree, root => {
        let h = Scan.stepped(~root, ~home, ~shouldIgnore=ignoreNothing, ());
        Test.assertTrue(Scan.isScanning(h), "still scanning");
        Test.assertFalse(
          Scan.applyIgnore(h, ~path=Filename.concat(root, "big")),
          "refused while the walk owns the arena",
        );
        Scan.cancel(h);
        Scan.finish(h);
      })
    );

    Test.run("resolve round-trips a path the walk created", () =>
      Fixture.withTree(tree, root => {
        let h = Scan.scanSync(~root, ~home, ~shouldIgnore=ignoreNothing, ());
        switch (Scan.resolve(h, Filename.concat(root, "small/c.bin"))) {
        | None => Test.assertTrue(false, "expected to resolve")
        | Some(id) =>
          Test.assertEqualStr(
            Store.get(Scan.store(h), id).Store.name,
            "c.bin",
            "the right node",
          )
        };
        Test.assertTrue(
          Scan.resolve(h, "/definitely/not/here") == None,
          "an unrelated path does not resolve",
        );
      })
    );
  });

let runStepped = () =>
  Test.group("scan: stepped", () => {
    Test.run("a stepped handle starts parked, mid-scan", () =>
      Fixture.withTree(tree, root => {
        let h = Scan.stepped(~root, ~home, ~shouldIgnore=ignoreNothing, ());
        Test.assertTrue(Scan.isScanning(h), "still scanning");
        Test.assertTrue(Scan.generation(h) > 0, "the first directory published");
        Scan.finish(h);
      })
    );

    Test.run("each step publishes and the walk stays parked between them", () =>
      Fixture.withTree(tree, root => {
        let h = Scan.stepped(~root, ~home, ~shouldIgnore=ignoreNothing, ());
        /* The determinism assertion: read the arena twice around no step at
           all and it must be byte-identical. If the walk were running ahead
           of the caller - the bug this gate exists to prevent - these two
           reads would differ on a machine under load, occasionally, which
           is the worst kind of test failure. */
        let before = Store.total(Scan.store(h));
        let againstNoStep = Store.total(Scan.store(h));
        Test.assertEqualInt(before, againstNoStep, "nothing moves without a step");

        let genBefore = Scan.generation(h);
        Scan.step(h);
        Test.assertTrue(
          Scan.generation(h) > genBefore,
          "a step publishes at least one new snapshot",
        );
        /* Unpark the walk before the fixture deletes the tree underneath it,
           so the suite does not accumulate a blocked thread per test. */
        Scan.cancel(h);
        Scan.finish(h);
      })
    );

    Test.run("finish drives a stepped scan to completion", () =>
      Fixture.withTree(tree, root => {
        let h = Scan.stepped(~root, ~home, ~shouldIgnore=ignoreNothing, ());
        Scan.finish(h);
        Test.assertTrue(Scan.published(h).Scan.phase == Scan.Done, "Done");
        Test.assertEqualInt(
          Store.total(Scan.store(h)),
          treeTotal,
          "the total matches a synchronous scan",
        );
      })
    );

    Test.run("the total only ever grows while stepping", () =>
      /* Eager upward propagation is what makes a mid-scan ranking a real
         answer rather than noise, and this is the property it rests on. */
      Fixture.withTree(tree, root => {
        let h = Scan.stepped(~root, ~home, ~shouldIgnore=ignoreNothing, ());
        let last = ref(Store.total(Scan.store(h)));
        let guard = ref(0);
        while (Scan.isScanning(h) && guard^ < 100) {
          incr(guard);
          Scan.step(h);
          let now = Store.total(Scan.store(h));
          Test.assertTrue(
            now >= last^,
            "total went backwards: "
            ++ string_of_int(last^)
            ++ " -> "
            ++ string_of_int(now),
          );
          last := now;
        };
        Test.assertEqualInt(last^, treeTotal, "and it arrives at the truth");
      })
    );

    Test.run("cancel releases a parked walk", () =>
      Fixture.withTree(tree, root => {
        let h = Scan.stepped(~root, ~home, ~shouldIgnore=ignoreNothing, ());
        Scan.cancel(h);
        Scan.finish(h);
        switch (Scan.published(h).Scan.phase) {
        | Scan.Cancelled
        | Scan.Done => Test.assertTrue(true, "settled")
        | _ => Test.assertTrue(false, "a cancelled scan must settle")
        };
      })
    );
  });

let runThreaded = () =>
  Test.group("scan: threaded", () => {
    Test.run("start agrees with scanSync", () =>
      /* The ONE threaded test, and the reason the rest of this file can be
         deterministic: it proves `start` and `scanSync` are the same
         traversal. Bounded retry, never a sleep of a fixed length - the
         loop ends as soon as the phase settles. */
      Fixture.withTree(tree, root => {
        let expected =
          Store.total(
            Scan.store(
              Scan.scanSync(~root, ~home, ~shouldIgnore=ignoreNothing, ()),
            ),
          );
        let h = Scan.start(~root, ~home, ~shouldIgnore=ignoreNothing, ());
        let deadline = Unix.gettimeofday() +. 10.0;
        while (Scan.isScanning(h) && Unix.gettimeofday() < deadline) {
          Thread.yield();
        };
        Test.assertFalse(
          Scan.isScanning(h),
          "the background scan finished within ten seconds",
        );
        Test.assertTrue(Scan.published(h).Scan.phase == Scan.Done, "Done");
        Test.assertEqualInt(
          Store.total(Scan.store(h)),
          expected,
          "the threaded total equals the synchronous one",
        );
      })
    );
  });
