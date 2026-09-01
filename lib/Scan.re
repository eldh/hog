/*
 * Scan: the walk on a background thread, published to the UI as snapshots.
 *
 * This is the ONLY module in hog that mentions Thread, Mutex or Atomic.
 * Walk knows nothing about threads; Store knows nothing about anything.
 *
 * ============================ THE SYNCHRONIZATION INVARIANT ================
 *
 *   The arena is written by exactly one thread at a time: the scan thread
 *   while the phase is Scanning, the UI thread otherwise. The only
 *   cross-thread channel is one Atomic.t holding an immutable snapshot.
 *
 * The UI reads the arena DIRECTLY and WITHOUT A LOCK - it must, because
 * painting a row needs a node's name and size. That is safe under exactly
 * this set of facts, and under no others:
 *
 *   - `name`, `parent`, `kind` and `depth` are written once, at
 *     construction, and never mutated afterwards.
 *   - `size`, `items`, `flags`, `childFirst` and `childCount` are mutated,
 *     but each is a single word, and under threads.posix there is ONE
 *     domain behind a master lock: exactly one systhread executes OCaml at
 *     a time, so there are no torn reads. A stale read yields a SMALLER
 *     directory total, never garbage - and numbers ticking upward during a
 *     scan is the behaviour we want, not a defect.
 *   - Growing `store.nodes` allocates a new array, blits, and stores the
 *     new pointer. A reader holding either array sees a consistent prefix,
 *     and `nodeCount` in the snapshot is the watermark that keeps it from
 *     indexing past the part that is fully constructed.
 *
 * EVERY ONE OF THOSE READS BECOMES UNDEFINED BEHAVIOUR THE MOMENT ANYONE
 * MOVES THIS TO Domain.spawn. Do not. There is nothing to gain: the walk is
 * lstat-bound, and OCaml's lstat stub does not release the runtime lock, so
 * a second domain would buy parallelism only across opendir/readdir while
 * costing the entire lock-free reading model above.
 *
 * The corollary of lstat holding the lock is that the scan thread does not
 * voluntarily yield between lstat calls; only the systhread tick preempts
 * it. UI latency during a scan is therefore bounded at about 50ms, and no
 * publish cadence faster than 20Hz can improve on it. Do NOT "fix" that
 * with a Thread.yield per entry - that is a syscall per file on a
 * half-million-file scan. The yield belongs in onDir, once per directory,
 * where it already is.
 *
 * ================================= THE TEST SEAM ==========================
 *
 * Three entry points over ONE traversal:
 *
 *   start    - Thread.create. What the binary uses.
 *   scanSync - the same function on the caller's thread, returning a handle
 *              already at its final generation.
 *   stepped  - no thread at all; publishes one more snapshot per `step`.
 *
 * The app takes a handle as a prop, so tests drive the real component with
 * `stepped` and a virtual clock instead of sleeping. That is the only line
 * that differs between a test and the binary: every call the UI makes is
 * the real one.
 */

/* ------------------------------------------------------------------ types */

type phase =
  | Idle
  | Scanning
  | Done
  | Cancelled
  | Failed(string);

type progress = {
  files: int,
  dirs: int,
  bytes: int,
  skipped: int,
  unreadable: int,
  errorCount: int,
  /* Materialized from an id only here, at publish time - ten string builds
     a second rather than one per directory. */
  currentDir: string,
  startedAt: float,
  finishedAt: option(float),
};

type snapshot = {
  /* Monotonic. THE useMemo dependency, and the only thing the UI polls. */
  gen: int,
  phase,
  store: Store.t,
  /* Watermark: ids below this are fully constructed. */
  nodeCount: int,
  ranked: array(Store.id),
  progress,
};

/* `mode` exists so that step() can refuse to do anything on a threaded
   handle rather than corrupting one, and so scanSync can assert it never
   races with a thread it did not start. */
type mode =
  | Threaded
  | Synchronous
  | Stepped;

/* Stepped mode's rendezvous. Held IN the handle rather than in a module-level
   table: two stepped handles on the same root are ordinary in a test suite,
   and any table keyed on the path would silently make them share a gate. */
type stepState = {
  mutable sWant: int, /* directories the caller has authorized */
  mutable sDone: int, /* directories the walk has completed */
  mutable sFinished: bool,
  sMutex: Mutex.t,
  sCond: Condition.t,
};

type handle = {
  hMode: mode,
  hStore: Store.t,
  hStats: Walk.stats,
  hPublished: Atomic.t(snapshot),
  hGen: Atomic.t(int),
  hCancel: Atomic.t(bool),
  hStep: option(stepState),
  hParams: Rank.params,
  hRoot: string,
  hHome: string,
};

/* ------------------------------------------------------------- publishing */

let publishEvery = 0.1; /* seconds; 10Hz */

let emptyProgress = (~startedAt: float): progress => {
  files: 0,
  dirs: 0,
  bytes: 0,
  skipped: 0,
  unreadable: 0,
  errorCount: 0,
  currentDir: "",
  startedAt,
  finishedAt: None,
};

let progressOf = (h: handle, ~finished: option(float)): progress => {
  let s = h.hStats;
  {
    files: s.Walk.files,
    dirs: s.Walk.dirs,
    bytes: Store.total(h.hStore),
    skipped: s.Walk.skipped,
    unreadable: s.Walk.unreadable,
    errorCount: s.Walk.errorCount,
    currentDir:
      s.Walk.currentDir >= 0 && s.Walk.currentDir < h.hStore.Store.count
        ? Store.path(h.hStore, s.Walk.currentDir) : "",
    startedAt: Atomic.get(h.hPublished).progress.startedAt,
    finishedAt: finished,
  };
};

/* Build and install a new snapshot. Called from whichever thread owns the
   arena at the time, never concurrently with itself. */
let publish = (h: handle, ~phase: phase, ~finished: option(float)): unit => {
  let gen = Atomic.get(h.hGen) + 1;
  Atomic.set(h.hGen, gen);
  let ranked =
    switch (phase) {
    | Failed(_) => [||]
    | _ => Rank.frontier(h.hStore, ~root=Store.rootId, ~params=h.hParams)
    };
  Atomic.set(
    h.hPublished,
    {
      gen,
      phase,
      store: h.hStore,
      nodeCount: h.hStore.Store.count,
      ranked,
      progress: progressOf(h, ~finished),
    },
  );
};

/* ------------------------------------------------------------- the traversal */

/* The one traversal all three entry points share.
 *
 * The try is not defensive decoration: without it an escaping exception
 * kills the scan thread SILENTLY and the UI sits on "scanning..." forever.
 * That is the single most likely way to ship a broken version of this
 * program, so the failure is turned into a phase the user can see. */
let traverse = (h: handle, ~shouldIgnore, ~crossDevices: bool): unit => {
  let last = ref(Unix.gettimeofday());
  let options: Walk.options = {
    crossDevices,
    onDir: () => {
      let now = Unix.gettimeofday();
      if (now -. last^ >= publishEvery) {
        last := now;
        publish(h, ~phase=Scanning, ~finished=None);
      };
      /* One yield per directory, not per entry - see the header. */
      Thread.yield();
    },
    cancel: () => Atomic.get(h.hCancel),
  };
  switch (Walk.run(~store=h.hStore, ~stats=h.hStats, ~shouldIgnore, ~options, ())) {
  | () =>
    let phase = Atomic.get(h.hCancel) ? Cancelled : Done;
    publish(h, ~phase, ~finished=Some(Unix.gettimeofday()));
  | exception e =>
    publish(
      h,
      ~phase=Failed(Printexc.to_string(e)),
      ~finished=Some(Unix.gettimeofday()),
    )
  };
};

/* ------------------------------------------------------------ construction */

let makeHandle =
    (
      ~mode: mode,
      ~root: string,
      ~home: string,
      ~params: Rank.params,
      ~step: option(stepState)=None,
      (),
    )
    : handle => {
  let store = Walk.makeStore(~root, ~home);
  let startedAt = Unix.gettimeofday();
  let seed = {
    gen: 0,
    phase: Idle,
    store,
    nodeCount: store.Store.count,
    ranked: [||],
    progress: emptyProgress(~startedAt),
  };
  {
    hMode: mode,
    hStore: store,
    hStats: Walk.makeStats(),
    hPublished: Atomic.make(seed),
    hGen: Atomic.make(0),
    hCancel: Atomic.make(false),
    hStep: step,
    hParams: params,
    hRoot: root,
    hHome: home,
  };
};

/* ------------------------------------------------------------ entry points */

let start =
    (
      ~root: string,
      ~home: string,
      ~shouldIgnore: (~name: string, ~path: string) => bool,
      ~crossDevices: bool=false,
      ~params: Rank.params=Rank.defaults,
      (),
    )
    : handle => {
  let h = makeHandle(~mode=Threaded, ~root, ~home, ~params, ());
  publish(h, ~phase=Scanning, ~finished=None);
  let _: Thread.t =
    Thread.create(() => traverse(h, ~shouldIgnore, ~crossDevices), ());
  h;
};

let scanSync =
    (
      ~root: string,
      ~home: string,
      ~shouldIgnore: (~name: string, ~path: string) => bool,
      ~crossDevices: bool=false,
      ~params: Rank.params=Rank.defaults,
      (),
    )
    : handle => {
  let h = makeHandle(~mode=Synchronous, ~root, ~home, ~params, ());
  traverse(h, ~shouldIgnore, ~crossDevices);
  h;
};

/* Stepped mode.
 *
 * The walk is a single `while` loop inside Walk.run, so it cannot be paused
 * from outside without either a continuation (which OCaml 5 effects could
 * give us) or a thread. Rather than either, `stepped` runs the traversal on
 * a thread that BLOCKS in onDir until step() releases it. The thread exists
 * but never races the caller: exactly one of the two is runnable at any
 * moment, handed back and forth by the two Atomics below. Tests therefore
 * stay deterministic, with no sleeping and no wall-clock dependence.
 */
let stepped =
    (
      ~root: string,
      ~home: string,
      ~shouldIgnore: (~name: string, ~path: string) => bool,
      ~crossDevices: bool=false,
      ~params: Rank.params=Rank.defaults,
      (),
    )
    : handle => {
  let st = {
    sWant: 0,
    sDone: 0,
    sFinished: false,
    sMutex: Mutex.create(),
    sCond: Condition.create(),
  };
  let h = makeHandle(~mode=Stepped, ~root, ~home, ~params, ~step=Some(st), ());
  publish(h, ~phase=Scanning, ~finished=None);

  /* THE PARKING CONTRACT, which is what makes stepped mode deterministic:
     whenever control is with the caller, the walk is PARKED here and is
     touching nothing. sDone counts gates reached; sWant counts gates the
     caller has authorized passing through. The walk parks while
     sDone > sWant, so after gate number N it stops until the caller raises
     sWant to N.

     Getting this backwards - releasing the walk and returning before it
     parks again - lets the walk mutate the arena while the test reads it,
     which is exactly the single-writer invariant this module exists to
     keep. */
  let gate = () => {
    Mutex.lock(st.sMutex);
    st.sDone = st.sDone + 1;
    Condition.broadcast(st.sCond);
    while (st.sDone > st.sWant && ! Atomic.get(h.hCancel)) {
      Condition.wait(st.sCond, st.sMutex);
    };
    Mutex.unlock(st.sMutex);
  };

  /* Publish on EVERY directory here, unlike the timed 10Hz cadence of the
     threaded path: a test that calls step() once expects exactly one more
     snapshot, not "one if enough wall-clock has passed". */
  let options: Walk.options = {
    crossDevices,
    onDir: () => {
      publish(h, ~phase=Scanning, ~finished=None);
      gate();
    },
    cancel: () => Atomic.get(h.hCancel),
  };
  let _: Thread.t =
    Thread.create(
      () => {
        switch (Walk.run(~store=h.hStore, ~stats=h.hStats, ~shouldIgnore, ~options, ())) {
        | () =>
          let phase = Atomic.get(h.hCancel) ? Cancelled : Done;
          publish(h, ~phase, ~finished=Some(Unix.gettimeofday()));
        | exception e =>
          publish(
            h,
            ~phase=Failed(Printexc.to_string(e)),
            ~finished=Some(Unix.gettimeofday()),
          )
        };
        Mutex.lock(st.sMutex);
        st.sFinished = true;
        Condition.broadcast(st.sCond);
        Mutex.unlock(st.sMutex);
      },
      (),
    );
  /* Do not return until the walk is parked at its first gate (or has already
     finished - a root with no subdirectories reaches neither). Returning
     earlier would hand the caller a handle whose arena is still being
     written. */
  Mutex.lock(st.sMutex);
  while (st.sDone == 0 && ! st.sFinished) {
    Condition.wait(st.sCond, st.sMutex);
  };
  Mutex.unlock(st.sMutex);
  h;
};

/* Let the walk complete one more directory, and return once it has (or once
   the walk has finished). Deterministic: no sleeping, no polling. */
let step = (h: handle): unit =>
  switch (h.hStep) {
  | None => () /* threaded or synchronous: nothing to release */
  | Some(st) =>
    Mutex.lock(st.sMutex);
    /* Authorize passing the gate the walk is parked at, then wait until it
       parks at the NEXT one - so this returns with the walk stopped again. */
    let from = st.sDone;
    st.sWant = from;
    Condition.broadcast(st.sCond);
    while (! st.sFinished && st.sDone <= from) {
      Condition.wait(st.sCond, st.sMutex);
    };
    Mutex.unlock(st.sMutex);
  };

let isFinished = (h: handle): bool =>
  switch (Atomic.get(h.hPublished).phase) {
  | Done
  | Cancelled
  | Failed(_) => true
  | Idle
  | Scanning => false
  };

/* Run a stepped handle to completion. */
let finish = (h: handle): unit =>
  switch (h.hStep) {
  | None => ()
  | Some(st) =>
    while (!
             {
               Mutex.lock(st.sMutex);
               let done_ = st.sFinished;
               Mutex.unlock(st.sMutex);
               done_;
             }) {
      step(h);
    }
  };

/* ----------------------------------------------------------------- reading */

/* Cheap, non-allocating, and safe to call from the UI thread at any time.
   This is what the render loop polls. */
let generation = (h: handle): int => Atomic.get(h.hGen);

let published = (h: handle): snapshot => Atomic.get(h.hPublished);

let cancel = (h: handle): unit => {
  Atomic.set(h.hCancel, true);
  /* A stepped walk may be parked in its gate waiting for permission it will
     now never get; wake it so it can observe the cancel and unwind. */
  switch (h.hStep) {
  | Some(st) =>
    Mutex.lock(st.sMutex);
    st.sWant = max_int;
    Condition.broadcast(st.sCond);
    Mutex.unlock(st.sMutex);
  | None => ()
  };
};

let isScanning = (h: handle): bool =>
  switch (Atomic.get(h.hPublished).phase) {
  | Scanning => true
  | Idle
  | Done
  | Cancelled
  | Failed(_) => false
  };

/* Resolve an absolute path to an id in the CURRENT snapshot.
 *
 * The UI holds its scope and its selection as PATHS, not ids, and comes
 * back through here every generation. Ids do not survive a rescan, and a
 * held id silently addresses a different node afterwards. */
let resolve = (h: handle, p: string): option(Store.id) =>
  Store.resolve(h.hStore, p);

/* The list for a view. `scope = None` is the landing view - the ranked
   frontier, the biggest things anywhere. `scope = Some(path)` is the
   drill-down: that directory's immediate children, plain and complete,
   because once you have descended you want a file manager, not an opinion.
 *
 * Takes the SNAPSHOT rather than the handle, so that a caller physically
 * cannot mix a `ranked` array from one generation with anything read at
 * another. Making the rule structural is worth more than the convenience of
 * passing a handle: the failure it prevents is a cross-generation index,
 * which only misbehaves under load and never in a test. */
let entries =
    (h: handle, ~snapshot: snapshot, ~scope: option(string), ~showIgnored: bool)
    : array(Store.id) =>
  switch (scope) {
  | None =>
    /* The published ranking already excludes ignored nodes, so revealing
       them has to re-rank. That costs microseconds and only happens on the
       keypress, which is a better trade than publishing two rankings ten
       times a second for a view the user is usually not in. */
    showIgnored
      ? Rank.frontierRevealed(h.hStore, ~root=Store.rootId, ~params=h.hParams)
      : snapshot.ranked
  | Some(p) =>
    switch (Store.resolve(h.hStore, p)) {
    | None => [||]
    | Some(id) => Rank.sortedChildren(h.hStore, id, ~showIgnored)
    }
  };

/* Mark a subtree ignored and take its bytes back out of every ancestor.
 *
 * This is what makes the `i` key honest. Without it the row disappears but
 * the header keeps counting the twenty gigabytes the user just said they
 * did not want to see, and the totals stay wrong until the next full scan.
 * The subtraction is O(depth) - ten integer adds - because sizes propagate
 * upward eagerly, so the arithmetic is already there to undo.
 *
 * Returns false, and changes nothing, WHILE A SCAN IS RUNNING: `addUp` is a
 * read-modify-write over every ancestor, so doing it from the UI thread
 * while the scan thread is doing its own would silently lose updates. That
 * is the single-writer invariant at the top of this file, and it is not
 * negotiable for a convenience. The caller falls back to hiding the row,
 * and the rule takes effect properly on the next run. */
let applyIgnore = (h: handle, ~path: string): bool =>
  if (isScanning(h)) {
    false;
  } else {
    switch (Store.resolve(h.hStore, path)) {
    | None => false
    | Some(id) =>
      let n = Store.get(h.hStore, id);
      if (Store.hasFlag(n.Store.flags, Store.fIgnored)) {
        false; /* already ignored: subtracting twice would go negative */
      } else {
        n.Store.flags = n.Store.flags lor Store.fIgnored;
        /* items + 1, not items: `node.items` counts what is BELOW the node,
           while every ancestor was also charged one for the node itself. */
        Store.addUp(
          h.hStore,
          id,
          ~bytes=- n.Store.size,
          ~items=- (n.Store.items + 1),
        );
        publish(h, ~phase=Atomic.get(h.hPublished).phase, ~finished=None);
        true;
      };
    };
  };

/* The inverse, for un-ignoring within a session. Only meaningful for a node
   that was ignored by applyIgnore - one pruned during the walk has no size
   to give back, because we never opened it. */
let unapplyIgnore = (h: handle, ~path: string): bool =>
  if (isScanning(h)) {
    false;
  } else {
    switch (Store.resolve(h.hStore, path)) {
    | None => false
    | Some(id) =>
      let n = Store.get(h.hStore, id);
      if (! Store.hasFlag(n.Store.flags, Store.fIgnored)) {
        false;
      } else {
        n.Store.flags = n.Store.flags land lnot(Store.fIgnored);
        Store.addUp(
          h.hStore,
          id,
          ~bytes=n.Store.size,
          ~items=n.Store.items + 1,
        );
        publish(h, ~phase=Atomic.get(h.hPublished).phase, ~finished=None);
        true;
      };
    };
  };

let store = (h: handle): Store.t => h.hStore;
let root = (h: handle): string => h.hRoot;
let home = (h: handle): string => h.hHome;
let errors = (h: handle): list((string, string)) => Walk.errorList(h.hStats);
