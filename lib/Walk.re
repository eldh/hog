/*
 * The filesystem walk: a depth-first traversal over an explicit stack of
 * directory ids, filling a Store arena.
 *
 * Single-threaded and synchronous. Scan is the module that puts it on a
 * background thread and publishes snapshots; nothing here knows about
 * threads, and nothing here prints.
 *
 * DFS rather than BFS because it completes whole subtrees progressively, so
 * an intermediate ranking taken mid-scan is a real answer about a finished
 * part of the tree rather than noise about a uniformly shallow one. An
 * explicit stack rather than recursion because the depth is attacker-supplied
 * in the sense that matters - a deep tree must not blow the OCaml stack.
 *
 * ---------------------------------------------------------------- lstat
 *
 * `Unix.LargeFile.lstat`, NEVER `stat`. Three independent reasons, each
 * fatal on its own:
 *
 *   - `stat` follows symlinks, so a link pointing at one of its own
 *     ancestors makes the walk non-terminating;
 *   - macOS is full of symlinks and firmlinks into /Applications and
 *     /System/Volumes/Data, so following them attributes the same bytes to
 *     two paths and pushes the root total past the size of the disk;
 *   - a symlink's honest size is its own, because deleting it frees about
 *     sixty bytes, not four gigabytes.
 *
 * For the same reason nothing here ever calls `opendir` on a path whose kind
 * is S_LNK.
 *
 * `Unix.readdir` returns names only - there is no d_type - so one lstat per
 * entry is mandatory, and it is the dominant cost of the whole scan.
 *
 * ---------------------------------------------------------------- ignoring
 *
 * The ignore rules arrive as a FUNCTION, `shouldIgnore(~name, ~path)`, not as
 * a compiled rule set. That is the only thing the walk needs from ignore
 * matching, it keeps this module free of a dependency on lib/Ignore.re (which
 * is a later milestone and does not exist yet), and it makes the pruning
 * behaviour trivially testable with a closure that records what it was asked
 * about. Ignore.re will supply the closure; nothing about the walk changes.
 *
 * Pruning is the entire performance argument, so the rules are tested at
 * ENTRY CREATION, before the descend decision - an ignored directory is
 * given a node and is then never opened.
 */

/* Hard cap on how deep we descend. A node may exist at this depth; we never
 * open a directory that is already at it. */
let maxDepth = 64;

/* The error list is bounded because a tree with a hundred thousand
 * unreadable directories must cost a hundred thousand ints, not a hundred
 * thousand strings. The uncapped count is kept separately. */
let errorLimit = 200;

/* Above this, an image file's apparent size is worth a warning marker. */
let sparseThreshold = 4 * 1024 * 1024 * 1024;

let sparseExtensions = [
  ".raw",
  ".qcow2",
  ".img",
  ".vmdk",
  ".sparsebundle",
  ".dmg",
];

/* ---------------------------------------------------------------- options */

type options = {
  /* du -x semantics by default. Irrelevant for a $HOME scan and decisive at
     "/", where /System/Volumes/Data is a firmlink onto the same data volume
     (double-counting the entire disk) and /Volumes/.timemachine re-walks it
     once per snapshot. */
  crossDevices: bool,
  /* Called once per completed directory, so a caller can publish progress. */
  onDir: unit => unit,
  /* Checked once per completed directory. Returning true stops the walk and
     leaves the arena consistent but incomplete. */
  cancel: unit => bool,
};

let defaultOptions = {
  crossDevices: false,
  onDir: () => (),
  cancel: () => false,
};

/* ------------------------------------------------------------------ stats */

type stats = {
  mutable files: int,
  mutable dirs: int,
  /* Entries that vanished between readdir and lstat. This is NORMAL, not an
     error: ~/Library/Caches churns constantly while the machine runs. It gets
     its own counter, creates no node, and never touches the error list. */
  mutable skipped: int,
  /* Directories we could not read. Their totals are lower bounds, and the UI
     has to say so - on macOS this is usually Full Disk Access, not a broken
     permission, and a tool that under-reports ~/Library by tens of GB without
     saying it is lying is worse than no tool. */
  mutable unreadable: int,
  mutable errorCount: int,
  /* Newest first, at most errorLimit entries. Use `errorList` for the
     chronological order a UI wants. */
  mutable errors: list((string, string)),
  /* Held as an id, materialized to a string only at publish time - ten string
     builds a second instead of fifty thousand. */
  mutable currentDir: Store.id,
};

let makeStats = (): stats => {
  files: 0,
  dirs: 0,
  skipped: 0,
  unreadable: 0,
  errorCount: 0,
  errors: [],
  currentDir: Store.rootId,
};

let errorList = (s: stats): list((string, string)) => List.rev(s.errors);

let recordError = (s: stats, path: string, msg: string): unit => {
  if (s.errorCount < errorLimit) {
    s.errors = [(path, msg), ...s.errors];
  };
  s.errorCount = s.errorCount + 1;
};

/* ------------------------------------------------------------- stat glue */

/* st_size is an int64 in the Unix interface and an int in the arena. Negative
 * is impossible in practice but is clamped rather than trusted, and the
 * upper clamp costs nothing and removes the only way this conversion could
 * produce a nonsense negative. */
let bytesOfInt64 = (n: Int64.t): int =>
  if (Int64.compare(n, 0L) <= 0) {
    0;
  } else if (Int64.compare(n, Int64.of_int(max_int)) > 0) {
    max_int;
  } else {
    Int64.to_int(n);
  };

/* Everything one lstat gives us, already in the plain int form the arena
 * stores. Converting here is what keeps Int64 and float out of Store. */
type entry = {
  eKind: Store.kind,
  eSize: int,
  eNlink: int,
  eDev: int,
  eIno: int,
  eMtime: int,
  eAtime: int,
  eCtime: int,
};

let readStat = (p: string): entry => {
  open Unix.LargeFile;
  let st = lstat(p);
  {
    eKind:
      switch (st.st_kind) {
      | Unix.S_DIR => Store.Dir
      | Unix.S_REG => Store.File
      | Unix.S_LNK => Store.Link
      | _ => Store.Other
      },
    eSize: bytesOfInt64(st.st_size),
    eNlink: st.st_nlink,
    eDev: st.st_dev,
    eIno: st.st_ino,
    eMtime: int_of_float(st.st_mtime),
    eAtime: int_of_float(st.st_atime),
    eCtime: int_of_float(st.st_ctime),
  };
};

/* ------------------------------------------------------------------ paths */

let join = (dir: string, name: string): string =>
  if (dir == "") {
    name;
  } else if (dir.[String.length(dir) - 1] == '/') {
    dir ++ name;
  } else {
    dir ++ "/" ++ name;
  };

let lowercase = String.lowercase_ascii;

let looksSparse = (~kind: Store.kind, ~size: int, ~name: string): bool =>
  kind == Store.File
  && size > sparseThreshold
  && List.mem(lowercase(Filename.extension(name)), sparseExtensions);

/* ------------------------------------------------------------------ store */

/* Build the arena the walk will fill. Separate from `run` so that Scan can
 * hold the store - and publish snapshots of it - while the walk is still
 * running inside it. Raises if the root is not a directory we can stat. */
let makeStore = (~root: string, ~home: string): Store.t => {
  let e = readStat(root);
  if (e.eKind != Store.Dir) {
    invalid_arg("Walk.makeStore: not a directory: " ++ root);
  };
  Store.create(
    ~root,
    ~home,
    ~mtime=e.eMtime,
    ~atime=e.eAtime,
    ~ctime=e.eCtime,
    (),
  );
};

/* ------------------------------------------------------------------- walk */

/* Read every name in a directory, or None when it could not be opened.
 *
 * Errors here are classified and never abort the scan: the directory keeps
 * its node, gets fUnreadable, and the walk carries on with its siblings. An
 * EACCES on ~/Library/Mail must cost the user that one folder, not the run. */
let readNames =
    (~stats: stats, ~node: Store.node, dirPath: string): option(list(string)) =>
  switch (Unix.opendir(dirPath)) {
  | exception (Unix.Unix_error(err, _, _)) =>
    node.Store.flags = node.Store.flags lor Store.fUnreadable;
    stats.unreadable = stats.unreadable + 1;
    recordError(stats, dirPath, Unix.error_message(err));
    None;
  | dh =>
    let acc = ref([]);
    /* closedir in a finaliser: a readdir failure mid-directory still has to
       give the descriptor back, and this loop has three exits. */
    Fun.protect(
      ~finally=
        () =>
          switch (Unix.closedir(dh)) {
          | () => ()
          | exception (Unix.Unix_error(_, _, _)) => ()
          },
      () => {
        let go = ref(true);
        while (go^) {
          switch (Unix.readdir(dh)) {
          | name =>
            if (name != "." && name != "..") {
              acc := [name, ...acc^];
            }
          | exception End_of_file => go := false
          | exception (Unix.Unix_error(err, _, _)) =>
            node.Store.flags = node.Store.flags lor Store.fUnreadable;
            stats.unreadable = stats.unreadable + 1;
            recordError(stats, dirPath, Unix.error_message(err));
            go := false;
          };
        };
      },
    );
    /* Sorted, because readdir order is arbitrary: sorting costs nothing next
       to one lstat per entry and it makes the contiguous child range - and
       therefore every drill-down list and every test - deterministic. */
    Some(List.sort(compare, acc^));
  };

/* Fill `store` by walking it from the root. `shouldIgnore` is described in
 * the module header. */
let run =
    (
      ~store: Store.t,
      ~stats: stats,
      ~shouldIgnore: (~name: string, ~path: string) => bool,
      ~options: options=defaultOptions,
      (),
    )
    : unit => {
  let rootDev = readStat(store.Store.root).eDev;
  /* Only ever consulted for regular files with more than one link, so it
     stays small: a few thousand entries on a normal home directory. */
  let seenLinks = Hashtbl.create(1024);
  let stack = ref([Store.rootId]);
  let cancelled = ref(false);

  while (! cancelled^ && stack^ != []) {
    let dirId = List.hd(stack^);
    stack := List.tl(stack^);
    let dirNode = Store.get(store, dirId);
    let dirPath = Store.path(store, dirId);
    stats.currentDir = dirId;

    switch (readNames(~stats, ~node=dirNode, dirPath)) {
    | None => Store.setChildRange(store, dirId, ~first=Store.noId, ~count=0)
    | Some(names) =>
      /* THE BATCH-ALLOCATION INVARIANT.
         Children are stored as the contiguous range [first, first + count),
         which holds only while nothing else allocates between here and
         setChildRange. Nothing in this loop may call Store.alloc for any
         other parent, and nothing may be moved out of it. The assertion below
         is the guard: violating this silently reparents half the tree with no
         error anywhere. */
      let first = store.Store.count;
      let count = ref(0);
      let subdirs = ref([]);
      let truncated = ref(false);

      List.iter(
        name =>
          if (count^ >= Store.childLimit) {
            truncated := true;
          } else {
            let childPath = join(dirPath, name);
            switch (readStat(childPath)) {
            | exception (Unix.Unix_error(Unix.ENOENT | Unix.ENOTDIR, _, _)) =>
              /* Vanished between readdir and lstat. Normal, not an error. */
              stats.skipped = stats.skipped + 1
            | exception (Unix.Unix_error(err, _, _)) =>
              stats.skipped = stats.skipped + 1;
              recordError(stats, childPath, Unix.error_message(err));
            | e =>
              let flags = ref(0);
              /* A directory inode's own st_size is not disk the user can
                 reclaim, so a directory starts at zero and grows only from
                 what is inside it. */
              let size = ref(e.eKind == Store.Dir ? 0 : e.eSize);

              /* Hard links. The S_REG test is a CORRECTNESS requirement, not
                 an optimization: on macOS every directory has st_nlink >= 2,
                 so a guard on nlink alone eventually collides with a
                 previously seen inode and silently zeroes a whole subtree. */
              if (e.eKind == Store.File && e.eNlink > 1) {
                let key = (e.eDev, e.eIno);
                if (Hashtbl.mem(seenLinks, key)) {
                  flags := flags^ lor Store.fHardLink;
                  size := 0;
                } else {
                  Hashtbl.add(seenLinks, key, ());
                };
              };

              if (! options.crossDevices && e.eDev != rootDev) {
                flags := flags^ lor Store.fOtherDev;
                size := 0;
              };

              /* Tested at creation, BEFORE the descend decision below, which
                 is what makes an ignored directory free rather than merely
                 hidden. An ignored entry keeps whatever size we know without
                 opening it: for a file that is its real size, for a directory
                 that is zero. */
              if (shouldIgnore(~name, ~path=childPath)) {
                flags := flags^ lor Store.fIgnored;
              };

              if (looksSparse(~kind=e.eKind, ~size=size^, ~name)) {
                flags := flags^ lor Store.fSparseSuspect;
              };

              let id =
                Store.alloc(
                  store,
                  ~name,
                  ~parent=dirId,
                  ~kind=e.eKind,
                  ~size=size^,
                  ~items=0,
                  ~flags=flags^,
                  ~mtime=e.eMtime,
                  ~atime=e.eAtime,
                  ~ctime=e.eCtime,
                  (),
                );
              Store.addUp(store, id, ~bytes=size^, ~items=1);
              count := count^ + 1;

              if (e.eKind == Store.Dir) {
                stats.dirs = stats.dirs + 1;
                /* Never open a symlink (its kind is Link, so it cannot reach
                   here), never open across a device boundary, never open an
                   ignored directory, never go past the depth cap. */
                if (flags^ land (Store.fIgnored lor Store.fOtherDev) == 0
                    && dirNode.Store.depth + 1 < maxDepth) {
                  subdirs := [id, ...subdirs^];
                };
              } else {
                stats.files = stats.files + 1;
              };
            };
          },
        names,
      );

      Store.setChildRange(store, dirId, ~first, ~count=count^);
      if (first + count^ != store.Store.count) {
        /* Raised rather than `assert`, so it survives every build profile.
           Scan turns an escaped exception into a visible Failed phase. */
        failwith(
          "Walk: child range broken for "
          ++ dirPath
          ++ " - something allocated mid-directory",
        );
      };

      if (truncated^) {
        dirNode.Store.flags = dirNode.Store.flags lor Store.fUnreadable;
        stats.unreadable = stats.unreadable + 1;
        recordError(stats, dirPath, "too many entries; list truncated");
      };

      /* subdirs is in reverse order of `names`, so rev_append puts the first
         name on top of the stack and the DFS visits children in sorted
         order. */
      stack := List.rev_append(subdirs^, stack^);
    };

    options.onDir();
    if (options.cancel()) {
      cancelled := true;
    };
  };
};
