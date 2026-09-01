/*
 * The node arena.
 *
 * Every scanned entry is a record in one growable array, and a reference to
 * an entry is its `int` index. That is not only a memory decision: an OCaml
 * int is an immediate, so an id is a legal `useMemo` dependency and a legal
 * `setState` value, which the UI needs everywhere. An `option(node)` parent
 * pointer would be neither.
 *
 * Pure. No Unix, no threads, no I/O - Walk fills it, Rank reads it, and both
 * are testable against an arena built by hand in memory.
 *
 * Two representation choices are load-bearing and must not be "modernized":
 *
 *   - Sizes are plain `int`, never `int64`. OCaml's native int is 63-bit,
 *     which tops out at 4 exabytes; `int64` would box every field and every
 *     array element. The conversion happens once, at the syscall boundary.
 *
 *   - Timestamps are `int` Unix seconds, never `float`. A float field inside
 *     a mixed record is boxed individually, and three of them across 500k
 *     nodes is ~36 MB spent on sub-second precision no pane ever shows.
 *
 * Children are a CONTIGUOUS RANGE - `[childFirst, childFirst + childCount)` -
 * which is what removes the per-node child array entirely. It holds only
 * because Walk allocates one directory's entries in a single uninterrupted
 * loop; see the assertion there.
 */

type kind =
  | Dir
  | File
  | Link
  | Other;

type id = int;

let noId: id = (-1);
let rootId: id = 0;

/* ------------------------------------------------------------------ flags */

/* A bitmask, because every one of these is a property of a node that the
 * ranking and the row builder have to test on a hot path. */

/* Matched an ignore rule: never descended, never ranked. */
let fIgnored = 1;
/* opendir or readdir failed, so this directory's size is a LOWER BOUND. */
let fUnreadable = 2;
/* This (dev, ino) was already counted: size forced to 0, because deleting
 * this name frees nothing while the other name survives. */
let fHardLink = 4;
/* On a different filesystem than the root, and we do not cross by default:
 * the node exists so the user can see why the number is small, with size 0. */
let fOtherDev = 8;
/* A large image file whose apparent size probably exceeds what it allocates.
 * The size is still shown, prefixed with "~". */
let fSparseSuspect = 16;

let hasFlag = (flags: int, flag: int): bool => flags land flag != 0;

/* ------------------------------------------------------------------ nodes */

type node = {
  /* Basename. The ONLY per-node string - a full path is materialized on
     demand by `path`, which is why the arena stays around 120 bytes a node. */
  name: string,
  parent: id,
  kind,
  depth: int,
  mutable childFirst: id,
  mutable childCount: int,
  /* Bytes: its own for a file, the recursive total for a directory. */
  mutable size: int,
  /* Number of entries underneath, not counting the node itself. */
  mutable items: int,
  mutable flags: int,
  mtime: int,
  atime: int,
  ctime: int,
};

type t = {
  mutable nodes: array(node),
  mutable count: int,
  root: string,
  home: string,
};

/* The filler for the unwritten tail of `nodes`. Every slot is overwritten by
 * `alloc` before anything can reach it, so the sharing is invisible; nothing
 * may ever hand this value out or mutate it. */
let placeholder = {
  name: "",
  parent: noId,
  kind: Other,
  depth: 0,
  childFirst: noId,
  childCount: 0,
  size: 0,
  items: 0,
  flags: 0,
  mtime: 0,
  atime: 0,
  ctime: 0,
};

/* The most children one directory may contribute to the arena. Past this the
 * walk stops reading entries and marks the directory unreadable, because its
 * total is then a lower bound - which is exactly what fUnreadable means. It
 * exists so that a pathological directory cannot exhaust memory before the
 * user ever sees a frame. */
let childLimit = 200000;

/* `root` is the absolute path the scan started from; `home` is only ever used
 * to shorten a path for DISPLAY. The root node is allocated here, so a store
 * always has a node 0 and `rootId` is always valid. */
let create =
    (
      ~root: string,
      ~home: string,
      ~capacity: int=1024,
      ~mtime: int=0,
      ~atime: int=0,
      ~ctime: int=0,
      (),
    )
    : t => {
  let cap = max(1, capacity);
  let t = {
    nodes: Array.make(cap, placeholder),
    count: 0,
    root,
    home,
  };
  t.nodes[0] = {
    name: Filename.basename(root),
    parent: noId,
    kind: Dir,
    depth: 0,
    childFirst: noId,
    childCount: 0,
    size: 0,
    items: 0,
    flags: 0,
    mtime,
    atime,
    ctime,
  };
  t.count = 1;
  t;
};

let get = (t: t, id: id): node =>
  if (id < 0 || id >= t.count) {
    invalid_arg("Store.get: no such id " ++ string_of_int(id));
  } else {
    t.nodes[id];
  };

let total = (t: t): int => t.nodes[rootId].size;

let alloc =
    (
      t: t,
      ~name: string,
      ~parent: id,
      ~kind: kind,
      ~size: int=0,
      ~items: int=0,
      ~flags: int=0,
      ~mtime: int=0,
      ~atime: int=0,
      ~ctime: int=0,
      (),
    )
    : id => {
  if (parent < 0 || parent >= t.count) {
    invalid_arg("Store.alloc: no such parent " ++ string_of_int(parent));
  };
  if (t.count == Array.length(t.nodes)) {
    let bigger = Array.make(Array.length(t.nodes) * 2, placeholder);
    Array.blit(t.nodes, 0, bigger, 0, t.count);
    t.nodes = bigger;
  };
  let id = t.count;
  t.nodes[id] = {
    name,
    parent,
    kind,
    depth: t.nodes[parent].depth + 1,
    childFirst: noId,
    childCount: 0,
    size,
    items,
    flags,
    mtime,
    atime,
    ctime,
  };
  t.count = id + 1;
  id;
};

let setChildRange = (t: t, id: id, ~first: id, ~count: int): unit => {
  let n = get(t, id);
  n.childFirst = count == 0 ? noId : first;
  n.childCount = count;
};

let iterChildren = (t: t, id: id, f: id => unit): unit => {
  let n = get(t, id);
  if (n.childCount > 0) {
    for (i in n.childFirst to n.childFirst + n.childCount - 1) {
      f(i);
    };
  };
};

/* Add one node's own contribution to every ancestor, up to and including the
 * root. Called once per created node, so it is O(depth) - about ten int adds.
 *
 * Eager upward propagation is deliberate. Directory totals stay live and
 * monotonically increasing while the scan runs, which is what a progress
 * display wants, and the invariant `parent.size == sum of children sizes`
 * holds at EVERY instant - which is exactly what lets the ranking run
 * correctly against a half-finished tree. */
let addUp = (t: t, id: id, ~bytes: int, ~items: int): unit => {
  let cur = ref(get(t, id).parent);
  while (cur^ != noId) {
    let n = t.nodes[cur^];
    n.size = n.size + bytes;
    n.items = n.items + items;
    cur := n.parent;
  };
};

/* ------------------------------------------------------------------ paths */

let joinRoot = (root: string, tail: string): string =>
  if (tail == "") {
    root;
  } else if (root == "") {
    tail;
  } else if (root.[String.length(root) - 1] == '/') {
    root ++ tail;
  } else {
    root ++ "/" ++ tail;
  };

/* The absolute path of a node, rebuilt from the basenames on the way up.
 * Cheap enough to call per directory during the walk, far too expensive to
 * call per entry - which is why the walk carries a directory id and
 * materializes the string once. */
let path = (t: t, id: id): string =>
  if (id == rootId) {
    t.root;
  } else {
    let parts = ref([]);
    let cur = ref(id);
    while (cur^ != rootId && cur^ != noId) {
      let n = get(t, cur^);
      parts := [n.name, ...parts^];
      cur := n.parent;
    };
    joinRoot(t.root, String.concat("/", parts^));
  };

/* The same path with the user's home rewritten to "~". For display only -
 * never pass this to anything that opens, moves or deletes a file. */
let displayPath = (t: t, id: id): string =>
  Fmt.tildify(~home=t.home, path(t, id));

/* Strictly above: a node is not its own ancestor. Deliberately a walk over
 * parent links rather than a string prefix test, because "Lib" is a prefix of
 * "Library" and no amount of care with separators makes that comparison
 * anything but a trap. */
let isAncestor = (t: t, ~ancestor: id, id: id): bool => {
  let cur = ref(get(t, id).parent);
  let found = ref(false);
  while (! found^ && cur^ != noId) {
    if (cur^ == ancestor) {
      found := true;
    } else {
      cur := get(t, cur^).parent;
    };
  };
  found^;
};

let segments = (s: string): list(string) =>
  List.filter(x => x != "", String.split_on_char('/', s));

/* Find the node for an absolute path, or None when it is outside the scanned
 * tree or was never scanned.
 *
 * The UI stores its scope and its selection as PATHS rather than ids, because
 * ids do not survive a rescan, and re-resolves them every generation. This is
 * that function, so it must never match by prefix alone: "/a/xy" is not
 * inside "/a/x". */
let resolve = (t: t, p: string): option(id) => {
  let rl = String.length(t.root);
  let rel =
    if (p == t.root) {
      Some("");
    } else if (String.length(p) > rl
               && String.sub(p, 0, rl) == t.root
               && (t.root == "/" || p.[rl] == '/')) {
      Some(String.sub(p, rl, String.length(p) - rl));
    } else {
      None;
    };
  switch (rel) {
  | None => None
  | Some(rel) =>
    List.fold_left(
      (acc, seg) =>
        switch (acc) {
        | None => None
        | Some(parent) =>
          let found = ref(None);
          iterChildren(
            t,
            parent,
            child =>
              if (found^ == None && get(t, child).name == seg) {
                found := Some(child);
              },
          );
          found^;
        },
      Some(rootId),
      segments(rel),
    )
  };
};
