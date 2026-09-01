/*
 * Ranking: the landing view's frontier, and the drill-down sort.
 *
 * Pure. A function of (store, root, params) and nothing else - no Unix, no
 * I/O, no mutable module state - which is what lets it be called from either
 * thread, run inside every 10 Hz publish, and unit-tested against an arena
 * built by hand with no filesystem at all.
 *
 * ------------------------------------------------------------- the problem
 *
 * The landing view cannot be "sort every node by size and take the top 200".
 * A huge directory drags in every one of its huge ancestors and every one of
 * its huge descendants, so ~/Library, ~/Library/Caches and
 * ~/Library/Caches/com.apple.Safari all appear and the screen says one thing
 * five times.
 *
 * -------------------------------------------------------------- the answer
 *
 * A size-ordered frontier refinement over a set that is an ANTICHAIN BY
 * CONSTRUCTION: no member is ever an ancestor of another, because a member
 * only ever leaves the set by being REPLACED with its own children. Seed the
 * set with the root's children, then repeatedly take the largest member and
 * either split it - swap it for its above-threshold children - or emit it as
 * a row.
 *
 * Split N iff all of:
 *
 *   - N is a directory, and its depth below the ranking root is < maxDepth;
 *   - it has A >= 1 candidate children of size >= T, the largest of them m;
 *   - EITHER A <= fanoutCap (few children, so naming them is informative)
 *     OR m * 2 >= N.size (one child holds at least half, so N is a
 *     pass-through whose name tells you nothing its child's does not);
 *   - the row budget still allows A more members.
 *
 * Otherwise emit it.
 *
 * Two behaviours that look like special cases fall out of that predicate for
 * free, which is the reason it is shaped this way:
 *
 *   - A pass-through chain collapses. A dominant child becomes the next
 *     maximum and is split immediately, so Containers/com.docker.docker/
 *     Data/vms/0/data costs six ITERATIONS and not one row slot.
 *   - A directory's loose files need no special scoring: they are children,
 *     competing with their sibling directories on equal terms.
 *
 * The root is never a candidate. A wide top level would otherwise satisfy
 * nothing, be emitted whole, and the landing view would be a single line
 * reading "~".
 *
 * ---------------------------------------------------------------- the cost
 *
 * `frontier` NEVER iterates the arena. It touches only the children of the
 * directories it actually splits - O(C log K) for C such children - so it is
 * microseconds on a half-million-node tree, and running it inside every
 * publish is free. Any change that introduces an Array.length(store.nodes)
 * pass has broken the one property this design exists for.
 */

type params = {
  /* The row budget. The frontier never returns more than this. */
  maxRows: int,
  /* Absolute floor: nothing smaller is ever a row, however empty the disk. */
  minBytes: int,
  /* Relative floor: the ranked subtree's total divided by this. Scaling with
     the total is what keeps the list interesting on a 200 GB home directory
     and on a 2 GB one. */
  minFraction: int,
  /* Above this many above-threshold children, splitting stops being
     informative - twenty rows that all say "a cache" are worse than one row
     that says "your caches". Dominance can still override it. */
  fanoutCap: int,
  /* How far below the ranking root we will descend. */
  maxDepth: int,
};

let defaults: params = {
  maxRows: 200,
  minBytes: 64 * 1024 * 1024,
  minFraction: 1000,
  fanoutCap: 8,
  maxDepth: 12,
};

/* ------------------------------------------------------------- candidacy */

/* Three kinds of node can never be a row, and for three different reasons:
   an ignored node was never descended so its size is not even known; an
   other-device node was deliberately not crossed into; and a hard link's
   bytes belong to the name that was counted first, so deleting this one
   frees nothing. All three are excluded by FLAG rather than by size, because
   an excluded node must stay excluded even if some future walk gives it a
   size. */
let excludedMask = Store.fIgnored lor Store.fOtherDev lor Store.fHardLink;

let isCandidate = (n: Store.node): bool => n.Store.flags land excludedMask == 0;

/* The same test with reveal (`I`) held on.
 *
 * fIgnored is the only one of the three that may be lifted, and only because
 * there are two kinds of ignored node. One was PRUNED during the walk and
 * has no measured size at all - revealing it can put it in a list but never
 * meaningfully in a ranking, and it simply falls below the threshold. The
 * other was ignored from inside the application AFTER being measured, so it
 * has a real size and deserves its real place the moment the user asks to
 * see it again.
 *
 * fOtherDev and fHardLink stay excluded either way: both are deliberately
 * size 0, so ranking them could only ever produce rows reading "0B". */
let isCandidateRevealed = (n: Store.node): bool =>
  n.Store.flags land (Store.fOtherDev lor Store.fHardLink) == 0;

/* T = max(minBytes, total / minFraction), where `total` is the size of the
   subtree being ranked - which for the default root is exactly
   Store.total. Guarded against a zero divisor so that a caller-supplied
   params record can never turn a ranking into an exception. */
let threshold = (~total: int, ~params: params): int =>
  if (params.minFraction <= 0) {
    params.minBytes;
  } else {
    max(params.minBytes, total / params.minFraction);
  };

/* -------------------------------------------------------------- ordering */

/* Size descending, then name ascending, then id.
 *
 * The name tie-break is not cosmetic: the frontier is recomputed from a
 * growing arena ten times a second, and two equal-sized entries that swapped
 * places between publishes would move a row under the user's cursor. The id
 * is the last resort that makes the order TOTAL - two nodes can genuinely
 * share a size and a basename (two directories both called `Caches`), and
 * `Array.sort` is not stable, so without it their order would be an
 * implementation detail of the sort. */
let compareNodes = (t: Store.t, a: Store.id, b: Store.id): int => {
  let na = Store.get(t, a);
  let nb = Store.get(t, b);
  if (na.Store.size != nb.Store.size) {
    compare(nb.Store.size, na.Store.size);
  } else {
    let byName = compare(na.Store.name, nb.Store.name);
    byName != 0 ? byName : compare(a, b);
  };
};

/* ------------------------------------------------------------------ heap */

/* An array-based binary max-heap over node ids, hand-rolled because the
 * frontier's access pattern is exactly a heap's - pop the maximum, push a
 * handful of replacements - and because a dependency for thirty lines of
 * sift-up and sift-down would be a poor trade. A sorted array would turn
 * every split into an O(K) insertion; a full re-sort per iteration would put
 * the arena-free cost argument back on the floor. */

type heap = {
  mutable items: array(Store.id),
  mutable len: int,
};

let heapCreate = (capacity: int): heap => {
  items: Array.make(max(1, capacity), Store.noId),
  len: 0,
};

let heapSwap = (h: heap, i: int, j: int): unit => {
  let tmp = h.items[i];
  h.items[i] = h.items[j];
  h.items[j] = tmp;
};

let heapPush =
    (h: heap, ~before: (Store.id, Store.id) => bool, id: Store.id): unit => {
  if (h.len == Array.length(h.items)) {
    let bigger = Array.make(h.len * 2, Store.noId);
    Array.blit(h.items, 0, bigger, 0, h.len);
    h.items = bigger;
  };
  h.items[h.len] = id;
  h.len = h.len + 1;
  let i = ref(h.len - 1);
  let settled = ref(false);
  while (! settled^ && i^ > 0) {
    let parent = (i^ - 1) / 2;
    if (before(h.items[i^], h.items[parent])) {
      heapSwap(h, i^, parent);
      i := parent;
    } else {
      settled := true;
    };
  };
};

/* Caller must check `len > 0`. */
let heapPop = (h: heap, ~before: (Store.id, Store.id) => bool): Store.id => {
  let top = h.items[0];
  h.len = h.len - 1;
  h.items[0] = h.items[h.len];
  let i = ref(0);
  let settled = ref(false);
  while (! settled^) {
    let left = 2 * i^ + 1;
    let right = left + 1;
    let best = ref(i^);
    if (left < h.len && before(h.items[left], h.items[best^])) {
      best := left;
    };
    if (right < h.len && before(h.items[right], h.items[best^])) {
      best := right;
    };
    if (best^ == i^) {
      settled := true;
    } else {
      heapSwap(h, i^, best^);
      i := best^;
    };
  };
  top;
};

/* ------------------------------------------------------------- the survey */

/* ONE pass over a node's contiguous child range, answering the only two
 * questions the split predicate asks: how many children are candidates at or
 * above the threshold (A), and how big the largest of those is (m).
 *
 * No intermediate array and no sort, deliberately. This runs for every node
 * the frontier considers - including every node it then emits whole, where
 * the answer is thrown away - so it has to be allocation-free.
 *
 * m is the largest of the ABOVE-THRESHOLD CANDIDATES, not the largest child.
 * Dominance is the claim "splitting N hands you a child that stands for
 * almost all of it", and a child we would refuse to emit cannot support that
 * claim. */
let surveyChildrenWith =
    (
      t: Store.t,
      id: Store.id,
      ~eligible: Store.node => bool,
      ~threshold: int,
    )
    : (int, int) => {
  let count = ref(0);
  let largest = ref(0);
  Store.iterChildren(
    t,
    id,
    child => {
      let n = Store.get(t, child);
      if (eligible(n) && n.Store.size >= threshold) {
        incr(count);
        if (n.Store.size > largest^) {
          largest := n.Store.size;
        };
      };
    },
  );
  (count^, largest^);
};

/* ---------------------------------------------------------- the frontier */

let frontierWith =
    (
      t: Store.t,
      ~eligible: Store.node => bool,
      ~root: Store.id,
      ~params: params,
    )
    : array(Store.id) => {
  let rootNode = Store.get(t, root);
  let maxRows = max(0, params.maxRows);
  let limit = threshold(~total=rootNode.Store.size, ~params);
  if (maxRows == 0) {
    [||];
  } else {
    let before = (a, b) => compareNodes(t, a, b) < 0;
    let heap = heapCreate(64);
    let push = id => heapPush(heap, ~before, id);
    let pushCandidateChildren = id =>
      Store.iterChildren(
        t,
        id,
        child => {
          let n = Store.get(t, child);
          if (eligible(n) && n.Store.size >= limit) {
            push(child);
          };
        },
      );

    /* Seed with the root's children. The root itself is never pushed, which
       is the whole reason the seeding is a separate step rather than "start
       the loop with the root in the heap". */
    pushCandidateChildren(root);

    /* A root wide enough to overflow the budget on its own would otherwise
       make every later split fail its budget check and emit hundreds of rows
       to be truncated at the end. Keep the largest maxRows and drop the rest
       up front, so |S| <= maxRows is an invariant of the loop rather than a
       fact about its output. */
    if (heap.len > maxRows) {
      let keep = Array.init(maxRows, _ => heapPop(heap, ~before));
      heap.len = 0;
      Array.iter(push, keep);
    };

    let emitted = ref([]);
    let emittedCount = ref(0);
    while (heap.len > 0) {
      /* |S| counting the node we just took out. */
      let members = emittedCount^ + heap.len;
      let n = heapPop(heap, ~before);
      let node = Store.get(t, n);
      let deep = node.Store.depth - rootNode.Store.depth >= params.maxDepth;
      let splittable = node.Store.kind == Store.Dir && ! deep;
      let (a, m) =
        splittable ? surveyChildrenWith(t, n, ~eligible, ~threshold=limit) : (0, 0);
      let split =
        splittable
        && a >= 1
        && (a <= params.fanoutCap || m * 2 >= node.Store.size)
        && members - 1 + a <= maxRows;
      if (split) {
        pushCandidateChildren(n);
      } else {
        emitted := [n, ...emitted^];
        incr(emittedCount);
      };
    };

    let rows = Array.of_list(emitted^);
    Array.sort(compareNodes(t), rows);
    /* The loop invariant already bounds this; the truncation is here because
       a budget the caller asked for must hold unconditionally, not because
       one held last time the predicate was read carefully. */
    Array.length(rows) > maxRows ? Array.sub(rows, 0, maxRows) : rows;
  };
};

/* ------------------------------------------------- the public entry points */

/* Two named entry points rather than one with an optional flag: an optional
 * argument only ERASES when a positional parameter follows it, and both of
 * these take nothing but labels. An optional here would silently turn
 * `frontier(t, ~root, ~params)` into a closure at every call site rather
 * than an array - and two names read better than a boolean anyway. */
let frontier = (t: Store.t, ~root: Store.id, ~params: params): array(Store.id) =>
  frontierWith(t, ~eligible=isCandidate, ~root, ~params);

/* With reveal (`I`) on: an ignored node that was MEASURED before being
   ignored takes its real place again. One that was pruned during the walk
   has no size at all and simply falls below the threshold, which is the
   honest outcome rather than a special case. */
let frontierRevealed =
    (t: Store.t, ~root: Store.id, ~params: params): array(Store.id) =>
  frontierWith(t, ~eligible=isCandidateRevealed, ~root, ~params);

/* The survey helper, unchanged, so tests can assert WHY a node split. */
let surveyChildren = (t: Store.t, id: Store.id, ~threshold: int): (int, int) =>
  surveyChildrenWith(t, id, ~eligible=isCandidate, ~threshold);

/* ------------------------------------------------------- the drill-down */

/* A node's immediate children, size descending, name as the tie-break.
 *
 * Deliberately NOT the frontier: plain and complete, because once the user
 * has descended into a directory they want a file manager, not an opinion.
 * Nothing is collapsed, nothing is thresholded, and other-device and
 * hard-link entries stay visible - a zero-sized row that explains itself is
 * the answer to "why is this number so small". Only ignored entries are
 * filtered, and only until the user asks for them. */
let sortedChildren =
    (t: Store.t, id: Store.id, ~showIgnored: bool): array(Store.id) => {
  let kept = ref([]);
  Store.iterChildren(
    t,
    id,
    child =>
      if (showIgnored
          || !Store.hasFlag(Store.get(t, child).Store.flags, Store.fIgnored)) {
        kept := [child, ...kept^];
      },
  );
  let rows = Array.of_list(kept^);
  Array.sort(compareNodes(t), rows);
  rows;
};
