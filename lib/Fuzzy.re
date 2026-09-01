/*
 * Fuzzy - the scorer behind hog's `/` filter.
 *
 * A query matches a path when its characters appear in the path IN ORDER but
 * not necessarily together ("bt" finds "Button.tsx"). Of the many ways a
 * query can be threaded through a path, one is the RIGHT one to show: the
 * alignment a reader would have meant. That is what the score picks out, and
 * the positions it comes back with are what the file list paints amber.
 *
 * This is a direct port of bdiff's lib/Fuzzy.re - same scoring, same DP,
 * same contract - because hog's filter is the same problem (rank fuzzy
 * matches of a query against file paths with cell-accurate highlight
 * positions) applied to a different list.
 *
 * WHY A DP AND NOT A GREEDY WALK
 * ------------------------------
 * The obvious implementation - take the leftmost occurrence of each query
 * character in turn - answers "bt" against "packages/bar/Toolbar.tsx" with
 * the 'b' of "bar", and then no amount of local repair recovers the
 * "Button"-style alignment a reader wanted. Trying every start position
 * instead is O(text * query^2). The recurrence below is exactly
 * O(text * query) because the gap penalty is LINEAR in the number of skipped
 * cells, which turns "max over every predecessor" into a running prefix
 * maximum.
 *
 * The greedy walk is still here, as the FEASIBILITY pre-filter: it answers
 * "no such subsequence" in O(text) with zero allocation, and on a real
 * directory tree that is the answer for most paths.
 *
 * WHY CELLS AND NOT BYTES
 * -----------------------
 * The positions are handed to a renderer that splits the path into styled
 * runs. Splitting on a byte index would cut a UTF-8 sequence in half and put
 * a replacement character on screen, so everything here counts TERMINAL
 * CELLS (TextWidth.toCells) - the same unit Matcha lays out in. A combining
 * accent travels with the character it sits on, because toCells fuses it.
 *
 * WHY "THE FILENAME WINS" IS NOT A SPECIAL CASE
 * ---------------------------------------------
 * It falls out of two constants. The first matched cell pays a leading
 * penalty for how deep into the path it is, but that penalty is CAPPED at 12
 * - a deeply nested path is not twenty times worse than a shallow one - while
 * every cell of the basename earns +20. So a query landing on the filename
 * of a deeply nested entry still beats the same query landing on a directory
 * component of a shallow one, and it beats it by more the longer the query
 * is. Nothing anywhere reads "is this the basename" as a branch.
 *
 * Everything in here is pure and total: no exceptions, no allocation beyond
 * the two score rows, and match_ never raises on malformed UTF-8 (toCells
 * decodes invalid bytes as U+FFFD).
 */
open Matcha;

/* The constants, at module level so the tests can reason about them rather
   than pinning magic numbers. Their RATIOS are the design; the absolute
   scale is arbitrary. */

/* Paid once per matched cell, so a longer query always outscores a shorter
   one on the same path. */
let matchScore = 16;

/* Paid when a matched cell sits immediately after the previous matched one.
   "abc" typed against "abc" is worth more than against "a-b-c". */
let consecutiveBonus = 8;

/* A cell that starts a "word": index 0, the cell after / _ - . or a space,
   or the upper-case half of a camelCase transition. This is what makes "fb"
   pick out the F and the B of "FloatingButton". */
let boundaryBonus = 12;

/* Every cell of the FILENAME earns this, on top of anything else. Paid per
   cell, not once, so it grows with the query - see the header. */
let basenameBonus = 20;

/* Per cell SKIPPED between two matched cells. Linear on purpose: it is what
   makes the running-prefix-maximum trick correct, and a quadratic penalty
   would make one long gap catastrophically worse than two medium ones for
   no reason a reader would recognise. */
let gapPenaltyPerCell = 3;

/* Per cell skipped BEFORE the first match, capped just below the basename
   bonus of a single cell. The cap is the whole point: it says "deep is
   slightly worse", not "deep is hopeless". */
let leadingPenaltyPerCell = 1;
let maxLeadingPenalty = 12;

/* The whole query present, contiguous, inside the filename. Applied AFTER
   the alignment is chosen - it is a tie-break BETWEEN paths, and letting it
   steer the DP would make it fight the per-cell bonuses. */
let wholeInBasenameBonus = 48;

/* "No alignment reaches here." Never added to, only compared against, so it
   cannot overflow. min_int/4 leaves three quarters of the range as headroom
   for real scores, which are bounded by a few dozen per cell. */
let none = min_int / 4;

/* A successful match: its score, and the CELL indices of the matched cells
   in ascending order. */
type m = {
  score: int,
  positions: list(int),
};

let isUpperCell = (s: string): bool =>
  String.length(s) == 1 && s.[0] >= 'A' && s.[0] <= 'Z';

let isLowerOrDigitCell = (s: string): bool =>
  String.length(s) == 1
  && (s.[0] >= 'a' && s.[0] <= 'z' || s.[0] >= '0' && s.[0] <= '9');

/* The cells a new "word" can start after. '/' is here because a path
   component boundary is the strongest one there is. */
let isSeparatorCell = (s: string): bool =>
  switch (s) {
  | "/"
  | "_"
  | "-"
  | "."
  | " " => true
  | _ => false
  };

/* ASCII-lowercase a cell. A multi-byte cell is returned untouched: case
   folding outside ASCII is locale business, and a terminal file filter has
   no business guessing at it. */
let lowerCell = (s: string): string =>
  if (isUpperCell(s)) {
    String.make(1, Char.lowercase_ascii(s.[0]));
  } else {
    s;
  };

/*
 * Score [query] against [text], or None if [text] does not contain the
 * query's cells as a subsequence at all.
 *
 * SMART CASE: a query with any upper-case cell in it is matched
 * case-SENSITIVELY (you typed the capital, you meant it); an all-lower-case
 * query matches either case.
 *
 * An empty query matches everything with score 0 and no positions, which is
 * what makes "the filter is off" the same code path as "the filter is on".
 */
let match_ = (~query: string, ~text: string): option(m) => {
  let qcells = TextWidth.toCells(query);
  let tcells = TextWidth.toCells(text);
  let qn = Array.length(qcells);
  let tn = Array.length(tcells);
  if (qn == 0) {
    Some({score: 0, positions: []});
  } else if (qn > tn) {
    None;
  } else {
    let caseSensitive =
      Array.exists(c => isUpperCell(c.TextWidth.bytes), qcells);
    let norm = (c: TextWidth.cell) =>
      caseSensitive ? c.TextWidth.bytes : lowerCell(c.TextWidth.bytes);
    let q = Array.map(norm, qcells);
    let t = Array.map(norm, tcells);

    /* THE FAST PATH. One left-to-right walk says whether any alignment
       exists at all; most paths in a real tree die here, before a single
       array is allocated. */
    let seen = ref(0);
    for (ti in 0 to tn - 1) {
      if (seen^ < qn && q[seen^] == t[ti]) {
        incr(seen);
      };
    };
    if (seen^ < qn) {
      None;
    } else {
      /* One past the last '/' - where the filename starts. A path with no
         '/' is all filename. */
      let baseStart = ref(0);
      for (i in 0 to tn - 1) {
        if (tcells[i].TextWidth.bytes == "/") {
          baseStart := i + 1;
        };
      };

      /* Everything a cell is worth REGARDLESS of what matched before it. */
      let bonus = Array.make(tn, 0);
      for (i in 0 to tn - 1) {
        let cur = tcells[i].TextWidth.bytes;
        let isBoundary =
          i == 0
          || isSeparatorCell(tcells[i - 1].TextWidth.bytes)
          || isUpperCell(cur)
          && isLowerOrDigitCell(tcells[i - 1].TextWidth.bytes);
        bonus[i] =
          (isBoundary ? boundaryBonus : 0)
          + (i >= baseStart^ ? basenameBonus : 0);
      };

      /*
       * h[qi * tn + ti] = the best total score of an alignment of the first
       * qi+1 query cells that matches query cell qi AT text cell ti.
       * parent[...] is the text cell that query cell qi-1 matched, so the
       * positions can be read back afterwards.
       *
       * Two candidate predecessors, and that is the whole recurrence:
       *
       *   A. the running prefix maximum over every tj < ti. Because the gap
       *      cost is gap * (ti - tj - 1), the tj-dependent part factors out
       *      as (h[qi-1][tj] + gap*tj), which can be maintained in O(1) per
       *      cell; the ti-dependent part, - gap*(ti-1), is subtracted at the
       *      end.
       *
       *   B. tj = ti - 1 specifically, which is the only predecessor that
       *      can collect consecutiveBonus. It is already inside A's maximum
       *      (with a zero gap), so B only ever wins by that bonus - and A
       *      keeps the tie, which prefers the alignment that started
       *      earlier.
       */
      let h = Array.make(qn * tn, none);
      let parent = Array.make(qn * tn, -1);

      for (ti in 0 to tn - 1) {
        if (q[0] == t[ti]) {
          h[ti] =
            matchScore
            + bonus[ti]
            - min(maxLeadingPenalty, leadingPenaltyPerCell * ti);
        };
      };

      for (qi in 1 to qn - 1) {
        let prev = (qi - 1) * tn;
        let cur = qi * tn;
        /* The running maximum of (h[qi-1][tj] + gap*tj) over tj < ti, and
           the tj that achieves it. bestIdx < 0 means "no predecessor yet",
           which is why bestVal is never read before it is written. */
        let bestVal = ref(0);
        let bestIdx = ref(-1);
        for (ti in 0 to tn - 1) {
          if (ti >= 1 && h[prev + ti - 1] != none) {
            let v = h[prev + ti - 1] + gapPenaltyPerCell * (ti - 1);
            if (bestIdx^ < 0 || v > bestVal^) {
              bestVal := v;
              bestIdx := ti - 1;
            };
          };
          if (bestIdx^ >= 0 && q[qi] == t[ti]) {
            let candA = bestVal^ - gapPenaltyPerCell * (ti - 1);
            let adjacent = ti >= 1 && h[prev + ti - 1] != none;
            let candB = adjacent ? h[prev + ti - 1] + consecutiveBonus : none;
            let (best, par) =
              adjacent && candB > candA
                ? (candB, ti - 1) : (candA, bestIdx^);
            h[cur + ti] = matchScore + bonus[ti] + best;
            parent[cur + ti] = par;
          };
        };
      };

      /* The end cell, chosen with a STRICT >, so among equally good
         alignments the leftmost one wins. */
      let last = (qn - 1) * tn;
      let endTi = ref(-1);
      let bestEnd = ref(none);
      for (ti in 0 to tn - 1) {
        if (h[last + ti] != none && (endTi^ < 0 || h[last + ti] > bestEnd^)) {
          bestEnd := h[last + ti];
          endTi := ti;
        };
      };

      if (endTi^ < 0) {
        /* Unreachable: the greedy pre-filter already proved an alignment
           exists, and the DP considers every one of them. Answering None
           rather than raising keeps the function total. */
        None;
      } else {
        let positions = ref([]);
        let ti = ref(endTi^);
        for (k in 0 to qn - 1) {
          let qi = qn - 1 - k;
          positions := [ti^, ...positions^];
          if (qi > 0) {
            ti := parent[qi * tn + ti^];
          };
        };

        /* Contiguous whole-query hit inside the filename. Deliberately
           checked against the text rather than read off the positions: the
           DP may well have chosen a different (equally scoring) alignment,
           and what earns the bonus is that the filename CONTAINS the query,
           not which cells were painted. */
        let whole = ref(false);
        for (s in baseStart^ to tn - qn) {
          if (! whole^) {
            let ok = ref(true);
            for (k in 0 to qn - 1) {
              if (q[k] != t[s + k]) {
                ok := false;
              };
            };
            if (ok^) {
              whole := true;
            };
          };
        };

        Some({
          score: bestEnd^ + (whole^ ? wholeInBasenameBonus : 0),
          positions: positions^,
        });
      };
    };
  };
};
