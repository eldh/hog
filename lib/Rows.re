/*
 * The list row builder.
 *
 * The list is handed to matcha as `<ScrollView rows />`, so what this module
 * produces is an `array(string)`: one finished, fully styled terminal line
 * per entry, exactly `width` cells wide.
 *
 *     > 1.2G ███████▌ Library/Caches/com.apple.WebKit/
 *        412M ██▊     Movies/2019-summer/
 *
 * STYLE SELF-CONTAINMENT IS THE LOAD-BEARING CONSTRAINT
 * ----------------------------------------------------
 * `~rows` mode lets the runtime start painting at row N without ever looking
 * at rows 0..N-1. Nothing checks that the rows can survive that, and a row
 * which inherits its colour from the row above renders correctly until the
 * moment the user scrolls past it. So every row here opens every style it
 * uses and ends with a reset.
 *
 * The non-obvious consequence is about WHOLE-ROW styles: the selection's
 * `Inverted` cannot be opened once at the start of the row, because every
 * piece after it that opens a style of its own resets back to plain when it
 * closes. A whole-row style has to be carried in EVERY piece's style list.
 * That is what `tier` below is for.
 *
 * WHY THE BAKING IS LAZY
 * ----------------------
 * One row costs a couple of microseconds, so per-row cost is not the
 * argument. The argument is that the row set is rebuilt from scratch on
 * every published scan snapshot - ten times a second, for a minute - and
 * again on every filter keystroke. Baking a 100k-entry directory eagerly is
 * a couple of hundred milliseconds per snapshot, i.e. a UI frozen solid for
 * the whole scan. Lazily it is one Array.make, one Bytes.make and the ~50
 * rows actually on screen.
 *
 * BOTH ensure* FUNCTIONS MUST BE IDEMPOTENT. matcha renders the tree twice
 * per frame - a measuring pass over an Auto child, then the real one - and
 * both passes run the render body, so both call these. A second call must
 * leave the array byte-identical.
 */
open Matcha;

/* -------------------------------------------------------------- geometry */

/* One cell: '>' selected, 'x' ignored, blank otherwise. Always present, so
 * that the size column starts at the same place on every row. */
let caretWidth = 1;

/* Fmt.sizeWidth is the widest humanSize can return; the extra cell is the
 * sparse-file marker, which sits immediately in front of the number. It
 * lives INSIDE the field rather than beside it so that a sparse row and an
 * ordinary row put their digits in the same columns - a size column that
 * shifted by one on some rows would defeat the whole point of the column. */
let sizeField = Fmt.sizeWidth + 1;

/* Eight cells of eighth-blocks. They earn their width: the entire task is
 * comparative magnitude, and `███████▌` against `██▊` is perception where
 * `1.2G` against `412M` is arithmetic. */
let barCells = 8;

/* Below this the bar and its separating space are dropped and the path gets
 * the room. Derived, not picked: with the bar a row spends 17 cells on
 * chrome, and under ~39 cells of path even a scope-relative name stops
 * being readable. */
let barMinWidth = 56;

let showBar = (width: int): bool => width >= barMinWidth;

/* Everything that is not the path. */
let chromeWidth = (width: int): int =>
  caretWidth + sizeField + 1 + (showBar(width) ? barCells + 1 : 0);

/* ---------------------------------------------------------------- styles */

/* The leading directories of a path. Grey rather than Dim because Dim is
 * already spent on the size and the bar, and the two tiers have to be
 * distinguishable from each other on the same row. */
let pathFg = Element.FgColor(Element.BrightBlack);

let ansi = Element.styleToAnsi;

/* Concatenate styled pieces, closing each styled one immediately. Copied
 * from bdiff, where it exists for exactly this reason. */
let renderPieces = (pieces: list((string, list(Element.style)))): string => {
  let buf = Buffer.create(160);
  List.iter(
    ((text, styles)) =>
      /* An empty piece is dropped rather than wrapped: a zero-width run
       * happens on every row (no leading directories, nothing to pad) and
       * an escape pair around nothing is bytes the frame differ has to
       * compare on every keystroke for no pixel at all. */
      if (text != "") {
        List.iter(st => Buffer.add_string(buf, ansi(st)), styles);
        Buffer.add_string(buf, text);
        if (styles != []) {
          Buffer.add_string(buf, Element.resetAnsi);
        };
      },
    pieces,
  );
  /* An unconditional trailing reset. The last piece of a row is usually the
   * padding, which on an unselected row carries no styles and so closes
   * nothing - and then the row would end with whatever the piece before it
   * left, which is the one thing `~rows` mode cannot tolerate. Costs four
   * bytes; removes a whole class of scroll-only bug. */
  Buffer.add_string(buf, Element.resetAnsi);
  Buffer.contents(buf);
};

/* Keep the END of a string when it does not fit, ANSI- and UTF-8-aware.
 *
 * Routed through StyledText even when the string already fits, for two
 * reasons: a multi-byte name cut with String.sub puts a replacement
 * character on screen, and parse/bake keeps only the FIRST line, so a
 * filename containing a newline - which every filesystem hog walks allows -
 * cannot smuggle a second line into a one-line row. */
let truncateStart = (s: string, w: int): string =>
  switch (StyledText.parse(s)) {
  | [] => ""
  | [line, ..._] => StyledText.bake([StyledText.truncateStartLine(line, w)])
  };

/* Split the cell range [from, to_) into MAXIMAL runs of equal `hit` and emit
 * one piece per run. Runs rather than one piece per cell because a per-cell
 * emission wraps every character in its own escape pair, which is several
 * times the bytes for the frame differ to chew through on each keystroke.
 * Cutting on CELLS is what makes the split safe - a run boundary can never
 * land inside a UTF-8 sequence. */
let styledRuns =
    (
      ~cells: array(TextWidth.cell),
      ~from: int,
      ~to_: int,
      ~hit: array(bool),
      ~normal: list(Element.style),
      ~highlighted: list(Element.style),
    )
    : list((string, list(Element.style))) => {
  let out: ref(list((string, list(Element.style)))) = ref([]);
  let i = ref(max(0, from));
  let stop = min(to_, Array.length(cells));
  while (i^ < stop) {
    let h = hit[i^];
    let buf = Buffer.create(16);
    let j = ref(i^);
    while (j^ < stop && hit[j^] == h) {
      Buffer.add_string(buf, cells[j^].TextWidth.bytes);
      incr(j);
    };
    out := [(Buffer.contents(buf), h ? highlighted : normal), ...out^];
    i := j^;
  };
  List.rev(out^);
};

/* ------------------------------------------------------------ the columns */

/* The size cell, right-aligned in `sizeField`.
 *
 * An ignored or off-volume entry shows "-" and NOT "0B": we never measured
 * it, and a zero would be read as "this is empty, why is it in my way". */
let sizeText = (n: Store.node): string => {
  let flags = n.Store.flags;
  if (Store.hasFlag(flags, Store.fIgnored)
      || Store.hasFlag(flags, Store.fOtherDev)) {
    "-";
  } else if (Store.hasFlag(flags, Store.fSparseSuspect)) {
    /* The apparent size very probably exceeds what the file allocates, so
     * the number is an upper bound and says so. */
    "~" ++ Fmt.humanSize(n.Store.size);
  } else {
    Fmt.humanSize(n.Store.size);
  };
};

/* Replace the control bytes in a name with '?'.
 *
 * Every byte except '/' and NUL is legal in a filename, and hog is pointed
 * at directories nobody has read. An ESC in a name would hand the rest of
 * the row - and every row after it - to whatever SGR state that name felt
 * like setting, which is style injection from untrusted data; a newline
 * would turn one row into two and desynchronise the whole list from its
 * offsets. Replaced rather than dropped, so a name is never silently
 * shortened into something that looks like a different file.
 *
 * The scan-first pass is so that the common case - no control bytes at all -
 * allocates nothing. Byte-wise is correct here because every byte of a
 * multi-byte UTF-8 sequence is >= 0x80. */
let isControl = (c: char): bool => {
  let n = Char.code(c);
  n < 0x20 || n == 0x7f;
};

let sanitize = (s: string): string => {
  let dirty = ref(false);
  String.iter(
    c =>
      if (isControl(c)) {
        dirty := true;
      },
    s,
  );
  dirty^ ? String.map(c => isControl(c) ? '?' : c, s) : s;
};

/* The path as the row shows it: relative to the current scope, with a
 * trailing '/' on directories.
 *
 * Scope-relative is what removes most of the eliding problem in the first
 * place. An entry that is NOT under the scope - which the landing view can
 * produce while a rescan re-roots the tree - keeps its absolute path, and
 * that one gets the home directory shortened to "~" instead. */
let pathText =
    (~store: Store.t, ~id: Store.id, ~scopePath: string, ~home: string)
    : string => {
  let abs = Store.path(store, id);
  let rel = Fmt.relativeTo(~base=scopePath, abs);
  let shown = sanitize(rel == abs ? Fmt.tildify(~home, abs) : rel);
  Store.get(store, id).Store.kind == Store.Dir ? shown ++ "/" : shown;
};

/*
 * One finished row, in exactly `max(0, width)` cells. Pure: everything it
 * needs is an argument, which is what lets the tests assert on rows without
 * a ScrollView, a runtime or a filesystem anywhere near them.
 *
 * `maxSize` scales the bar, and is the largest size among the entries
 * CURRENTLY LISTED rather than the tree total: after descending into a
 * directory, or filtering, the comparison the user is making is between the
 * rows in front of them.
 */
let rowText =
    (
      ~store: Store.t,
      ~id: Store.id,
      ~width: int,
      ~maxSize: int,
      ~query: string,
      ~scopePath: string,
      ~home: string,
      ~selected: bool,
    )
    : string => {
  let width = max(0, width);
  if (width == 0) {
    "";
  } else {
    let node = Store.get(store, id);
    let ignored = Store.hasFlag(node.Store.flags, Store.fIgnored);

    /* THE SELECTED ROW IS ONE COLOUR. The tiers below - grey directories,
     * dim size, dim bar - exist to guide the eye down the rows you are NOT
     * on. On the row you are on they only muddy it: under inverted video a
     * grey directory next to its own default-coloured basename reads as
     * half-selected, and the row comes out patchy. So on a selected row
     * every tier collapses to the selection style and the row is uniform.
     * (The match highlight stays - it answers "why is this row in the
     * filtered list", which is information rather than decoration.) */
    let sel = selected ? [Element.Inverted] : [];
    let tier = (extra: list(Element.style)) => selected ? sel : sel @ extra;
    let highlighted = sel @ [Element.Bold];

    let caret = selected ? ">" : ignored ? "x" : " ";

    let size = sizeText(node);
    let sizePad = String.make(max(0, sizeField - Element.visibleLength(size)), ' ');

    let bar =
      showBar(width)
        ? Fmt.bar(~value=max(0, node.Store.size), ~max=maxSize, ~cells=barCells)
        : "";

    let pathW = max(0, width - chromeWidth(width));
    let path =
      truncateStart(pathText(~store, ~id, ~scopePath, ~home), pathW);
    let cells = TextWidth.toCells(path);
    let n = Array.length(cells);

    /* Everything through the last '/' is directory, EXCEPT a '/' in the
     * final cell - that one is the directory marker this function just
     * appended, and it belongs to the basename it marks. Without the
     * exception every directory row would be entirely grey. */
    let dirEnd = ref(0);
    for (i in 0 to n - 1) {
      if (cells[i].TextWidth.bytes == "/" && i < n - 1) {
        dirEnd := i + 1;
      };
    };

    /* Positions come from the ELIDED path, not the full one: the cells the
     * runs are cut on are these cells, so a match scored against the
     * pre-elision string would highlight the wrong ones - or cells that are
     * no longer on the row at all. */
    let hit = Array.make(max(1, n), false);
    if (query != "") {
      switch (Fuzzy.match_(~query, ~text=path)) {
      | Some(m) =>
        List.iter(
          i =>
            if (i >= 0 && i < n) {
              hit[i] = true;
            },
          m.Fuzzy.positions,
        )
      | None => ()
      };
    };

    /* Padding to the exact width. It is not only for a short path: eliding
     * to a budget that would split a double-width cell comes back one cell
     * narrow, and the columns of every row below would be off by one. */
    let used =
      caretWidth
      + sizeField
      + 1
      + (showBar(width) ? Element.visibleLength(bar) + 1 : 0)
      + Element.visibleLength(path);
    let pad = String.make(max(0, width - used), ' ');

    let row =
      renderPieces(
        [(caret, sel), (sizePad, sel), (size, tier([Element.Dim])), (" ", sel)]
        @ (
          showBar(width)
            ? [(bar, tier([Element.Dim])), (" ", sel)] : []
        )
        @ styledRuns(
            ~cells,
            ~from=0,
            ~to_=dirEnd^,
            ~hit,
            ~normal=tier([pathFg]),
            ~highlighted,
          )
        @ styledRuns(~cells, ~from=dirEnd^, ~to_=n, ~hit, ~normal=sel, ~highlighted)
        @ [(pad, sel)],
      );
    /* The only way to overrun is a width narrower than the chrome itself,
     * where the columns cannot all fit however they are arranged. Cutting
     * from the front keeps the path, which is the half a reader can still
     * use at that size. The reset is re-applied because the cut goes
     * through parse/bake, which drops the one renderPieces appended when
     * the surviving cells happen to carry no styles. */
    Element.visibleLength(row) > width
      ? truncateStart(row, width) ++ Element.resetAnsi : row;
  };
};

/* ------------------------------------------------------------ the row set */

/* `rows` is handed straight to <ScrollView rows /> and its identity never
 * changes for the life of a `t`, so lazily filling a slot shows up on the
 * next frame with no rebuild anywhere.
 *
 * `baked` is one byte per row: '\000' not baked, '\001' baked plain,
 * '\002' baked selected. Three states rather than a bool because
 * ensureRange must skip the selected row instead of flattening it back. */
type t = {
  store: Store.t,
  entries: array(Store.id),
  width: int,
  query: string,
  scopePath: string,
  home: string,
  maxSize: int,
  rows: array(string),
  baked: Bytes.t,
  mutable selected: int,
};

let notBaked = '\000';
let bakedPlain = '\001';
let bakedSelected = '\002';

let build =
    (
      ~store: Store.t,
      ~entries: array(Store.id),
      ~width: int,
      ~query: string,
      ~scopePath: string,
      ~home: string,
    )
    : t => {
  let n = Array.length(entries);
  /* The bar is scaled to the largest entry in this list, computed once here
   * rather than per row - it is the only value a row needs that it cannot
   * read off its own node. */
  let maxSize = ref(0);
  Array.iter(
    id => {
      let s = Store.get(store, id).Store.size;
      if (s > maxSize^) {
        maxSize := s;
      };
    },
    entries,
  );
  {
    store,
    entries,
    width,
    query,
    scopePath,
    home,
    maxSize: maxSize^,
    rows: Array.make(n, ""),
    baked: Bytes.make(n, notBaked),
    selected: (-1),
  };
};

let rows = (t: t): array(string) => t.rows;

let count = (t: t): int => Array.length(t.entries);

let bake = (t: t, i: int, ~selected: bool): string =>
  rowText(
    ~store=t.store,
    ~id=t.entries[i],
    ~width=t.width,
    ~maxSize=t.maxSize,
    ~query=t.query,
    ~scopePath=t.scopePath,
    ~home=t.home,
    ~selected,
  );

/* Fill [fromRow, toRow), skipping anything already baked. Idempotent by
 * construction: a second call finds every flag set and writes nothing. */
let ensureRange = (t: t, ~fromRow: int, ~toRow: int): unit => {
  let n = count(t);
  let first = max(0, fromRow);
  let last = min(n, toRow);
  for (i in first to last - 1) {
    if (Bytes.get(t.baked, i) == notBaked) {
      t.rows[i] = bake(t, i, ~selected=false);
      Bytes.set(t.baked, i, bakedPlain);
    };
  };
};

/*
 * Two writes and no invalidation: put the previously selected row back to
 * its plain form, bake the new one selected.
 *
 * Always called AFTER ensureRange, so a window bake can never clobber the
 * selection - and it does not need to, since ensureRange skips any row whose
 * flag is set, selected included.
 *
 * The guard is what makes this idempotent. The measuring pass and the paint
 * pass of the same frame both run the render body and so both call this with
 * the same index; the second call must produce byte-identical rows.
 */
let ensureSelected = (t: t, ~sel: int): unit => {
  let n = count(t);
  let inRange = i => i >= 0 && i < n;
  let alreadyRight =
    t.selected == sel
    && (! inRange(sel) || Bytes.get(t.baked, sel) == bakedSelected);
  if (! alreadyRight) {
    if (inRange(t.selected) && t.selected != sel) {
      t.rows[t.selected] = bake(t, t.selected, ~selected=false);
      Bytes.set(t.baked, t.selected, bakedPlain);
    };
    if (inRange(sel)) {
      t.rows[sel] = bake(t, sel, ~selected=true);
      Bytes.set(t.baked, sel, bakedSelected);
    };
    t.selected = sel;
  };
};
