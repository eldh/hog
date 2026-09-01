/*
 * hog's user interface.
 *
 * One component, started by bin/main.re through Runtime.start and by
 * test/app_tests.re through Runtime.startHeadless. There is no second,
 * test-only copy: `app` takes the scan handle and the trash function as
 * PARAMETERS, so a test swaps in a stepped handle and a recording stub and
 * every other call the UI makes is the real one.
 *
 * ============================ FULLSCREEN, NOT INLINE ======================
 *
 * The root Flexes to fill the terminal, so this app must run on the
 * alternate screen. An inline app that is as tall as the terminal forces the
 * terminal to SCROLL to make room, and nothing can un-scroll a terminal: the
 * user's shell history is gone. The consequence to remember while editing
 * this file is that <Static> and useStdout RAISE under Fullscreen - there is
 * no scrollback to commit to - so every message the app wants to show lives
 * in state and is rendered.
 *
 * ============================ WHY THE ROOT IS NOT A COMPONENT =============
 *
 * The root body below is the module's own `make`, not a [@component] wrapped
 * in Element.createComponent. That is deliberate and load-bearing for the
 * MOUSE. Runtime runs C.make() inside the ROOT render context, and
 * Hooks.dispatchMouse ends with an unconditional fan-out to
 * rootCtx.mouseHandlers - every mouse event reaches the root, in frame
 * coordinates, whatever component is innermost under the pointer. A
 * [@component] root would get its own child context instead, and the
 * <ScrollView> (which registers useMouse for the wheel) would be the
 * innermost target over the list, swallowing every click. Click-to-select
 * would silently never fire.
 *
 * The other half of that arrangement is the root's ~wheel=false: the root
 * declares no wheel interest, so a notch picks the ScrollView as its target
 * and scrolls the list. The root still SEES the notch through the fan-out,
 * and deliberately ignores it - scrolling must not move the selection, or a
 * wheel notch would retarget the `d` key.
 *
 * ============================ IDENTITY IS A PATH ==========================
 *
 * Store ids are indices into an arena that is still being appended to, and
 * they do not survive a rescan. So the SCOPE (which directory is on screen)
 * and the SELECTION are both held as absolute path strings and re-resolved
 * to ids through Scan.resolve every generation. Row indices are derived, and
 * clamped IN THE RENDER - never in an effect, because the list shrinks under
 * the selection on every filter keystroke and an effect would repaint one
 * frame with an out-of-range index first.
 *
 * ============================ MEMO DEPENDENCIES ===========================
 *
 * Hooks.useMemo compares deps by PHYSICAL equality, so every dep in this
 * file is an immediate through Obj.repr: an int or a bool, never an array,
 * a string, a tuple or a record. A freshly allocated dep is never equal to
 * itself and silently turns every memo into a no-op, reintroducing exactly
 * the cost the memo existed to remove. The stand-in for each string-valued
 * piece of state is a GENERATION COUNTER carried in the same useState cell
 * as the value, so a frame can never read a new counter against an old
 * string.
 */
open Matcha;

/* ============================================================================
 * View, dialog and small shared bits
 * ============================================================================ */

/*
 * Landing   - the ranked frontier from the scan root: the biggest things
 *             anywhere, at any depth. What the app opens on.
 * Drill(p)  - p's immediate children, complete and size-ordered. Once you
 *             have descended you want a file manager, not an opinion.
 * Ranked(p) - the frontier re-run rooted at p, which is what `r` toggles.
 */
type view =
  | Landing
  | Drill(string)
  | Ranked(string);

/*
 * The Trash dialog.
 *
 * There is no Working state: Trash.move is a synchronous blocking call, so
 * no frame can be painted between "confirmed" and "finished" and a Working
 * arm would be an unreachable branch pretending otherwise.
 */
type confirm =
  | Closed
  | Confirm(string, int, int, bool) /* absolute path, size, items, isDir */
  | Failed(string, string); /* absolute path, message */

let dot = " \xc2\xb7 "; /* " · " */

/* Named once because the whole point of the warning is that the user can act
   on it. A count of unreadable directories with no fix attached is a tool
   telling you it is lying and refusing to say what to do about it. */
let fdaHint = "System Settings \xe2\x80\xba Privacy & Security \xe2\x80\xba Full Disk Access";

/* Rows already owns both of these, and they must stay the same two functions:
   a second copy of renderPieces that forgot the trailing reset would produce
   rows that look right until something scrolls. */
let renderPieces = Rows.renderPieces;
let truncateStart = Rows.truncateStart;

let dimStyle = [Element.Dim];
let greyStyle = [Element.FgColor(Element.BrightBlack)];

/* Cut or pad a finished one-line row to exactly `width` cells. ANSI-aware,
   so a truncation never lands inside an escape sequence. */
let exactly = (s: string, width: int): string =>
  width <= 0 ? "" : Element.padToWidth(s, width);

/*
 * Hard-wrap into lines of at most `width` cells, at most `maxLines` of them.
 *
 * Cell-based rather than byte-based: an absolute path in a Trash confirmation
 * may hold any byte a filesystem allows, and cutting a multi-byte name with
 * String.sub puts a replacement character in front of the user at the exact
 * moment they are being asked to approve a deletion.
 */
let wrapCells = (s: string, ~width: int, ~maxLines: int): list(string) =>
  if (width <= 0 || maxLines <= 0) {
    [];
  } else {
    let cells = TextWidth.toCells(Element.stripAnsi(s));
    let n = Array.length(cells);
    let out = ref([]);
    let i = ref(0);
    let lines = ref(0);
    while (i^ < n && lines^ < maxLines) {
      let buf = Buffer.create(width * 2);
      let w = ref(0);
      while (i^ < n && w^ + cells[i^].TextWidth.width <= width) {
        Buffer.add_string(buf, cells[i^].TextWidth.bytes);
        w := w^ + cells[i^].TextWidth.width;
        incr(i);
      };
      /* A single cell wider than the whole budget would otherwise spin here
         forever, emitting empty lines. */
      if (Buffer.length(buf) == 0 && i^ < n) {
        incr(i);
      };
      out := [Buffer.contents(buf), ...out^];
      incr(lines);
    };
    List.rev(out^);
  };

/* ============================================================================
 * Creation time
 * ============================================================================ */

/*
 * st_birthtime, which Unix.LargeFile.stats does not expose.
 *
 * Shelled out ONCE PER SELECTION and cached, never per node: a scan visits
 * half a million entries and half a million process spawns is not a feature,
 * it is a fork bomb with a progress bar. `stat -f %B` is macOS-specific and
 * returns a bare integer; anything unexpected is treated as "unknown" rather
 * than guessed at.
 *
 * create_process rather than a shell, so a path containing a quote or a
 * semicolon is an argument and not a command.
 */
let birthTime = (path: string): option(int) => {
  let (r, w) = Unix.pipe();
  let devnull = Unix.openfile("/dev/null", [Unix.O_RDWR], 0);
  switch (
    Unix.create_process("stat", [|"stat", "-f", "%B", path|], devnull, w, devnull)
  ) {
  | exception _ =>
    Unix.close(r);
    Unix.close(w);
    Unix.close(devnull);
    None;
  | pid =>
    Unix.close(w);
    let buf = Bytes.create(64);
    let n =
      switch (Unix.read(r, buf, 0, 64)) {
      | k => k
      | exception _ => 0
      };
    Unix.close(r);
    Unix.close(devnull);
    switch (Unix.waitpid([], pid)) {
    | _ => ()
    | exception _ => ()
    };
    int_of_string_opt(String.trim(Bytes.sub_string(buf, 0, max(0, n))));
  };
};

/* ============================================================================
 * Layout geometry
 * ============================================================================
 *
 * The split breakpoint is DERIVED, not picked, so nothing in this file says
 * "98" and the number moves on its own if the row layout ever changes:
 *
 *   list chrome      caret 1 + size 6 + gap 1 + bar 8 + gap 1   = 17
 *   readable path                                               = 47
 *   the list's scrollbar column                                 =  1
 *   the rule between the panes                                  =  1
 *   the details column                                          = 32
 *                                                               ----
 *                                                                 98
 */

let pathFloor = 47;
let listMinInner = Rows.caretWidth + Rows.sizeField + 1 + Rows.barCells + 1 + pathFloor;
let detailMin = 32;
let splitWidth = listMinInner + 1 + 1 + detailMin;

/* ============================================================================
 * The Trash confirmation - the Modal's CHILD, and so a member of the layer
 * ============================================================================
 *
 * Its keys are useInput, which is what makes them fire while the root's are
 * suppressed. Esc is deliberately absent: <Modal> owns it and calls
 * ~onDismiss, as does a click outside the box.
 */
module ConfirmPane = {
  [@component]
  let make = (~state: confirm, ~onConfirm: unit => unit, ~onCancel: unit => unit) => {
    /* The dialog's own box: an Overlay pushes it as a container, and Modal
       spends 2 columns of border plus a column of padding on each side, and
       one row of border top and bottom. */
    let box = useContainerSize();
    let innerW = max(1, box.Runtime.availWidth - 4);
    let innerH = max(1, box.Runtime.availHeight - 2);

    Hooks.useInput((key, _mods) =>
      switch (key) {
      | Key.Char('y')
      | Key.Char('Y')
      | Key.Enter => onConfirm()
      | Key.Char('n')
      | Key.Char('N') => onCancel()
      | _ => ()
      }
    );

    let lines =
      switch (state) {
      | Closed => []
      | Confirm(path, size, items, isDir) =>
        let what = isDir ? "folder" : "file";
        [renderPieces([("Move this " ++ what ++ " to the Trash?", [Element.Bold])])]
        @ [""]
        /* THE ABSOLUTE PATH, never an elided or ~-shortened one. A
           confirmation that does not name exactly what it is about to move
           is not a confirmation. */
        @ List.map(
            l => renderPieces([(l, [])]),
            wrapCells(path, ~width=innerW, ~maxLines=2),
          )
        @ [""]
        @ [
          renderPieces([
            (Fmt.humanSize(size), [Element.Bold]),
            (
              isDir ? dot ++ Fmt.humanCount(items) ++ " items" : "",
              dimStyle,
            ),
          ]),
        ]
      | Failed(path, msg) =>
        [
          renderPieces([
            ("Could not move it to the Trash", [Element.FgColor(Element.Red)]),
          ]),
        ]
        @ List.map(
            l => renderPieces([(l, greyStyle)]),
            wrapCells(path, ~width=innerW, ~maxLines=1),
          )
        @ [""]
        @ List.map(
            l => renderPieces([(l, [])]),
            wrapCells(msg, ~width=innerW, ~maxLines=3),
          )
      };

    let hint =
      switch (state) {
      | Failed(_, _) =>
        renderPieces([("enter / esc", [Element.Bold]), ("  close", dimStyle)])
      | _ =>
        renderPieces([
          ("y", [Element.Bold]),
          (" move to trash", dimStyle),
          (dot, dimStyle),
          ("n", [Element.Bold]),
          (" cancel", dimStyle),
        ])
      };

    /* The hint is pinned to the last inner row so the dialog does not change
       shape between the Confirm and Failed states. */
    let body = List.filteri((i, _) => i < innerH - 1, lines);
    let filler = List.init(max(0, innerH - 1 - List.length(body)), _ => "");

    <Text> {String.concat("\n", body @ filler @ [hint])} </Text>;
  };
};

/* ============================================================================
 * The application
 * ============================================================================ */

let app =
    (
      ~scan: Scan.handle,
      ~config: Cli.config,
      ~rules: Ignore.rules,
      ~trash: string => result(unit, string),
    )
    : (module Runtime.HooksComponent) => {
  let store = Scan.store(scan);
  let root = Scan.root(scan);
  let home = Scan.home(scan);
  /* --min-size is a display and ranking floor, never a walk filter: filtering
     during the walk would corrupt every parent total. */
  let params = {...Rank.defaults, Rank.minBytes: config.Cli.minSize};

  (module
   {
     let make = (): Element.t => {
       let quit = Event.useQuit();
       let {Runtime.availWidth: width, availHeight: height} = useContainerSize();

       /* ---- state ------------------------------------------------------
          Slot hooks, unconditional and in a fixed order. Every (gen, value)
          pair is one cell on purpose: holding the counter separately would
          let a frame read a new counter against an old value. */
       let (pollGen, setPollGen) = Hooks.useState(0);
       let (viewState, setViewState) = Hooks.useState((0, Landing));
       let (queryState, setQueryState) = Hooks.useState((0, ""));
       let (filterOpen, setFilterOpen) = Hooks.useState(false);
       let (cursor, setCursor) = Hooks.useState((0, 0));
       let (taSel, setTaSel) = Hooks.useState(None: option(TextArea.selection));
       let (selState, setSelState) = Hooks.useState((0, None: option(string)));
       let (offset, setOffset) = Hooks.useState(0);
       /* The rules record plus the paths ignored IN THIS SESSION. The rules
          are what Ignore.addPath edits and writes; the path list is what the
          list filters on, because the arena's fIgnored flags were decided at
          scan time and no rescan is available to redo them. */
       let (ignoreState, setIgnoreState) =
         Hooks.useState((0, rules, []: list(string)));
       let (showIgnored, setShowIgnored) =
         Hooks.useState(config.Cli.showIgnored);
       let (confirm, setConfirm) = Hooks.useState(Closed);
       /* Optimistic removal, so a trashed row disappears on the keystroke
          rather than sitting there until something re-reads the arena. */
       let (trashedState, setTrashedState) =
         Hooks.useState((0, []: list(string)));
       let (notice, setNotice) = Hooks.useState(None: option(string));
       let (birthGen, setBirthGen) = Hooks.useState(0);

       /* Refs, not state: nothing renders from either, so writing one must
          not schedule a frame. */
       let resumeRef = Hooks.useRef(None: option(string));
       let landingRef = Hooks.useRef(None: option(string));

       let (viewGen, view) = viewState;
       let (queryGen, query) = queryState;
       let (selGen, selPath) = selState;
       let (ignoreGen, rules, sessionIgnored) = ignoreState;
       let (trashedGen, trashed) = trashedState;
       let (cursorRow, cursorCol) = cursor;

       /*
        * EXACTLY ONE Scan.published PER RENDER.
        *
        * Two Atomic.gets in one body can straddle a publish and hand back a
        * `ranked` array from generation N indexed against a `nodeCount` from
        * N+1 - an out-of-bounds read that only fires under load. Everything
        * below reads THIS snapshot, including the phase (Scan.isScanning
        * would be a second get for no new information).
        *
        * The memo dependency is snap.gen and not the polled counter, so the
        * generation and the data it labels come from the same read: polling
        * Scan.generation and reading the snapshot separately can, at the very
        * last publish, cache one generation behind forever.
        */
       let snap = Scan.published(scan);
       let scanGen = snap.Scan.gen;
       let progress = snap.Scan.progress;
       let scanning =
         switch (snap.Scan.phase) {
         | Scan.Scanning => true
         | Scan.Idle
         | Scan.Done
         | Scan.Cancelled
         | Scan.Failed(_) => false
         };

       let scopePath =
         switch (view) {
         | Landing => root
         | Drill(p)
         | Ranked(p) => p
         };

       /* ---- geometry ---------------------------------------------------- */

       let wide = width >= splitWidth;
       let detailW = wide ? max(detailMin, min(44, width / 3)) : width;
       let listColW = wide ? max(1, width - 1 - detailW) : width;
       /* Bake to availWidth - 1: showScrollbar is a PROP, and Runtime spends
          the column on the bar whether or not the content overflows. Baking
          to the full width makes every row one cell too wide. */
       let listW = max(1, listColW - 1);

       let bodyH = max(1, height - 3);
       let filterVisible = filterOpen || query != "";
       let detailStripH = 3;
       let listH =
         max(
           1,
           (wide ? bodyH : max(1, bodyH - detailStripH))
           - (filterVisible ? 1 : 0),
         );
       /* Constant geometry: header and status are always present in every
          scan state, which is what keeps this a number rather than a
          function of the phase. */
       let listTop = 2 + (filterVisible ? 1 : 0);
       let page = max(1, listH - 1);

       /* ---- derived data ------------------------------------------------ */

       let birthCache: Hashtbl.t(Store.id, int) =
         Hooks.useMemo(() => Hashtbl.create(64), [||]);

       let scopeId =
         Hooks.useMemo(
           () =>
             switch (view) {
             | Landing => Some(Store.rootId)
             | Drill(p)
             | Ranked(p) => Scan.resolve(scan, p)
             },
           [|Obj.repr(scanGen), Obj.repr(viewGen)|],
         );

       let entries =
         Hooks.useMemo(
           () =>
             switch (view) {
             | Landing =>
               Scan.entries(scan, ~snapshot=snap, ~scope=None, ~showIgnored)
             | Drill(p) =>
               Scan.entries(scan, ~snapshot=snap, ~scope=Some(p), ~showIgnored)
             | Ranked(p) =>
               switch (Scan.resolve(scan, p)) {
               | None => [||]
               | Some(id) => Rank.frontier(store, ~root=id, ~params)
               }
             },
           [|
             Obj.repr(scanGen),
             Obj.repr(viewGen),
             Obj.repr(ignoreGen),
             Obj.repr(showIgnored),
           |],
         );

       /* What is actually listed: entries minus what has been trashed and
          minus what was ignored this session, narrowed by the filter. The
          fast path matters - materializing a path per entry is the only
          allocation in here, and a 100k-child directory would pay it ten
          times a second during a scan for nothing. */
       let visible =
         Hooks.useMemo(
           () => {
             let hidden = trashed @ (showIgnored ? [] : sessionIgnored);
             if (hidden == [] && query == "") {
               entries;
             } else {
               let keep = id => {
                 let p = Store.path(store, id);
                 !List.mem(p, hidden)
                 && (
                   query == ""
                   || Fuzzy.match_(
                        ~query,
                        ~text=Fmt.relativeTo(~base=scopePath, p),
                      )
                   != None
                 );
               };
               Array.of_list(List.filter(keep, Array.to_list(entries)));
             };
           },
           [|
             Obj.repr(scanGen),
             Obj.repr(viewGen),
             Obj.repr(ignoreGen),
             Obj.repr(showIgnored),
             Obj.repr(queryGen),
             Obj.repr(trashedGen),
           |],
         );

       let count = Array.length(visible);

       /* The selected PATH re-resolved into a row of the current list. -1
          when it is not there any more (trashed, filtered out, or the scan
          re-rooted underneath it), which the clamp below turns into row 0. */
       let selIdx =
         Hooks.useMemo(
           () =>
             switch (selPath) {
             | None => (-1)
             | Some(p) =>
               switch (Scan.resolve(scan, p)) {
               | None => (-1)
               | Some(id) =>
                 let n = Array.length(visible);
                 let i = ref(0);
                 let found = ref(-1);
                 while (found^ < 0 && i^ < n) {
                   if (visible[i^] == id) {
                     found := i^;
                   };
                   incr(i);
                 };
                 found^;
               }
             },
           [|
             Obj.repr(scanGen),
             Obj.repr(viewGen),
             Obj.repr(ignoreGen),
             Obj.repr(showIgnored),
             Obj.repr(queryGen),
             Obj.repr(trashedGen),
             Obj.repr(selGen),
           |],
         );

       /* CLAMPED IN THE RENDER, never in an effect: the list shrinks under
          the selection on every filter keystroke, and an effect would let one
          frame paint with an index the list no longer has. */
       let sel = count == 0 ? (-1) : max(0, min(selIdx < 0 ? 0 : selIdx, count - 1));
       /* The same clamp <ScrollView> applies, computed here so ensureRange
          below covers exactly the rows the runtime is about to paint. If
          these two ever drift the symptom is BLANK ROWS, not stale colour. */
       let topRow = max(0, min(offset, max(0, count - listH)));

       let selectedId = sel >= 0 ? Some(visible[sel]) : None;
       let selectedNode = Option.map(id => Store.get(store, id), selectedId);
       let selectedPath = Option.map(id => Store.path(store, id), selectedId);
       let selIdDep =
         switch (selectedId) {
         | Some(id) => id
         | None => (-1)
         };

       let rowsStore =
         Hooks.useMemo(
           () =>
             Rows.build(
               ~store,
               ~entries=visible,
               ~width=listW,
               ~query,
               ~scopePath,
               ~home,
             ),
           [|
             Obj.repr(scanGen),
             Obj.repr(viewGen),
             Obj.repr(ignoreGen),
             Obj.repr(showIgnored),
             Obj.repr(queryGen),
             Obj.repr(trashedGen),
             Obj.repr(listW),
           |],
         );

       /* ensureRange FIRST, then ensureSelected - in that order, because
          ensureSelected does two writes and no invalidation and a window bake
          would otherwise flatten the selected row back to plain. Both are
          idempotent, which is what makes it safe for Matcha to run this body
          twice in one frame (measure, then paint). */
       Rows.ensureRange(rowsStore, ~fromRow=topRow, ~toRow=topRow + listH + 16);
       Rows.ensureSelected(rowsStore, ~sel);
       let rows = Rows.rows(rowsStore);

       let ruleText =
         Hooks.useMemo(
           () =>
             String.concat(
               "\n",
               List.init(bodyH, _ =>
                 renderPieces([(Element.BoxChars.vertical, dimStyle)])
               ),
             ),
           [|Obj.repr(bodyH)|],
         );

       /* ---- effects ----------------------------------------------------- */

       /*
        * Creation time for the selected entry only. The dep is an int (the id,
        * or -1), never the option - Some(x) is a heap block and would make
        * this run every frame, i.e. one `stat` process per frame.
        */
       Hooks.useEffect(
         () => {
           switch (selectedId, selectedPath) {
           | (Some(id), Some(p)) when !Hashtbl.mem(birthCache, id) =>
             let t =
               switch (birthTime(p)) {
               | Some(v) => v
               | None => (-1)
               };
             Hashtbl.replace(birthCache, id, t);
             /* A cache write does not re-render on its own; bumping this is
                what puts the answer on screen. */
             setBirthGen(birthGen + 1);
           | _ => ()
           };
           None;
         },
         [|Obj.repr(selIdDep)|],
       );

       /*
        * THE SCAN POLL. The scan thread never calls setState; the UI asks.
        * ~ms=0 registers no timer at all, so this costs nothing the moment
        * the walk finishes - and because useInterval runs on the headless
        * VIRTUAL clock, a test drives the whole progress path with
        * handle.advanceTime(100) and no sleeping.
        */
       Hooks.useInterval(
         () => {
           let g = Scan.generation(scan);
           if (g != pollGen) {
             setPollGen(g);
           };
         },
         ~ms=scanning ? 100 : 0,
       );

       /* ---- actions ----------------------------------------------------- */

       let quitApp = () => {
         /* Let a walking thread unwind rather than leaving it parked. */
         Scan.cancel(scan);
         quit(ClearScreen);
       };

       let clampOffset = o => max(0, min(o, max(0, count - listH)));

       let selectRow = (i: int): unit =>
         if (count > 0) {
           let i = max(0, min(i, count - 1));
           setSelState((selGen + 1, Some(Store.path(store, visible[i]))));
           /* Keep it on screen with a two-row margin. Moving the selection
              nudges the offset; scrolling never moves the selection. */
           let margin = 2;
           let want =
             if (i < topRow + margin) {
               i - margin;
             } else if (i > topRow + listH - 1 - margin) {
               i - listH + 1 + margin;
             } else {
               topRow;
             };
           setOffset(clampOffset(want));
         };

       let goTo = (v: view, ~select: option(string)): unit => {
         setViewState((viewGen + 1, v));
         setSelState((selGen + 1, select));
         setOffset(0);
       };

       let descend = () =>
         switch (selectedId, selectedNode, selectedPath) {
         | (Some(_), Some(n), Some(p)) when n.Store.kind == Store.Dir =>
           if (view == Landing) {
             landingRef := Some(p);
           };
           goTo(Drill(p), ~select=None);
         | _ => ()
         };

       let ascend = () =>
         switch (view) {
         /* At the landing view there is nowhere up to go, and Backspace is a
            deliberate no-op rather than a jump to somewhere surprising. */
         | Landing => ()
         | Drill(p)
         | Ranked(p) =>
           if (p == root) {
             goTo(Landing, ~select=landingRef^);
           } else {
             goTo(Drill(Filename.dirname(p)), ~select=Some(p));
           }
         };

       let gotoLanding = () =>
         switch (view) {
         | Landing => ()
         | Drill(_)
         | Ranked(_) => goTo(Landing, ~select=landingRef^)
         };

       let rerank = () =>
         switch (view) {
         | Landing => ()
         | Drill(p) => goTo(Ranked(p), ~select=None)
         | Ranked(p) => goTo(Drill(p), ~select=None)
         };

       let setQueryTo = (q: string): unit => {
         setQueryState((queryGen + 1, q));
         setOffset(0);
       };

       let openFilter = () => {
         resumeRef := selectedPath;
         setFilterOpen(true);
         setCursor((0, TextWidth.stringWidth(query)));
       };

       let clearFilter = () => {
         setQueryTo("");
         setFilterOpen(false);
         setCursor((0, 0));
         setTaSel(None);
         /* Put the view back where the filter found it. */
         setSelState((selGen + 1, resumeRef^));
       };

       /*
        * `i` toggles. Adding writes the rule to the ignore file (atomically,
        * and re-read from disk afterwards) AND hides the entry now: the
        * arena's fIgnored flags were decided at scan time, and with no rescan
        * available the only honest immediate feedback is to drop the row.
        * `I` brings it back, undimmed - we never measured a pruned directory,
        * so there is nothing more to show.
        */
       let toggleIgnore = () =>
         switch (selectedPath) {
         | None => ()
         | Some(p) =>
           let already = List.mem(p, sessionIgnored);
           let (next, err) =
             already
               ? Ignore.removePath(~home, rules, p)
               : Ignore.addPath(~home, rules, p);
           /* Take the bytes out of (or put them back into) every ancestor
              right now, so the header total stops counting something the
              user just said they do not want to see. This returns false
              while a scan is still running - the arena has a single writer
              and it is not this thread - in which case dropping the row
              from the list below is the only honest feedback available, and
              the rule takes full effect on the next run. */
           let _: bool =
             already
               ? Scan.unapplyIgnore(scan, ~path=p)
               : Scan.applyIgnore(scan, ~path=p);
           let paths =
             already
               ? List.filter(x => x != p, sessionIgnored)
               : [p, ...sessionIgnored];
           setIgnoreState((ignoreGen + 1, next, paths));
           setNotice(err);
         };

       let openConfirm = () =>
         switch (selectedId, selectedNode, selectedPath) {
         | (Some(_), Some(n), Some(p)) =>
           setConfirm(
             Confirm(p, n.Store.size, n.Store.items, n.Store.kind == Store.Dir),
           )
         | _ => ()
         };

       let doTrash = (p: string) => {
         /* Where the selection lands once this row is gone. Worked out here,
            while the current list is still on screen. */
         let nextPath =
           if (sel >= 0 && sel + 1 < count) {
             Some(Store.path(store, visible[sel + 1]));
           } else if (sel > 0) {
             Some(Store.path(store, visible[sel - 1]));
           } else {
             None;
           };
         switch (trash(p)) {
         | Ok () =>
           setTrashedState((trashedGen + 1, [p, ...trashed]));
           setSelState((selGen + 1, nextPath));
           setConfirm(Closed);
         | Error(msg) => setConfirm(Failed(p, msg))
         };
       };

       let reveal = () =>
         switch (selectedPath) {
         | None => ()
         | Some(p) =>
           /* create_process, never a shell: a path is an argument. */
           switch (Unix.create_process("open", [|"open", "-R", p|],
                     Unix.stdin, Unix.stderr, Unix.stderr)) {
           | pid =>
             switch (Unix.waitpid([], pid)) {
             | _ => ()
             | exception _ => ()
             }
           | exception _ => setNotice(Some("could not reveal " ++ p))
           }
         };

       /* ---- keys --------------------------------------------------------
          THE OUTERMOST BRANCH IS switch (confirm), and that is not
          decoration: without it `d` at the confirmation opens a second dialog
          and `q` quits with the dialog still on screen.

          Ctrl+C is bound HERE, in useKeyDown, and never in useInput. Raw mode
          disables ISIG so Ctrl+C is an ordinary keypress only the app can act
          on, and useInput is suppressed while a layer is open - a useInput
          binding would make this app unquittable behind the Trash dialog. */
       Event.useKeyDown((key, mods) =>
         switch (confirm) {
         | Confirm(_, _, _, _)
         | Failed(_, _) =>
           switch (key, mods) {
           | (Key.Char('c'), {Key.ctrl: true, _}) => quitApp()
           | _ => ()
           }
         | Closed =>
           if (filterOpen) {
             switch (key, mods) {
             | (Key.Char('c'), {Key.ctrl: true, _}) => quitApp()
             | (Key.Escape, _) => clearFilter()
             /* Keep the filter, hand the keyboard back to the list. */
             | (Key.Enter, _) => setFilterOpen(false)
             /* Claimed so they move the selection instead of reaching
                TextArea's cursor. */
             | (Key.Arrow_down, _) => selectRow(sel + 1)
             | (Key.Arrow_up, _) => selectRow(sel - 1)
             | (Key.Page_down, _) => selectRow(sel + page)
             | (Key.Page_up, _) => selectRow(sel - page)
             | _ => ()
             };
           } else {
             switch (key, mods) {
             | (Key.Char('c'), {Key.ctrl: true, _}) => quitApp()
             | (Key.Char('q'), _) => quitApp()
             | (Key.Arrow_down, _)
             | (Key.Char('j'), _) => selectRow(sel + 1)
             | (Key.Arrow_up, _)
             | (Key.Char('k'), _) => selectRow(sel - 1)
             | (Key.Page_down, _) => selectRow(sel + page)
             | (Key.Page_up, _) => selectRow(sel - page)
             | (Key.Home, _)
             | (Key.Char('g'), _) => selectRow(0)
             | (Key.End, _)
             | (Key.Char('G'), _) => selectRow(count - 1)
             | (Key.Enter, _)
             | (Key.Arrow_right, _)
             | (Key.Char('l'), _) => descend()
             | (Key.Backspace, _)
             | (Key.Arrow_left, _)
             | (Key.Char('h'), _) => ascend()
             | (Key.Escape, _) =>
               if (query != "") {
                 clearFilter();
               } else {
                 gotoLanding();
               }
             | (Key.Char('/'), _) => openFilter()
             | (Key.Char('i'), _) => toggleIgnore()
             | (Key.Char('I'), _) => setShowIgnored(!showIgnored)
             | (Key.Char('d'), _) => openConfirm()
             | (Key.Char('r'), _) => rerank()
             | (Key.Char('o'), _) => reveal()
             | _ => ()
             };
           }
         }
       );

       /* The filter's text entry. useInput, so it goes deaf the moment the
          Trash dialog opens - the root is base, not a member of the layer.
          Intercept-then-delegate: everything the chords above claimed is
          swallowed here so it cannot ALSO reach TextArea, and everything else
          - printable characters, Backspace, Ctrl+U/W, left/right - is handed
          straight to the editor. That is what makes `q` type a q rather than
          quit, and Backspace edit rather than ascend. */
       Hooks.useInput((key, mods) =>
         if (filterOpen) {
           switch (key) {
           | Key.Escape
           | Key.Enter
           | Key.Arrow_up
           | Key.Arrow_down
           | Key.Page_up
           | Key.Page_down => ()
           | Key.Char('c') when mods.Key.ctrl => ()
           | _ =>
             TextArea.handleKeyDown(
               key,
               mods,
               query,
               setQueryTo,
               None,
               cursorRow,
               cursorCol,
               setCursor,
               taSel,
               setTaSel,
             )
           };
         }
       );

       /* ~wheel=false is load-bearing: it keeps the root out of the wheel's
          target search so a notch reaches the ScrollView. The root still sees
          every event through dispatchMouse's unconditional root fan-out, in
          FRAME coordinates, which is what makes click-to-select possible at
          all in ~rows mode (there are no children, so no <Clickable>).

          Click selects and nothing else. No click-to-descend: `d` is one key
          away from a click that landed on the wrong row. */
       Event.useMouse(~wheel=false, ev =>
         switch (ev.Mouse.kind) {
         | Mouse.Down =>
           let row = ev.Mouse.y - listTop;
           if (row >= 0
               && row < listH
               && ev.Mouse.x >= 0
               && ev.Mouse.x < listColW) {
             selectRow(topRow + row);
           };
         | _ => ()
         }
       );

       /* ---- the frame --------------------------------------------------- */

       let scopeNode = Option.map(id => Store.get(store, id), scopeId);

       let headerRow = {
         let (sz, it) =
           switch (scopeNode) {
           | Some(n) => (n.Store.size, n.Store.items)
           | None => (0, 0)
           };
         let right =
           Fmt.humanSize(sz)
           ++ " apparent"
           ++ dot
           ++ Fmt.humanCount(it)
           ++ " items";
         let tag =
           switch (view) {
           | Landing => "  biggest anywhere"
           | Ranked(_) => "  biggest here"
           | Drill(_) => ""
           };
         let rw = Element.visibleLength(right);
         let lw = max(0, width - rw - 1);
         let left = truncateStart(Fmt.breadcrumb(~home, scopePath) ++ tag, lw);
         exactly(
           renderPieces([
             (Element.padToWidth(left, lw), [Element.Bold]),
             (" ", []),
             (right, dimStyle),
           ]),
           width,
         );
       };

       let statusRow = {
         let base =
           switch (notice) {
           | Some(msg) => renderPieces([(msg, [Element.FgColor(Element.Yellow)])])
           | None =>
             switch (snap.Scan.phase) {
             | Scan.Failed(msg) =>
               renderPieces([
                 ("scan failed: " ++ msg, [Element.FgColor(Element.Red)]),
               ])
             | _ =>
               let head =
                 switch (snap.Scan.phase) {
                 | Scan.Scanning => "scanning\xe2\x80\xa6 "
                 | Scan.Cancelled => "scan stopped "
                 | Scan.Idle => "waiting "
                 | Scan.Done
                 | Scan.Failed(_) => "scanned "
                 };
               let counts =
                 Fmt.humanSize(progress.Scan.bytes)
                 ++ dot
                 ++ Fmt.humanCount(progress.Scan.files)
                 ++ " files"
                 ++ dot
                 ++ Fmt.humanCount(progress.Scan.dirs)
                 ++ " dirs";
               /* THE WARNING REPLACES THE CURRENT DIRECTORY, never the
                  counts. macOS returns EPERM for ~/Library/Mail and friends
                  even to the owning user without Full Disk Access, silently
                  under-reporting by tens of gigabytes - a tool that does that
                  without saying so is worse than no tool, so the tail of this
                  row belongs to the warning whenever there is one. */
               if (progress.Scan.unreadable > 0) {
                 renderPieces([
                   (head, dimStyle),
                   (counts, dimStyle),
                   (
                     Printf.sprintf(
                       "%s%d unreadable",
                       dot,
                       progress.Scan.unreadable,
                     ),
                     [Element.FgColor(Element.Yellow)],
                   ),
                   (dot ++ "grant Full Disk Access: " ++ fdaHint, dimStyle),
                 ]);
               } else {
                 renderPieces([
                   (head, dimStyle),
                   (counts, dimStyle),
                   (
                     /* Only while it means something. A finished scan that
                        still names the last directory it happened to be in
                        reads as "still working on that one". */
                     !scanning || progress.Scan.currentDir == ""
                       ? ""
                       : dot ++ Fmt.tildify(~home, progress.Scan.currentDir),
                     greyStyle,
                   ),
                 ]);
               };
             }
           };
         exactly(base, width);
       };

       let hintRow = {
         let text =
           switch (confirm) {
           | Confirm(_, _, _, _)
           | Failed(_, _) => "y move to trash \xc2\xb7 n cancel"
           | Closed =>
             filterOpen
               ? "type to filter \xc2\xb7 \xe2\x8f\x8e keep \xc2\xb7 esc clear \xc2\xb7 \xe2\x86\x91\xe2\x86\x93 move"
               : "\xe2\x86\x91\xe2\x86\x93 move \xc2\xb7 \xe2\x8f\x8e open \xc2\xb7 \xe2\x8c\xab up \xc2\xb7 / filter \xc2\xb7 i ignore \xc2\xb7 I reveal \xc2\xb7 d trash \xc2\xb7 r rank \xc2\xb7 o finder \xc2\xb7 q quit"
           };
         exactly(renderPieces([(text, dimStyle)]), width);
       };

       /* The details pane. Two shapes, one body of facts. */
       let detailLines = (~labelW: int, ~w: int): list(string) => {
         let field = (label, value) =>
           renderPieces([
             (Element.padToWidth(label, labelW), dimStyle),
             (value, []),
           ]);
         switch (selectedId, selectedNode, selectedPath) {
         | (Some(id), Some(n), Some(p)) =>
           let flags = n.Store.flags;
           let sizeLine =
             Store.hasFlag(flags, Store.fIgnored)
             || Store.hasFlag(flags, Store.fOtherDev)
               ? "\xe2\x80\x94" /* em dash: never measured */
               : (Store.hasFlag(flags, Store.fSparseSuspect) ? "~" : "")
                 ++ Fmt.humanSize(n.Store.size);
           let notes =
             (
               Store.hasFlag(flags, Store.fSparseSuspect)
                 ? ["apparent size; may be sparse"] : []
             )
             @ (
               Store.hasFlag(flags, Store.fUnreadable)
                 ? ["unreadable: size is a lower bound"] : []
             )
             @ (
               Store.hasFlag(flags, Store.fOtherDev)
                 ? ["on another filesystem; not descended"] : []
             )
             @ (
               Store.hasFlag(flags, Store.fHardLink)
                 ? ["hard link: already counted elsewhere"] : []
             )
             @ (
               Store.hasFlag(flags, Store.fIgnored) ? ["ignored; not measured"] : []
             );
           let created =
             switch (Hashtbl.find_opt(birthCache, id)) {
             | Some(t) when t > 0 => [field("created", Fmt.timestamp(t))]
             | _ => []
             };
           [renderPieces([(Rows.sanitize(n.Store.name), [Element.Bold])])]
           @ List.map(
               l => renderPieces([(l, greyStyle)]),
               wrapCells(Fmt.tildify(~home, p), ~width=w, ~maxLines=3),
             )
           @ [""]
           @ [field("size", sizeLine)]
           @ (
             n.Store.kind == Store.Dir
               ? [field("items", Fmt.humanCount(n.Store.items))] : []
           )
           @ [field("modified", Fmt.timestamp(n.Store.mtime))]
           /* "changed", never "created": st_ctime is inode-change time, and
              calling it created is a lie the user would act on. */
           @ [field("changed", Fmt.timestamp(n.Store.ctime))]
           /* FILES ONLY. Our own readdir counts as an access, so on a second
              run every directory hog walked reports "accessed: just now" and
              the field is worse than useless. */
           @ (
             n.Store.kind == Store.Dir
               ? [] : [field("accessed", Fmt.timestamp(n.Store.atime))]
           )
           @ created
           @ (notes == [] ? [] : [""])
           @ List.map(t => renderPieces([(t, [Element.FgColor(Element.Yellow)])]), notes);
         | _ => [renderPieces([("nothing selected", dimStyle)])]
         };
       };

       let detailsWide = {
         let w = max(1, detailW - 1);
         let lines = detailLines(~labelW=10, ~w);
         let shown = List.filteri((i, _) => i < bodyH, lines);
         <Text> {String.concat("\n", List.map(l => " " ++ l, shown))} </Text>;
       };

       let detailsStrip = {
         /* Narrow: three rows, and the long path gets the whole frame width
            on a line of its own - which is the one thing the split layout
            cannot give it. */
         let pathLine =
           switch (selectedPath) {
           | Some(p) =>
             renderPieces([(truncateStart(Fmt.tildify(~home, p), width), [Element.Bold])])
           | None => renderPieces([("nothing selected", dimStyle)])
           };
         let facts =
           switch (selectedNode) {
           | Some(n) =>
             renderPieces([
               (Fmt.humanSize(n.Store.size), []),
               (
                 n.Store.kind == Store.Dir
                   ? dot ++ Fmt.humanCount(n.Store.items) ++ " items" : "",
                 dimStyle,
               ),
               (dot ++ "modified " ++ Fmt.timestamp(n.Store.mtime), dimStyle),
             ])
           | None => ""
           };
         let times =
           switch (selectedId, selectedNode) {
           | (Some(id), Some(n)) =>
             let created =
               switch (Hashtbl.find_opt(birthCache, id)) {
               | Some(t) when t > 0 => dot ++ "created " ++ Fmt.timestamp(t)
               | _ => ""
               };
             renderPieces([
               ("changed " ++ Fmt.timestamp(n.Store.ctime), dimStyle),
               (
                 n.Store.kind == Store.Dir
                   ? "" : dot ++ "accessed " ++ Fmt.timestamp(n.Store.atime),
                 dimStyle,
               ),
               (created, dimStyle),
             ]);
           | _ => ""
           };
         <Text>
           {String.concat(
              "\n",
              List.map(l => exactly(l, width), [pathLine, facts, times]),
            )}
         </Text>;
       };

       let filterRow =
         <TextArea
           value=query
           onChange=setQueryTo
           placeholder="filter\xe2\x80\xa6"
           minHeight=1
           maxHeight=1
           blink=false
           cursorRow
           cursorCol
           setCursor
           selection=taSel
           setSelection=setTaSel
         />;

       /*
        * The empty state has to say WHY it is empty, and the ranking floor is
        * the reason that will actually bite: --min-size defaults to 10 MiB,
        * so a modest directory ranks to nothing and a bare "nothing here"
        * reads as "hog found no files" when the truth is "hog found nothing
        * big enough to be worth your attention". Only the ranked views are
        * thresholded - a drill-down lists everything - so only they say it.
        */
       let emptyText =
         count == 0
           ? Some(
               if (query != "") {
                 "no entry matches \"" ++ query ++ "\"";
               } else if (scanning) {
                 "scanning\xe2\x80\xa6";
               } else {
                 switch (view) {
                 | Landing
                 | Ranked(_) when config.Cli.minSize > 0 =>
                   "nothing above the "
                   ++ Fmt.humanSize(config.Cli.minSize)
                   ++ " floor"
                   ++ dot
                   ++ "lower it with --min-size, or press \xe2\x8f\x8e on a folder"
                 | Landing
                 | Ranked(_)
                 | Drill(_) => "nothing here"
                 };
               },
             )
           : None;

       let listColumn =
         <VStack>
           ...{
                (filterVisible ? [<Sized size={Chars(1)}> filterRow </Sized>] : [])
                @ [
                  <Sized size={Flex(1)}>
                    {switch (emptyText) {
                     | Some(t) => <Text dim=true> {" " ++ t} </Text>
                     | None =>
                       <ScrollView
                         id="list"
                         focusable=false
                         showScrollbar=true
                         rows
                         offset=topRow
                         onScroll={o => setOffset(o)}
                       />
                     }}
                  </Sized>,
                ]
              }
         </VStack>;

       let body =
         wide
           ? <HStack>
               <Sized size={Flex(1)}> <Container> listColumn </Container> </Sized>
               <Sized size={Chars(1)}> <Text> ruleText </Text> </Sized>
               <Sized size={Chars(detailW)}>
                 <Container> detailsWide </Container>
               </Sized>
             </HStack>
           : <VStack>
               <Sized size={Flex(1)}> <Container> listColumn </Container> </Sized>
               <Sized size={Chars(detailStripH)}>
                 <Container> detailsStrip </Container>
               </Sized>
             </VStack>;

       <VStack>
         <Sized size={Chars(1)}> <Text> headerRow </Text> </Sized>
         <Sized size={Chars(1)}> <Text> statusRow </Text> </Sized>
         <Sized size={Flex(1)}> body </Sized>
         /* DIRECTLY IN THE STACK. An <Overlay> costs no row, no gap slot and
            no justify share in either state - but only where it is written in
            a stack. Inside a <Sized>, or returned from a component, it would
            take a blank row. */
         <Modal
           isOpen={confirm != Closed}
           title=" Move to Trash "
           width={Percent(56)}
           height={Chars(9)}
           onDismiss={() => setConfirm(Closed)}>
           <ConfirmPane
             state=confirm
             onConfirm={() =>
               switch (confirm) {
               | Confirm(p, _, _, _) => doTrash(p)
               | Closed
               | Failed(_, _) => setConfirm(Closed)
               }
             }
             onCancel={() => setConfirm(Closed)}
           />
         </Modal>
         <Sized size={Chars(1)}> <Text> hintRow </Text> </Sized>
       </VStack>;
     };
   });
};
