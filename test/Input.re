/*
 * Input - the essentials of Matcha's own test/input.re, copied here because
 * matcha's test directory is not part of the installed package (and must
 * never be added to this build). It compiles against the PUBLIC
 * InputDecoder / Key / Mouse surface only.
 *
 * feedBytes drives a FRESH InputDecoder over the given string exactly like
 * Runtime's loops do with a real terminal read: feed the whole string in one
 * call, then flush (so a trailing lone ESC still resolves without waiting on
 * the 25ms deadline). Every decoded event is delivered through the matching
 * headless-handle method.
 */
open Matcha;

let feedBytes = (handle: Runtime.headlessHandle, s: string): unit => {
  let decoder = InputDecoder.create();
  let deliver = (event: InputDecoder.event) =>
    switch (event) {
    | InputDecoder.KeyEvent(key, modifiers) => handle.sendKey(key, modifiers)
    | InputDecoder.PasteEvent(text) => handle.sendPaste(text)
    | InputDecoder.MouseEvent(ev) => handle.sendMouse(ev)
    | InputDecoder.CursorReport(_, _) => () /* interactive-loop only */
    /* An OSC reply (Matcha's background-colour probe) is terminal plumbing;
       handle.setTerminalBackground is the headless equivalent. */
    | InputDecoder.OscReport(_, _) => ()
    };
  let bytes = Bytes.of_string(s);
  List.iter(deliver, InputDecoder.feed(decoder, bytes, Bytes.length(bytes)));
  List.iter(deliver, InputDecoder.flush(decoder));
};

/* Feed a pre-parsed list of (key, modifiers) events directly. */
let feedKeys =
    (handle: Runtime.headlessHandle, keys: list((Key.t, Key.modifiers)))
    : unit =>
  List.iter(((key, modifiers)) => handle.sendKey(key, modifiers), keys);

let pressKey = (handle: Runtime.headlessHandle, key: Key.t): unit =>
  handle.sendKey(key, Key.noModifiers);

let pressTab = (handle: Runtime.headlessHandle): unit =>
  handle.sendKey(Key.Tab, Key.noModifiers);

/* Click the left button at (x, y) in live-region coordinates (0-based,
   (0, 0) = top-left of the frame). Fires a button-DOWN event, which is what
   <Clickable> reacts to. */
let clickAt = (handle: Runtime.headlessHandle, ~x: int, ~y: int): unit =>
  handle.sendMouse({
    Mouse.kind: Mouse.Down,
    button: Mouse.Left,
    x,
    y,
    shift: false,
    alt: false,
    ctrl: false,
  });

/* One wheel notch at (x, y). The innermost ScrollView under the point wins. */
let wheelAt =
    (handle: Runtime.headlessHandle, ~x: int, ~y: int, ~up: bool): unit =>
  handle.sendMouse({
    Mouse.kind: up ? Mouse.ScrollUp : Mouse.ScrollDown,
    button: Mouse.Left,
    x,
    y,
    shift: false,
    alt: false,
    ctrl: false,
  });
