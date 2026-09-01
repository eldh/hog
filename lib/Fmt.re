/*
 * Formatting helpers. Pure - no state, no I/O. (Unix appears only as
 * Unix.localtime, which is a calendar calculation, not I/O.)
 *
 * Everything here is measured in TERMINAL CELLS, not bytes, because the
 * list rows are laid out by column. humanSize in particular has a hard
 * contract - it never returns more than 5 cells - and the row builder
 * relies on it to keep the size column aligned.
 */

/* ------------------------------------------------------------------ sizes */

/* A byte count in at most 5 cells: "0B", "999B", "9.8K", "412M", "1023M".
 *
 * The rule: below 1000 bytes show plain bytes; otherwise scale to the
 * largest unit that keeps the value under 1024, then show one decimal
 * place while the value is under 10 and none above it. That keeps three
 * significant figures everywhere without ever needing a sixth cell. */
let humanSize = (bytes: int): string =>
  if (bytes < 0) {
    "-";
  } else if (bytes < 1000) {
    string_of_int(bytes) ++ "B";
  } else {
    let units = [|"K", "M", "G", "T", "P"|];
    let value = ref(float_of_int(bytes) /. 1024.0);
    let unit = ref(0);
    while (value^ >= 1024.0 && unit^ < Array.length(units) - 1) {
      value := value^ /. 1024.0;
      incr(unit);
    };
    let v = value^;
    if (v < 10.0) {
      Printf.sprintf("%.1f%s", v, units[unit^]);
    } else {
      Printf.sprintf("%.0f%s", v, units[unit^]);
    };
  };

/* The widest string humanSize can return, in cells. The row layout reserves
 * exactly this much, so a change to humanSize that widens it fails the test
 * that pins these two together rather than quietly shifting every column. */
let sizeWidth = 5;

/* 12345 -> "12,345". Item counts are read, not computed, so the separators
 * earn their width. */
let humanCount = (n: int): string => {
  let s = string_of_int(abs(n));
  let len = String.length(s);
  let buf = Buffer.create(len + len / 3);
  if (n < 0) {
    Buffer.add_char(buf, '-');
  };
  String.iteri(
    (i, c) => {
      if (i > 0 && (len - i) mod 3 == 0) {
        Buffer.add_char(buf, ',');
      };
      Buffer.add_char(buf, c);
    },
    s,
  );
  Buffer.contents(buf);
};

/* ------------------------------------------------------------------- bars */

/* Eighth-block glyphs, index 0..8. Index 0 is a space, so a zero-width bar
 * still occupies its cell and the columns after it do not shift. */
let eighths = [|" ", "\xe2\x96\x8f", "\xe2\x96\x8e", "\xe2\x96\x8d", "\xe2\x96\x8c", "\xe2\x96\x8b", "\xe2\x96\x8a", "\xe2\x96\x89", "\xe2\x96\x88"|];

/* A proportional bar exactly `cells` cells wide. `value` is clamped into
 * [0, max]; max <= 0 yields an empty (all-space) bar rather than dividing
 * by zero. Resolution is cells*8 levels, which is far more than the eye
 * needs and costs one string build. */
let bar = (~value: int, ~max: int, ~cells: int): string =>
  if (cells <= 0) {
    "";
  } else if (max <= 0 || value <= 0) {
    String.concat("", List.init(cells, _ => eighths[0]));
  } else {
    let v = min(value, max);
    /* Total eighths to fill, rounded down. */
    let filled = v * cells * 8 / max;
    let whole = filled / 8;
    let rest = filled mod 8;
    let buf = Buffer.create(cells * 3);
    for (i in 0 to cells - 1) {
      if (i < whole) {
        Buffer.add_string(buf, eighths[8]);
      } else if (i == whole) {
        Buffer.add_string(buf, eighths[rest]);
      } else {
        Buffer.add_string(buf, eighths[0]);
      };
    };
    Buffer.contents(buf);
  };

/* -------------------------------------------------------------- timestamps */

let months = [|
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
|];

/* "2024-11-30 08:41" - sortable, unambiguous, 16 cells. Takes Unix seconds
 * and formats in LOCAL time, which is what a person comparing against their
 * own memory of a download expects. */
let timestamp = (~now as _: int=0, seconds: int): string => {
  let tm = Unix.localtime(float_of_int(seconds));
  Printf.sprintf(
    "%04d-%02d-%02d %02d:%02d",
    tm.Unix.tm_year + 1900,
    tm.Unix.tm_mon + 1,
    tm.Unix.tm_mday,
    tm.Unix.tm_hour,
    tm.Unix.tm_min,
  );
};

/* "3 days ago", "just now". Deliberately coarse: the question this answers
 * is "have I touched this recently", not "when exactly". */
let relativeTime = (~now: int, seconds: int): string => {
  let d = now - seconds;
  if (d < 0) {
    "in the future";
  } else if (d < 60) {
    "just now";
  } else if (d < 3600) {
    let m = d / 60;
    string_of_int(m) ++ (m == 1 ? " minute ago" : " minutes ago");
  } else if (d < 86400) {
    let h = d / 3600;
    string_of_int(h) ++ (h == 1 ? " hour ago" : " hours ago");
  } else if (d < 86400 * 365) {
    let days = d / 86400;
    string_of_int(days) ++ (days == 1 ? " day ago" : " days ago");
  } else {
    let y = d / (86400 * 365);
    string_of_int(y) ++ (y == 1 ? " year ago" : " years ago");
  };
};

/* Month name for a Unix timestamp, exposed so tests can build expectations
 * without duplicating the table. */
let monthName = (m: int): string =>
  m >= 0 && m < 12 ? months[m] : "?";

/* ------------------------------------------------------------------- paths */

/* Rewrite an absolute path under `home` to "~/...". Leaves anything else
 * alone. Used everywhere a path is DISPLAYED, never where one is acted on. */
let tildify = (~home: string, path: string): string => {
  let hl = String.length(home);
  if (hl > 0 && String.length(path) >= hl && String.sub(path, 0, hl) == home) {
    if (String.length(path) == hl) {
      "~";
    } else if (path.[hl] == '/') {
      "~" ++ String.sub(path, hl, String.length(path) - hl);
    } else {
      path;
    };
  } else {
    path;
  };
};

/* `path` expressed relative to `base`, or unchanged when it is not
 * underneath. The landing view renders every row this way, which is what
 * keeps most rows short enough to need no eliding at all. */
let relativeTo = (~base: string, path: string): string => {
  let bl = String.length(base);
  if (bl > 0
      && String.length(path) > bl
      && String.sub(path, 0, bl) == base
      && path.[bl] == '/') {
    String.sub(path, bl + 1, String.length(path) - bl - 1);
  } else if (bl > 0 && path == base) {
    ".";
  } else {
    path;
  };
};

/* Split an absolute path into its segments, dropping empties. */
let segments = (path: string): list(string) =>
  List.filter(s => s != "", String.split_on_char('/', path));

/* "~ > Library > Caches" for the header. Shows at most `maxSegments` from
 * the end, prefixed with an ellipsis when it had to drop any. */
let breadcrumb = (~home: string, ~maxSegments: int=4, path: string): string => {
  let shown = tildify(~home, path);
  let segs =
    switch (String.length(shown) > 0 && shown.[0] == '~') {
    | true => ["~", ...segments(String.sub(shown, 1, String.length(shown) - 1))]
    | false => segments(shown)
    };
  let n = List.length(segs);
  let (segs, elided) =
    if (n > maxSegments) {
      (
        List.filteri((i, _) => i >= n - maxSegments, segs),
        true,
      );
    } else {
      (segs, false);
    };
  let joined = String.concat(" \xe2\x80\xba ", segs);
  elided ? "\xe2\x80\xa6 \xe2\x80\xba " ++ joined : joined;
};
