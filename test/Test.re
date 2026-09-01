/*
 * A tiny test framework - the same shape as Matcha's own test/Test.re, kept
 * local so bdiff depends only on the INSTALLED matcha library (matcha's test
 * helpers are not installed, and its test directory must never be added to
 * this build).
 *
 *   Test.group("git: parser", () => {
 *     Test.run("two hunks", () => Test.assertEqual(n, 2, "hunk count"));
 *   });
 *   Test.finish();   // prints the summary and exits 1 on any failure
 */

let passCount = ref(0);
let failCount = ref(0);
let errors: ref(list(string)) = ref([]);

let green = "\027[32m";
let red = "\027[31m";
let reset = "\027[0m";
let bold = "\027[1m";

exception AssertionFailed(string);

let run = (name: string, f: unit => unit): unit => {
  print_string("  " ++ name ++ " ... ");
  flush(stdout);
  switch (f()) {
  | () =>
    incr(passCount);
    print_endline(green ++ "PASS" ++ reset);
  | exception e =>
    incr(failCount);
    let msg = Printexc.to_string(e);
    errors := [name ++ ": " ++ msg, ...errors^];
    print_endline(red ++ "FAIL" ++ reset);
    print_endline("    " ++ red ++ msg ++ reset);
  };
};

let group = (name: string, f: unit => unit): unit => {
  print_endline(bold ++ name ++ reset);
  f();
  print_newline();
};

let assertTrue = (cond: bool, msg: string): unit =>
  if (!cond) {
    raise(AssertionFailed(msg));
  };

let assertFalse = (cond: bool, msg: string): unit =>
  if (cond) {
    raise(AssertionFailed(msg ++ " (expected false)"));
  };

let assertEqualInt = (actual: int, expected: int, msg: string): unit =>
  if (actual != expected) {
    raise(
      AssertionFailed(
        Printf.sprintf(
          "%s\n      expected: %d\n      actual:   %d",
          msg,
          expected,
          actual,
        ),
      ),
    );
  };

let assertEqualStr = (actual: string, expected: string, msg: string): unit =>
  if (actual != expected) {
    raise(
      AssertionFailed(
        msg
        ++ "\n      expected: \""
        ++ expected
        ++ "\"\n      actual:   \""
        ++ actual
        ++ "\"",
      ),
    );
  };

/* Number of non-overlapping occurrences of [needle] in [haystack]. */
let countOccurrences = (haystack: string, needle: string): int => {
  let hlen = String.length(haystack);
  let nlen = String.length(needle);
  if (nlen == 0 || nlen > hlen) {
    0;
  } else {
    let count = ref(0);
    let i = ref(0);
    while (i^ <= hlen - nlen) {
      if (String.sub(haystack, i^, nlen) == needle) {
        incr(count);
        i := i^ + nlen;
      } else {
        incr(i);
      };
    };
    count^;
  };
};

let contains = (haystack: string, needle: string): bool =>
  String.length(needle) == 0 || countOccurrences(haystack, needle) > 0;

let assertContains = (haystack: string, needle: string, msg: string): unit =>
  if (!contains(haystack, needle)) {
    raise(
      AssertionFailed(
        msg
        ++ "\n      missing: \""
        ++ needle
        ++ "\"\n      in:\n"
        ++ haystack,
      ),
    );
  };

let assertNotContains = (haystack: string, needle: string, msg: string): unit =>
  if (contains(haystack, needle)) {
    raise(
      AssertionFailed(
        msg ++ "\n      unexpectedly present: \"" ++ needle ++ "\"",
      ),
    );
  };

let finish = (): unit => {
  print_newline();
  let total = passCount^ + failCount^;
  if (failCount^ == 0) {
    print_endline(
      green ++ bold ++ "All " ++ string_of_int(total) ++ " tests passed!" ++ reset,
    );
    exit(0);
  } else {
    print_endline(
      red
      ++ bold
      ++ string_of_int(failCount^)
      ++ " of "
      ++ string_of_int(total)
      ++ " tests failed"
      ++ reset,
    );
    print_newline();
    print_endline(red ++ "Failures:" ++ reset);
    List.iter(err => print_endline("  - " ++ err), List.rev(errors^));
    exit(1);
  };
};
