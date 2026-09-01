/*
 * Test runner for hog.
 *
 * Run with: dune runtest   (or: dune exec test/run_tests.exe)
 *
 * Test.re and Input.re are copies of matcha's own, kept here because hog
 * depends only on the INSTALLED matcha library - matcha's test directory is
 * not part of the package and must never be added to this build.
 */

let () = {
  print_endline("");
  print_endline("Running hog tests");
  print_endline("=================");
  print_endline("");

  Fmt_tests.run();
  Fmt_tests.runBars();
  Fmt_tests.runPaths();
  Cli_tests.run();
  Fuzzy_tests.run();
  Ignore_tests.run();
  Ignore_tests.runMatching();
  Ignore_tests.runFile();
  Store_tests.run();
  Walk_tests.run();

  Test.finish();
};
