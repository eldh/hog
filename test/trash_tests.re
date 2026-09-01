/*
 * Tests for lib/Trash.re.
 *
 * ONLY `script` IS TESTED, AND NOTHING IN HERE EXECUTES ANYTHING. `move`
 * spawns osascript and asks Finder to delete a file; a suite that exercised
 * it would either need a real file to destroy or a TCC prompt in the middle
 * of `dune runtest`. The part that can go wrong quietly is the escaping, and
 * that part is pure - which is exactly why it was split out.
 *
 * The escaping is where a bug would be worst: a path that closes the
 * AppleScript string literal early does not fail, it runs the REST of the
 * path as AppleScript, against a tool people point at directories full of
 * names they have never read.
 */

/* The literal opens right after `POSIX file `. Written out once here so the
 * cases below assert on the payload rather than restating the whole
 * sentence each time. */
let prefix = "tell application \"Finder\" to delete POSIX file \"";
let suffix = "\"";

let expect = (path: string, escaped: string, msg: string) =>
  Test.assertEqualStr(Trash.script(path), prefix ++ escaped ++ suffix, msg);

let run = () =>
  Test.group("trash: script", () => {
    Test.run("an ordinary path appears verbatim", () => {
      expect(
        "/Users/tester/Library/Caches",
        "/Users/tester/Library/Caches",
        "nothing to escape, nothing changed",
      );
    });

    Test.run("a double quote is backslash-escaped", () => {
      /* Each quote in the path gains exactly one backslash. */
      expect(
        "/tmp/say\"cheese\"",
        "/tmp/say\\\"cheese\\\"",
        "each quote gains one backslash",
      );
    });

    Test.run("a backslash is doubled", () => {
      /* One backslash in the path becomes two in the literal. */
      expect("/tmp/back\\slash", "/tmp/back\\\\slash", "one backslash becomes two");
    });

    Test.run("a backslash before a quote is escaped first", () => {
      /* THE ORDERING CASE. The path holds a backslash immediately followed
         by a quote. Each is escaped from the ORIGINAL text: the backslash
         doubles, the quote gains a backslash, and neither escape is looked
         at again. A two-pass replace that handled the quote first would
         write a backslash and then double it on the second pass, closing
         the AppleScript literal one character early and handing the rest of
         the path to AppleScript as code. */
      expect("/tmp/\\\"x", "/tmp/\\\\\\\"x", "backslash doubled, quote escaped, independently");
    });

    Test.run("a space needs no escaping inside the literal", () => {
      /* Deliberate: quoting for a SHELL would need this, and the fact that
         it does not is the visible consequence of never using one. */
      expect(
        "/Users/tester/My Big Folder",
        "/Users/tester/My Big Folder",
        "spaces survive untouched",
      );
    });

    Test.run("a unicode name survives byte for byte", () => {
      let p = "/Users/tester/\xe5\x86\x99\xe7\x9c\x9f/\xf0\x9f\x8e\xac.mov";
      expect(p, p, "no transcoding, no escaping");
    });

    Test.run("the script is a single line addressed to Finder", () => {
      let s = Trash.script("/tmp/x");
      Test.assertEqualInt(Test.countOccurrences(s, "\n"), 0, "one line");
      Test.assertContains(s, "application \"Finder\"", "addressed to Finder");
      Test.assertContains(s, "POSIX file", "a POSIX path, not an HFS one");
    });
  });
