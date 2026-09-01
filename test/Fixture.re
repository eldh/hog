/*
 * Throwaway directory trees for the walk tests.
 *
 * Two rules run through everything here:
 *
 *   1. Every fixture cleans up in a Fun.protect finaliser, INCLUDING
 *      restoring permissions. A test that fails halfway through otherwise
 *      leaves a chmod-0 directory behind, and the next run cannot even
 *      delete it - one red test becomes a permanently broken suite.
 *
 *   2. File sizes are exact and distinct, never random. Ranking assertions
 *      are about which entry is bigger; a tie or a size that changes between
 *      runs turns a real failure into a coin flip.
 */

/* Recursively delete. Uses lstat, so a symlink is unlinked rather than
 * followed - a fixture with a link to its own ancestor must not make the
 * cleanup infinite.
 *
 * Directories are chmod'ed back to 0o700 before being read, defensively: a
 * test that died before its own finaliser ran would otherwise leave a
 * directory this cannot open. */
let rec rmRf = (path: string): unit =>
  switch (Unix.lstat(path)) {
  | exception (Unix.Unix_error(_, _, _)) => ()
  | st =>
    switch (st.Unix.st_kind) {
    | Unix.S_DIR =>
      switch (Unix.chmod(path, 0o700)) {
      | () => ()
      | exception (Unix.Unix_error(_, _, _)) => ()
      };
      switch (Sys.readdir(path)) {
      | entries =>
        Array.iter(name => rmRf(Filename.concat(path, name)), entries)
      | exception (Sys_error(_)) => ()
      };
      switch (Unix.rmdir(path)) {
      | () => ()
      | exception (Unix.Unix_error(_, _, _)) => ()
      };
    | _ =>
      switch (Unix.unlink(path)) {
      | () => ()
      | exception (Unix.Unix_error(_, _, _)) => ()
      }
    }
  };

/* Run `f` with a fresh empty temp directory, removed afterwards whatever
 * happens. The path is NOT canonicalized: on macOS the temp directory lives
 * under a /var symlink, and resolving it here would mean the tests assert
 * against a path the walk never saw. */
let withTmp = (f: string => 'a): 'a => {
  let dir = Filename.temp_dir("hog-test-", "");
  Fun.protect(~finally=() => rmRf(dir), () => f(dir));
};

/* mkdir -p. */
let rec mkdirP = (path: string): unit =>
  if (path != "" && path != "/" && !Sys.file_exists(path)) {
    mkdirP(Filename.dirname(path));
    switch (Unix.mkdir(path, 0o755)) {
    | () => ()
    /* Harmless when two spec entries share a parent. */
    | exception (Unix.Unix_error(Unix.EEXIST, _, _)) => ()
    };
  };

/* A file of exactly `size` bytes. Bytes.make keeps it dense, so the apparent
 * size the walk reads is also the allocated size and nothing is sparse. */
let writeFile = (~path: string, ~size: int): unit => {
  mkdirP(Filename.dirname(path));
  let oc = open_out_bin(path);
  Fun.protect(
    ~finally=() => close_out(oc),
    () =>
      if (size > 0) {
        output_bytes(oc, Bytes.make(size, 'x'));
      },
  );
};

/* Build a tree from (relative path, byte size) pairs under a fresh temp
 * directory, and hand the root to `f`.
 *
 * A path ending in "/" is an empty DIRECTORY and its size is ignored - that
 * is the only way to spell one, since a directory is otherwise implied by
 * the files inside it. Intermediate directories are always created. */
let withTree = (spec: list((string, int)), f: string => 'a): 'a =>
  withTmp(root => {
    List.iter(
      ((rel, size)) => {
        let full = Filename.concat(root, rel);
        if (String.length(rel) > 0 && rel.[String.length(rel) - 1] == '/') {
          mkdirP(full);
        } else {
          writeFile(~path=full, ~size);
        };
      },
      spec,
    );
    f(root);
  });

/* A symlink at `link` pointing at `target`. The target is written verbatim
 * and does not have to exist - a dangling link is a case the walk has to
 * survive, and an absolute link to an ancestor is how the non-descent test
 * proves the walk terminates. */
let symlink = (~target: string, ~link: string): unit => {
  mkdirP(Filename.dirname(link));
  Unix.symlink(target, link);
};

/* A second name for an existing file - same inode, so the walk's dedup has
 * to count it once. */
let hardlink = (~target: string, ~link: string): unit => {
  mkdirP(Filename.dirname(link));
  Unix.link(target, link);
};

/* Run `f` with every path in `paths` chmod'ed to 0, restoring the original
 * mode afterwards even if `f` raises. Restoring is not optional: rmRf
 * defends against it, but a leftover unreadable directory outside a fixture
 * would still break the run. */
let withUnreadable = (paths: list(string), f: unit => 'a): 'a => {
  let saved =
    List.map(p => (p, Unix.stat(p).Unix.st_perm), paths);
  List.iter(p => Unix.chmod(p, 0o000), paths);
  Fun.protect(
    ~finally=
      () =>
        List.iter(
          ((p, perm)) =>
            switch (Unix.chmod(p, perm)) {
            | () => ()
            | exception (Unix.Unix_error(_, _, _)) => ()
            },
          saved,
        ),
    f,
  );
};
