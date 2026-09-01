# hog

Find the folders eating your disk.

Big *files* are easy to find. What is hard to find is a folder full of things
you stopped using two years ago. `hog` scans a directory tree and ranks the
biggest things it finds **anywhere underneath, at any depth**, so a forgotten
cache surfaces on the first screen instead of after ten levels of drilling.

```
hog                      # scan $HOME
hog ~/Code               # scan somewhere else
hog --min-size 100M      # only show entries over 100 MB
```

Built with [matcha](https://github.com/eldh/matcha).

## What you get

- **A ranked landing view.** Not the root's immediate children — the actual
  hoards, wherever they are. A directory whose contents are spread evenly
  across sixty caches is shown as one row (that is the useful answer); a
  directory that is really just a wrapper around one huge file is stepped
  through until it reaches the file.
- **Navigation.** Enter walks into anything; Backspace comes back out.
  Inside a folder you get its plain contents, largest first.
- **Details.** Full path, size, item count, and timestamps for whatever is
  selected.
- **A filter.** `/` and then type; matches are highlighted and ranked.
- **An ignore list.** `i` on a row adds it to `~/.config/hog/ignore` and
  rescans. Ignored directories are *pruned* — never opened, never walked —
  so the list makes scans faster as well as quieter.
- **Trash.** `d` moves the selected entry to the macOS Trash, behind a
  confirmation naming the full path and the size. It goes to the Trash, not
  to `unlink`, so a mistake is recoverable.

## Keys

| | |
|---|---|
| `↑` `↓` / `k` `j` | move |
| `PgUp` `PgDn` · `Home` `End` / `g` `G` | move faster |
| `Enter` / `→` | open the selected folder |
| `Backspace` / `←` | up one level |
| `/` | filter · `Esc` clears it |
| `i` | ignore the selected entry · `I` reveals ignored entries |
| `d` | move to Trash |
| `o` | reveal in Finder |
| `r` | rescan |
| `q` / `Ctrl+C` | quit |

## The ignore list

`~/.config/hog/ignore`, seeded on first run and yours to edit afterwards.
One rule per line:

- A pattern **containing `/`** is an absolute path, starting with `/` or
  `~/`. It ignores that directory and everything under it.
- A pattern **without `/`** is a basename matched at any depth, with `*` and
  `?` as wildcards.
- `#` starts a comment. Matching is case-insensitive.
- There is no negation. To un-ignore something, delete or comment out its
  line.

The shipped defaults cover the things that are large, slow or dangerous to
walk rather than the things that are merely big — other volumes, the
synthetic `/System` mount layout, swap, and cloud-storage folders (reading a
dataless iCloud or Dropbox placeholder can make the system *download* it).

Notably **not** ignored by default: `~/.Trash`, `DerivedData`,
`CoreSimulator`, `MobileSync/Backup`, `.git`. Those are frequently enormous
*and* safe to delete — they are the answer, not the noise.

## Two things the numbers cannot tell you

**Sizes are apparent, not allocated.** `hog` reports what a file says it
contains, which is what Finder shows. Sparse files — VM disk images like
`Docker.raw`, `.qcow2`, `.img` — can claim far more than they occupy, so
they are flagged with a `~` and a note. APFS clones (a Finder duplicate)
are counted twice, because nothing in user space can see that two files
share blocks.

**Some folders need Full Disk Access.** macOS hides `~/Library/Mail`,
`~/Library/Messages`, `~/Library/Safari` and your Photos library from any
program that has not been granted it — even from you. Without it `hog`
under-reports `~/Library` by however much lives in there. When it hits
directories it cannot read it says so in the status bar rather than quietly
reporting a smaller number. Grant it in System Settings › Privacy &
Security › Full Disk Access, for your terminal.

## Install

```
sh install.sh
```

Builds and copies the binary to `~/.local/bin/hog` (override with `HOG_BIN`).
The build needs an OCaml >= 5.3 switch with `matcha` available; the installed
binary is self-contained, so it keeps working after you switch switches.

If your active switch is older, point the script at one that is not:

```
sh install.sh ~/Code/matcha
```

Or install through opam, into the active switch:

```
opam pin add -y matcha https://github.com/eldh/matcha.git
opam pin add -y hog .
```

## Development

```
dune build
dune runtest
```

Run it headlessly — **always with all three of `timeout`,
`MATCHA_HEADLESS=1` and a closed stdin**, or it waits forever for a terminal
that is not there:

```
timeout 20 env MATCHA_HEADLESS=1 MATCHA_WIDTH=140 MATCHA_HEIGHT=40 \
  dune exec bin/main.exe -- ./test < /dev/null
```

The suite drives the real component through `Runtime.startHeadless` against
throwaway trees built on disk, and never at 80x24 — that size is matcha's
constraints default, its headless default *and* its non-TTY fallback all at
once, so a layout bug can hide inside the coincidence.
