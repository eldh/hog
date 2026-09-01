#!/bin/sh
#
# Build hog and copy the binary onto your PATH.
#
#   sh install.sh                    # build in the active switch
#   sh install.sh ~/Code/matcha      # build in that switch instead
#
# Destination defaults to ~/.local/bin; override with HOG_BIN.
#
# The BUILD needs an OCaml >= 5.3 switch with matcha available. The INSTALLED
# binary does not: it is self-contained, so it keeps working after you change
# switches, and inside repositories whose own tooling uses a different one.

set -e

here="$(cd "$(dirname "$0")" && pwd)"
dest="${HOG_BIN:-$HOME/.local/bin}"

if [ "$#" -ge 1 ]; then
  eval "$(opam env --switch "$1" --set-switch)"
else
  eval "$(opam env)"
fi

cd "$here"
dune build bin/main.exe
mkdir -p "$dest"
install -m 755 _build/default/bin/main.exe "$dest/hog"

printf 'installed %s\n' "$dest/hog"
case ":$PATH:" in
  *":$dest:"*) ;;
  *) printf 'note: %s is not on your PATH\n' "$dest" ;;
esac
