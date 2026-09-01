(lang dune 3.0)

(name hog)

(generate_opam_files true)

(source
 (github eldh/hog))

(license MIT)

(authors "Andreas Eldh")

(maintainers "Andreas Eldh")

(package
 (name hog)
 (synopsis "A terminal explorer for the folders eating your disk")
 (description
  "hog scans a directory tree and ranks the biggest things it finds anywhere underneath, at any depth, so that a forgotten cache or build directory surfaces without being hunted for. Walk into any entry to see its contents, read size, item count and timestamps, filter by name, and move things to the Trash. An ignore list keeps the large things you mean to keep out of the way, and is pruned during the walk rather than filtered after it.")
 (depends
  (ocaml
   (>= 5.3.0))
  (reason
   (>= 3.12.0))
  matcha))
