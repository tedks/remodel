Remodel
=======

Remodel is a toy replacement for GNU Make written in OCaml. It uses the parallelism primitives from Roberto Di Cosmo's parmap package.

Remodel uses an MD5 hash of files and dependencies to enforce ordering between dependencies. 

Remodel currently doesn't use work-stealing, so each level of the dependency graph will build as fast as the slowest node on that level.

Building Remodel
----------------

Remodel depends on the Parmap package, which is easily installable from opam:

    opam install parmap

After installing parmap, simply run `make` in the top-level directory:

    make -C src

This will leave a `remodel` binary under remodel/src/.


