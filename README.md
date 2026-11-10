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

## Bazel OCaml graph tracer

The Bazel target `//test:depgraph_test` compiles `src/remodel_ast.ml` and
`src/remodel_depgraph.ml` as separate modules, then runs assertions over graph
construction and traversal. It is a bounded toolchain tracer; the complete
Remodel executable also needs the lexer, parser, cache, and Parmap package.

The Bazel module pins OCaml 5.5.2 source by SHA-256 and uses BCR-pinned GCC and
configure/make rules to build a compiler. This configuration currently targets
Linux x86-64. The compiler action and remote portability must be verified
before treating this graph as an accepted onboarding target. The existing
`make -C src` entry point remains available.


