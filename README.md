imp-model
========

generates a z3 file when a valid imp program is provided as input.

Tested on: 
- `cabal 2.4` 
- `ghc 8.6.3`

Usage: 
- `cabal new-configure`
- `cabal new-run imp-model -- examples/factorial.imp`

This will produce a z3 file `examples/factorial.rs`
There are 2 other files `unsat.imp` and `ite.imp` for sample testing.

- `cabal new-test`

This will run some sanity tests on the parser and AST generation
