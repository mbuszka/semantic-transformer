# Building the project
The project is built with `cabal`

- `cabal build` to build
- `cabal install` to install `semt` executable
- `cabal run semt` to run the development build without installing
- `make test` to test the tool on all example interpreters
- `make clean` to delete all generated interpreters

To pass arguments to the executable use `cabal run semt --`

# Running examples
```
cd interpreters
raco test src/lc-call-by-value.rkt
semt src/lc-call-by-value.rkt
raco test out/lc-call-by-value.rkt
```

These commands will:
- run tests in the source file
- transform the interpreter and put the result into `out/lc-call-by-value.rkt`
- run tests in the result file

# Example programs
All example interpreters are placed in directory `interpreters/src`.
They `require` a library in `interpreters/lib` by relative path so it is recommended to put results into an immediate subdirectory of `interpreters` such as `interpreters/out`

# Project structure
- `semt/Main.hs` is the `Main` module, which contains the `main` function, parses command line arguments and calls `Pipeline.run` which performs the transformation
- module `Pipeline` contains the transformation pipeline and its components
- module `Syntax` contains all syntax definitions used in the transformer. The `TermF` is a functor over its subtrees to accomodate different representations of syntax trees (e.g. `Term` defined in `src/Syntax.hs` or `Label` used in the abstract interpreter `src/AbsInt/Types.hs`)
- module `Prelude` contains the prelude imported implicitly into every module in `src/`
- module `AbsInt` contains the abstract abstract machine used to compute control-flow analysis

The project uses `Polysemy` to manage computational effects.