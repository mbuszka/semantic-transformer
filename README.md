# Building the project
The project is built with `cabal`

- `cabal build` to build
- `cabal install` to install `semt` executable
- `cabal run semt` to run the development build without installing
- `cabal run semt-test` to test the tool on all example interpreters

To pass arguments to the executable use `cabal run semt --`

# Running examples
```
cd interpreters
raco test src/lambda-value.rkt
semt src/lambda-value.rkt -o out/
raco test out/lambda-value.rkt
```

These commands will:
- run tests in the source file
- transform the interpreter and put the result into `out/lambda-value.rkt`
- run tests in the result file

# Example programs
All example interpreters are placed in directory `interpreters/src`.
They require a library in `interpreters/lib` by relative path so it is recommended to put results into immediate subdirectory of `interpreters`

