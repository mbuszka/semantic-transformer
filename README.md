# Building the project
The project is built with `cabal`

`cabal build` -- to build

# Running examples
`cabal run semt -- /path/to/definitions`

This command will perform all transformations, run tests on intermediate
results and display final program.

# Example programs
`test/lambda.smt` -- evaluator for λ-calculus with some tests
