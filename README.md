# Building the project
The project is built with `cabal`

`cabal build` -- to build

# Running examples
`cabal run semt-exe -- /path/to/definitions` to perform control flow analysis and display intermediate representations

# Example programs
`test/lambda.smt` -- minimal evaluator for λ-calculus

`test/lambda-env.smt` -- evaluator with environments

`test/unary.smt` -- addition of unary encoded numbers, currently hangs the analysis
