# Examples
Example interpreters are located in `interpreters` directory.
- The subdirectory `src` contains source interpreters
- The subdirectory `out` contains abstract machines generated from source interpreters
- The subdirectory `lib` contains syntax definitions `reuire`d by interpreters

Both source interpreters and generated abstract machines are executable by _Racket_. To run the test suite of an interpreter (or a machine) use `raco`, e.g for call-by-value lambda calculus:
```
cd interpreters
raco test src/lc-call-by-value.rkt
raco test out/lc-call-by-value.rkt
```

# Installation
The semantic transformer - `semt` is built from source using `cabal` - a _Haskell_ package manager.

- `cabal build` to build
- `cabal install` to install `semt` executable
- `cabal run semt --` to run the development build without installing
- `make test` to test the tool on all example interpreters
- `make clean` to delete all generated interpreters

# Tool usage
The basic mode of usage is to transform a `file.rkt` containing an interpreter into `out/file.rkt` which is a source file containing the transformed interpreter (i.e., an abstract machine) using the command `semt file.rkt`.
The options modifying the behavior of the tool can be displayed with the command `semt --help`:
```
Usage: semt FILE [-o|--output DIR] [-i|--intermediate] 
            [-d|--debug] [-t|--self-test]
  Transform an interpreter into an abstract machine.

Available options:
  FILE                     Source file with the interpreter.
  -o,--output DIR          Output directory for generated
                            files, defaults to ./out/
  -i,--intermediate        Emit executable source files for
                            each stage.
  -d,--debug               Emit labeled source files for
                            each stage.
  -t,--self-test           Run raco test on each
                            intermediate file; implies
                            --intermediate
  -h,--help                Show this help text
```

# Source File Format
The tool assumes that the source file with an interpreter is a _Racket_ program.
An example source listing is shown below.

```startline=0
#lang racket
(require "../lib/idl.rkt")

; begin interpreter
(def-data Term
  String            ;; variable
  {Abs String Term} ;; lambda abstraction
  {App Term Term}   ;; application
  {Unit})           ;; unit value (for testing)

(def eval (env [Term term])
  (match term
    ([String x] (env x))
    ({Abs x body}
     (fun (v) (eval (extend env x v) body)))
    ({App fn arg}
      (let fn (eval env fn))
      (let arg (eval env arg))
      (fn arg))
    ({Unit} {Unit})))

(def extend #:atomic (env k v)
  (fun #:atomic #:name Extend #:apply lookup (x)
    (match (eq? x k)
      (#t v)
      (#f (env x)))))

(def init #:atomic (x) (error "empty environment"))

(def main ([Term term])
  (eval init term))
; end interpreter

(module+ test
  (require rackunit)
  (define pgm
    {App {Abs "x" "x"} {Unit}})
  (check-equal? (main pgm) {Unit})
) 
```

The preamble (everything up to the `; begin interpreter` marker in line 4) is copied verbatim by the tool and should be used to specify _Racket_'s dialect (line 1) and import syntax definitions (line 2).
Afterwards an interpreter is specified (lines 4 -- 31) and it ends with another marker `; end interpreter` in line 32.
Everything following the marker is again copied to the output file.
In the example this space is used to define some tests for the interpreter.

# Syntax
The syntax of _IDL_ is given in the table below.

|                   |     |     |
| ---               | ---:| --- |
| _data-def_        | ::= | `(def-data `_tp-name_ _tp-elem_ ... `)` |
| _tp-elem_         | ::= | _tp_ \| _record_ |
| _record_          | ::= | `{` _tp-name_ _record-field_ ...`}` |
| _record-field_    | ::= | _tp_ \| _var_ \| `[` _tp_ _var_ `]` |
| _record-def_      | ::= | `(def-struct `_record_ `)` |
| _base-tp_         | ::= | `String` \| `Integer` \| `Boolean` |
| _tp_              | ::= | `Any` \| _base-tp_ \| _tp-name_ |
| _fun-def_         | ::= | `(def` _var_ _annot_ ... `(`_arg_ ...`)` _statements_ `)` |
| _annot_           | ::= | `#:no-defun` \| `#:atomic` \| `#:name` _tp-name_ \| `#:apply` _var_ |
| _arg_             | ::= | _var_ \| `[`_tp_ _var_ `]` |
| _const_           | ::= | _integer_ \| _string_ \| `#t` \| `#f` |
| _statements_      | ::= | `(let` _var_ _term_ `)` _statements_ \| _term_ |
| _term_            | ::= | _var_  \| `(fun` _annot_ ... `(`_arg_ ...`)` _statements_`)` \| `(`_term_ _term_ ... `)` |
|                  | \| | `{` _tp-name_ _term_ ...`}` \| `(match` _term_ _branch_ ... `)` \| `(error` _string_ `)` |
| _branch_ | ::= | `(` _pattern_ _statements_ `)` |
| _pattern_ | ::= | _var_ \| _const_ \| `_` \| `[`_base-tp_ _var_`]` \| `{` _tp-name_ _pattern_ ...`}` |

The interpreter consists of a sequence of datatype (_data-def_), record (_struct-def_) and function (_fun-def_) definitions.
One of the functions must be named `main` and will serve as an entry point of the interpreter.
It is required for the `main` function to have parameters annotated with types, e.g., `(def main ([Term term]) body)`.
Names (_tp-name_) of datatypes and records must be unique and distinct from base types (_base-tp_ and `Any`).
All definitions may be mutually recursive.
The term syntax is split into terms (_term_) and statements (_statements_) where the latter appear as the bodies of function definitions (both top-level and anonymous) and branches.
Statements are sequences of let-bindings terminated by a regular term (see lines 16-19 of the example program).
Variables (_var_) and type names (_tp-name_) may consist of _ASCII_ letters, decimal digits and the following symbols: `-+/*_?<`.
Variables begin either with a lower-case letter or symbol and type names begin with an upper-case letter.

# Execution
In order to run an interpreter embedded in a source file, the library with syntax definitions (`idl.rkt`) provided with this thesis must be imported.
The file is located in the source tree of the project in the `interpreters/lib/` directory.
The run-time behavior of the program follows the call-by-value behavior of _Racket_, e.g.:
- `let` evaluates the bound term to a value before binding.
  The binding is visible only in following statements.
- Application evaluates terms left-to-right and then applies the value in function position.
- `match` evaluates the term under scrutiny and tests patterns against the value in order of their definition continuing with the first match.

The interpreters may use the following builtin operations: `+`, `-`, `*`, `/`, `neg`, `not`, `and`, `or`, `eq?`, `<` with the usual semantics.

# Annotations
- `#:apply` specifies the name for _apply_ function generated for the defunctionalized function space whose member is the annotated function.
- `#:name` specifies the name for the record which will represent the annotated function after defunctionalization.
- `#:no-defun` skips defunctionalization of the annotated function (either all or none of the functions in a space must be annotated).
-  `#:atomic` means that the annotated function (and calls to it) will be left in direct style during translation to CPS.

# For developers
### Project Structure
- `semt/Main.hs` is the `Main` module, which contains the `main` function, parses command line arguments and calls `Pipeline.run` which performs the transformation
- module `Pipeline` contains the transformation pipeline and its components
- module `Syntax` contains all syntax definitions used in the transformer. The `TermF` is a functor over its subtrees to accomodate different representations of syntax trees (e.g. `Term` defined in `src/Syntax.hs` or `Label` used in the abstract interpreter `src/AbsInt/Types.hs`)
- module `Prelude` contains the prelude imported implicitly into every module in `src/`
- module `AbsInt` contains the abstract abstract machine used to compute control-flow analysis

The project uses `Polysemy` to manage computational effects.