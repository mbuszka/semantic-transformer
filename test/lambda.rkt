#lang racket

(require
  (for-syntax syntax/parse
              racket/syntax))

(begin-for-syntax
  (define-syntax-class type
    #:attributes (tp contract struct-def)
    (pattern tp:type-literal
             #:with contract (format-id #'tp "~a?" #'tp)
             #:attr struct-def #f)
    (pattern tp:id
             #:attr struct-def #f
             #:with contract (format-id #'tp "~a/c" #'tp))
    (pattern (tp:id field:type ...)
             #:with c-name (format-id #'name "~a/c" #'tp)
             #:with contract #'(recursive-contract c-name #:flat)
             #:with (fields ...) (generate-temporaries #'(field.tp ...))
             #:with struct-def
             #'((struct tp (fields ...))
                (define c-name (struct/c tp field.contract ...)))
             ))

  (define-syntax-class type-literal
    (pattern (~literal integer))
    (pattern (~literal string))
    (pattern (~literal boolean)))

  (define-syntax-class def-arg
    (pattern name:id
             #:with contract #'any/c)
    (pattern [name:id tp:type]
             #:with contract #'tp.contract))
  )

(define-syntax (def-data stx)
    (syntax-parse stx
      [(_ name:id elems:type ...)
       #:with ((struct-def contract-def) ...) #'((~? elems.struct-def) ...)
       #:with c-name (format-id #'name "~a/c" #'name)
       #'(begin
           (define c-name (flat-named-contract 'name (or/c elems.contract ...)))
           struct-def ...
           contract-def ...
           )]))

(define-syntax (def stx)
  (syntax-parse stx
    [(_ name:id (arg:def-arg ...) body)
     #:with contract #'(-> arg.contract ... any/c)
     #'(begin
         (define/contract (name arg.name ...) contract body))]))

(define-syntax (define-base-types stx)
  (syntax-parse stx
    [(_ type:id ...)
     #:with (pred ...) (for/list ([tp (syntax->list #'(type ...))]) (format-id tp "~a?" tp))
     #'(begin
         (define-match-expander type (lambda (stx) (syntax-case stx () [(_ p) #'(? pred p)]))) ...)]))

(define-base-types integer string boolean)

; begin interpreter

(def-data expr
  string
  integer
  {lam string expr}
  {app expr expr}
  {let_ string expr expr}
  {add expr expr})

(def eval (env term)
  (match term
    ([string x] (env x))
    ({lam x body}
     (lambda (v) (eval (extend env x v) body)))
    ({let_ x e b}
     (eval env {app {lam x b} e}))
    ({app f x} ((eval env f) (eval env x)))
    ({add n m} (+ (eval env n) (eval env m)))
    ([integer n] n)))

(def extend (env k v)
  (lambda (x)
    (match (eq? x k)
      (#t v)
      (#f (env x)))))

(def init (x) (error "empty environment"))

(def main ([term expr])
  (eval init term))

; end interpreter

(main 42)
(main (app (lam "x" "x") 42))
(main (let_ "foo" 42 "foo"))
;(main '(not a program))

; (def-test "number evaluates to number"
;   (42)
;   42)

; (def-test "simple call"
;   ({app {lam "x" "x"} 42})
;   42)

; (def-test "higher order call"
;   ({app 
;     {app
;       {lam "f" {lam "x"
;         {app "f" "x"}}}
;       {lam "x" "x"}}
;     42})
;   42)

; (def-test "let binding"
;   ({let_ "id" {lam "x" "x"}
;       {app "id" 42}})
;   42)

(main {let_ "zero" {lam "s" {lam "z" "z"}}
       {let_ "succ" {lam "n" {lam "s" {lam "z" 
                                           {app "s" {app {app "n" "s"} "z"}}}}}
             {let_ "plus" {lam "n" {lam "m" {lam "s" {lam "z"
                                                          {app {app "n" "s"} {app {app "m" "s"} "z"}}}}}}
                   {let_ "plus1" {lam "n" {add "n" 1}}
                         {let_ "one" {app "succ" "zero"}
                               {let_ "two" {app {app "plus" "one"} "one"}
                                     {app {app "two" "plus1"} 0}}}}}}})
 