#lang racket

(require "../lib/idl.rkt")
(require
  (for-syntax syntax/parse
              racket/syntax))

; begin interpreter

(def-data Term
  String
  Integer
  {Lam String Term}
  {App Term Term}
  {Add Term Term}
  {Raise Term}
  {Try Term String Term})

(def-data Res
  {Err Any}
  {Ok Any})

(def-struct {Fun body env x})
(def-struct {Fun1 err f k})
(def-struct {Fun2 arg env err k})
(def-struct {Fun3 k n})
(def-struct {Fun4 env err k m})
(def-struct {Fun5 env err handle k x})
(def-struct {Extend env k v})
(def-struct {Fun6 })
(def-struct {Fun7 })
(def-struct {Init })
(def eval (env [Term term] k err)
  (match term
    ([String x] (apply k (lookup env x)))
    ([Integer n] (apply k n))
    ({Lam x body} (apply k {Fun body env x}))
    ({App fn arg}
      (let k1 {Fun2 arg env err k})
      (eval env fn k1 err))
    ({Add n m}
      (let k1 {Fun4 env err k m})
      (eval env n k1 err))
    ({Raise t} (eval env t err err))
    ({Try t x handle} (eval env t k {Fun5 env err handle k x}))))

(def extend (env k v) {Extend env k v})

(def init (x) (error "empty environment"))

(def apply1 (fn1 v k err)
  (match fn1 ({Fun body env x} (eval (extend env x v) body k err))))

(def apply (fn2 v)
  (match fn2
    ({Fun1 err f k} (apply1 f v k err))
    ({Fun2 arg env err k} (eval env arg {Fun1 err v k} err))
    ({Fun3 k n} (apply k (+ n v)))
    ({Fun4 env err k m} (eval env m {Fun3 k v} err))
    ({Fun5 env err handle k x} (eval (extend env x v) handle k err))
    ({Fun6 } {Ok v})
    ({Fun7 } {Err v})))

(def lookup (fn3 x)
  (match fn3
    ({Extend env k v}
      (match (eq? x k)
        (#t v)
        (#f (lookup env x))))
    ({Init } (init x))))

(def main ([Term term]) (eval {Init } term {Fun6 } {Fun7 }))

; end interpreter

(module+ test
  (require rackunit)
  (check-equal? (main 42) {Ok 42})
  (check-equal? (main (App (Lam "x" "x") 42)) {Ok 42})
  
  (define-syntax (lam* stx)
    (syntax-parse stx
      [(_ (v) body) #'(Lam v body)]
      [(_ (v vs ...+) body) #'(Lam v (lam* (vs ...) body))]))

  (define-syntax (app* stx)
    (syntax-parse stx
      [(_ f v) #'(App f v)]
      [(_ f v vs ...+) #'(app* (App f v) vs ...)]))

  (let*
    ([zero (lam* ("s" "z") "z")]
     [succ (lam* ("n" "s" "z") {App "s" (app* "n" "s" "z")})]
     [two {App succ {App succ zero}}]
     [plus (lam*  ("n" "m" "s" "z") (app* "n" "s" (app* "m" "s" "z")))]
     [pgm (app* plus two two {Lam "n" {Add "n" 1}} 0)]
     [pgm1 {Try {Add 1 {Raise 17}} "x" {Add 25 "x"}}])
    (check-equal? (main pgm) {Ok 4})
    (check-equal? (main pgm1) {Ok 42}))
)
 