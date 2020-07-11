#lang racket

(require "../lib/idl.rkt")
(require
  (for-syntax syntax/parse
              racket/syntax))

; begin interpreter

(def-data Term
  String                  ;; variable
  Integer                 ;; number
  {Lam String Term}       ;; lambda abstraction
  {App Term Term}         ;; application
  {Add Term Term}         ;; addition on numbers
  {Raise Term}
  {Try Term String Term})

(def-data Res
  {Err Any}
  {Ok Any})

(def eval #:atomic (env [Term term] k err)
  (match term
    ([String x] (k (env x)))
    ([Integer n] (k n))
    ({Lam x body}
      (k (fun #:atomic (v k err) (eval (extend env x v) body k err))))
    ({App fn arg}
      (let k1 (fun #:atomic (f) (eval env arg (fun #:atomic (v) (f v k err)) err)))
      (eval env fn k1 err))
    ({Add n m}
      (let k1 (fun #:atomic (n) (eval env m (fun #:atomic (m) (k (+ n m))) err)))
      (eval env n k1 err))
    ({Raise t} (eval env t err err))
    ({Try t x handle}
      (eval env t k (fun #:atomic (v) (eval (extend env x v) handle k err))))
    ))

(def extend #:atomic (env k v)
  (fun #:atomic #:name Extend #:apply lookup (x)
    (match (eq? x k)
      (#t v)
      (#f (env x)))))

(def init #:atomic (x) (error "empty environment"))

(def main ([Term term])
  (eval init term (fun #:atomic (v) {Ok v}) (fun #:atomic (v) {Err v})))

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
 