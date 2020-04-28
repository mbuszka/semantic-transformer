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
  {Add Term Term})        ;; addition on numbers

(def eval (env [Term term])
  (match term
    ([String x] (env x))
    ([Integer n] n)
    ({Lam x body}
     (lambda (v) (eval (extend env x v) body)))
    ({App f x} ((eval env f) (eval env x)))
    ({Add n m} (+ (eval env n) (eval env m)))
    ))

(def extend #:atomic (env k v)
  (lambda #:atomic (x)
    (match (eq? x k)
      (#t v)
      (#f (env x)))))

(def init #:atomic (x) (error "empty environment"))

(def main ([Term term])
  (eval init term))

; end interpreter

(module+ test
  (require rackunit)
  (check-equal? (main 42) 42)
  (check-equal? (main (App (Lam "x" "x") 42)) 42)
  
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
     [pgm (app* plus two two {Lam "n" {Add "n" 1}} 0)])
    (check-equal? (main pgm) 4))
)
 