#lang racket

(require "../lib/idl.rkt")
(require
  (for-syntax syntax/parse
              racket/syntax))

; begin interpreter

(def-data expr
  string                  ;; variable
  integer                 ;; number
  {lam string expr}       ;; lambda abstraction
  {app expr expr}         ;; application
  {add expr expr})        ;; addition on numbers

(def eval (env term)
  (match term
    ([string x] (env x))
    ([integer n] n)
    ({lam x body}
     (lambda (v) (eval (extend env x v) body)))
    ({app f x} ((eval env f) (eval env x)))
    ({add n m} (+ (eval env n) (eval env m)))
    ))

(def extend (env k v)
  (lambda (x)
    (match (eq? x k)
      (#t v)
      (#f (env x)))))

(def init (x) (error "empty environment"))

(def main ([term expr])
  (eval init term))

; end interpreter

(module+ test
  (require rackunit)
  (check-equal? (main 42) 42)
  (check-equal? (main (app (lam "x" "x") 42)) 42)
  
  (define-syntax (lam* stx)
    (syntax-parse stx
      [(_ (v) body) #'(lam v body)]
      [(_ (v vs ...+) body) #'(lam v (lam* (vs ...) body))]))

  (define-syntax (app* stx)
    (syntax-parse stx
      [(_ f v) #'(app f v)]
      [(_ f v vs ...+) #'(app* (app f v) vs ...)]))

  (let*
    ([zero (lam* ("s" "z") "z")]
     [succ (lam* ("n" "s" "z") {app "s" (app* "n" "s" "z")})]
     [two {app succ {app succ zero}}]
     [plus (lam*  ("n" "m" "s" "z") (app* "n" "s" (app* "m" "s" "z")))]
     [pgm (app* plus two two {lam "n" {add "n" 1}} 0)])
    (check-equal? (main pgm) 4))
)
 