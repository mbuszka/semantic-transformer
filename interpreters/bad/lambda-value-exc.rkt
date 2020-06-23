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
  {Raise Integer}
  {Try Term String Term})

(def-data Res
  {Err Integer}
  {Ok Any})

(def eval (env [Term term])
  (match term
    ([String x] {Ok (env x)})
    ([Integer n] {Ok n})
    ({Lam x body}
     {Ok (fun (v) (eval (extend env x v) body))})
    ({App fn arg}
      (match (eval env fn)
        ({Err e} {Err e})
        ({Ok f} 
          (match (eval env arg)
            ({Err e} {Err e})
            ({Ok v} (f v))))))
    ({Add n m}
      (match (eval env n)
        ({Err e} {Err e})
        ({Ok n}
          (match (eval env m)
            ({Err e} {Err e})
            ({Ok m} {Ok (+ n m)})))))
    ({Raise n} {Err n})
    ({Try t x handle}
      (match (eval env t)
        ({Ok v} {Ok v})
        ({Err e} (eval (extend env x e) handle))))
    ))

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
 