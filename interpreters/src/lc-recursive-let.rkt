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
  {If0 Term Term Term}
  {Let-Rec String String Term Term})

(def eval (env [Term term])
  (match term
    ([String x] (env x))
    ([Integer n] n)
    ({Lam x body} (ev-lambda env x body))
    ({Let-Rec f x body rest}
      (let env (extend-rec env f x body))
      (eval env rest))
    ({App fn arg} ((eval env fn) (eval env arg)))
    ({Add n m} (+ (eval env n) (eval env m)))
    ({If0 cond then else}
      (match (eval env cond)
        (0 (eval env then))
        (_ (eval env else))))
    ))

(def ev-lambda #:atomic (env x body)
  (fun (v) (eval (extend env x v) body)))

(def extend-rec #:atomic (env f y body)
  (fun #:atomic #:name Ext-Rec (x)
    (match (eq? x f)
      (#t (ev-lambda (extend-rec env f y body) y body))
      (#f (env x)))))

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

  (let*
    ([pgm 
    {Let-Rec "sum" "n" 
      {If0 "n"
        0
        {Add "n" {App "sum" {Add "n" -1}}}}
        
      {App "sum" 5}}])
    (check-equal? (main pgm) 15))
)
 