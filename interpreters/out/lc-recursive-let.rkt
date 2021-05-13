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
  {If0 Term Term Term}
  {Let-Rec String String Term Term})

(def-struct {Fun body env x})
(def-struct {App2 cont var})
(def-struct {App1 arg cont env})
(def-struct {Add2 cont var2})
(def-struct {Add1 cont env m})
(def-struct {If01 cont else env then})
(def-struct {Ext-Rec body env f y})
(def-struct {Extend env k v})
(def-struct {Halt })
(def-struct {Init })
(def eval (env [Term term] cont)
  (match term
    ([String x] (continue1 cont (lookup env x)))
    ([Integer n] (continue1 cont n))
    ({Lam x body} (continue1 cont (ev-lambda env x body)))
    ({Let-Rec f x body rest}
      (let env (extend-rec env f x body))
      (eval env rest cont))
    ({App fn arg} (eval env fn {App1 arg cont env}))
    ({Add n m} (eval env n {Add1 cont env m}))
    ({If0 cond then else} (eval env cond {If01 cont else env then}))))

(def ev-lambda (env x body) {Fun body env x})

(def extend-rec (env f y body) {Ext-Rec body env f y})

(def extend (env k v) {Extend env k v})

(def init (x) (error "empty environment"))

(def apply (fn1 v cont1)
  (match fn1 ({Fun body env x} (eval (extend env x v) body cont1))))

(def continue1 (fn2 var1)
  (match fn2
    ({App2 cont var} (apply var var1 cont))
    ({App1 arg cont env} (eval env arg {App2 cont var1}))
    ({Add2 cont var2} (continue1 cont (+ var2 var1)))
    ({Add1 cont env m} (eval env m {Add2 cont var1}))
    ({If01 cont else env then}
      (match var1
        (0 (eval env then cont))
        (_ (eval env else cont))))
    ({Halt } var1)))

(def lookup (fn3 x)
  (match fn3
    ({Ext-Rec body env f y}
      (match (eq? x f)
        (#t (ev-lambda (extend-rec env f y body) y body))
        (#f (lookup env x))))
    ({Extend env k v}
      (match (eq? x k)
        (#t v)
        (#f (lookup env x))))
    ({Init } (init x))))

(def main ([Term term]) (eval {Init } term {Halt }))

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
 