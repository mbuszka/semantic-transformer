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
  {Add Term Term})

(def-struct {Fun body env x})
(def-struct {App2 cont var1})
(def-struct {App1 arg cont env})
(def-struct {Add2 cont var3})
(def-struct {Add1 cont env m})
(def-struct {Extend env k v})
(def-struct {Halt })
(def-struct {Init })
(def eval (env [Term term] cont)
  (match term
    ([String x] (continue1 cont (lookup env x)))
    ([Integer n] (continue1 cont n))
    ({Lam x body} (continue1 cont {Fun body env x}))
    ({App fn arg} (eval env fn {App1 arg cont env}))
    ({Add n m} (eval env n {Add1 cont env m}))))

(def extend (env k v) {Extend env k v})

(def init (x) (error "empty environment"))

(def apply (fn1 v cont1)
  (match fn1 ({Fun body env x} (eval (extend env x v) body cont1))))

(def continue1 (fn2 var2)
  (match fn2
    ({App2 cont var1} (apply var1 var2 cont))
    ({App1 arg cont env} (eval env arg {App2 cont var2}))
    ({Add2 cont var3} (continue1 cont (+ var3 var2)))
    ({Add1 cont env m} (eval env m {Add2 cont var2}))
    ({Halt } var2)))

(def lookup (fn3 x)
  (match fn3
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
)
 