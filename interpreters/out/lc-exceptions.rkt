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
(def-struct {App2 cont var3})
(def-struct {App1 arg cont env})
(def-struct {Add2 cont var5})
(def-struct {Add1 cont env m})
(def-struct {Raise1 cont})
(def-struct {Try1 cont env handle x})
(def-struct {Extend env k v})
(def-struct {Halt })
(def-struct {Init })
(def eval (env [Term term] cont)
  (match term
    ([String x] (continue1 cont {Ok (lookup env x)}))
    ([Integer n] (continue1 cont {Ok n}))
    ({Lam x body} (continue1 cont {Ok {Fun body env x}}))
    ({App fn arg} (eval env fn {App1 arg cont env}))
    ({Add n m} (eval env n {Add1 cont env m}))
    ({Raise t} (eval env t {Raise1 cont}))
    ({Try t x handle} (eval env t {Try1 cont env handle x}))))

(def app (f arg cont2)
  (match f
    ({Err e} (continue1 cont2 {Err e}))
    ({Ok f}
      (match arg
        ({Err e} (continue1 cont2 {Err e}))
        ({Ok v} (apply f v cont2))))))

(def add (n m cont3)
  (match n
    ({Err e} (continue1 cont3 {Err e}))
    ({Ok n}
      (match m
        ({Err e} (continue1 cont3 {Err e}))
        ({Ok m} (continue1 cont3 {Ok (+ n m)}))))))

(def extend (env k v) {Extend env k v})

(def init (x) (error "empty environment"))

(def apply (fn1 v cont1)
  (match fn1 ({Fun body env x} (eval (extend env x v) body cont1))))

(def continue1 (fn2 var4)
  (match fn2
    ({App2 cont var3} (app var3 var4 cont))
    ({App1 arg cont env} (eval env arg {App2 cont var4}))
    ({Add2 cont var5} (add var5 var4 cont))
    ({Add1 cont env m} (eval env m {Add2 cont var4}))
    ({Raise1 cont}
      (continue1
        cont
        (match var4
          ({Ok v} {Err v})
          ({Err e} {Err e}))))
    ({Try1 cont env handle x}
      (match var4
        ({Ok v} (continue1 cont {Ok v}))
        ({Err e} (eval (extend env x e) handle cont))))
    ({Halt } var4)))

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
 