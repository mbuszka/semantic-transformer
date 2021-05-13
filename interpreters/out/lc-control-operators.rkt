#lang racket

(require "../lib/idl.rkt")
; begin interpreter

(def-data Term
  Integer
  String
  {Lam String Term}
  {App Term Term}
  {Succ Term}
  {Reset Term}
  {Shift String Term})

(def-struct {F-Clo body env x})
(def-struct {K-Arg arg env k})
(def-struct {K-App fn k})
(def-struct {K-Succ k})
(def-struct {K-Reset })
(def-struct {F-Cont k})
(def-struct {K-Shift })
(def-struct {K-Halt })
(def-struct {Extend env k v})
(def-struct {Shift1 cont6 k1})
(def-struct {Halt })
(def-struct {Init })
(def extend (env k v) {Extend env k v})

(def init (x) (error "empty environment"))

(def eval (term env k cont)
  (match term
    ([Integer n] (apply-k k n cont))
    ([String x] (apply-k k (lookup env x) cont))
    ({Lam x body} (apply-k k {F-Clo body env x} cont))
    ({App fn arg} (eval fn env {K-Arg arg env k} cont))
    ({Succ t} (eval t env {K-Succ k} cont))
    ({Reset t} (eval t env {K-Reset } cont))
    ({Shift k1 t}
      (let c {F-Cont k})
      (eval t (extend env k1 c) {K-Shift } cont))))

(def apply (fn1 v k1 cont1)
  (match fn1
    ({F-Clo body env x} (eval body (extend env x v) k1 cont1))
    ({F-Cont k} (apply-k k v {Shift1 cont1 k1}))))

(def apply-k (fn2 v cont2)
  (match fn2
    ({K-Arg arg env k} (eval arg env {K-App v k} cont2))
    ({K-App fn k} (apply fn v k cont2))
    ({K-Succ k} (apply-k k (+ v 1) cont2))
    ({K-Reset } (continue cont2 v))
    ({K-Shift } (continue cont2 v))
    ({K-Halt } (continue cont2 v))))

(def lookup (fn3 x)
  (match fn3
    ({Extend env k v}
      (match (eq? x k)
        (#t v)
        (#f (lookup env x))))
    ({Init } (init x))))

(def continue (fn4 var10)
  (match fn4
    ({Shift1 cont6 k1} (apply-k k1 var10 cont6))
    ({Halt } var10)))

(def main ([Term term]) (eval term {Init } {K-Halt } {Halt }))

; end interpreter

(module+ test
  (require rackunit)
  (require
    (for-syntax syntax/parse
                racket/syntax))
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
     [pgm (app* plus two two {Lam "n" {Succ "n"}} 0)])
    (check-equal? (main pgm) 4))

  (check-equal? (main (Reset (Succ (Succ (Shift "k" (Succ (App "k" 3))))))) 6)
  )