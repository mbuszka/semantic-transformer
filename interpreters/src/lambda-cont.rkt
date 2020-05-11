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

(def extend #:atomic (env k v)
  (fun #:atomic (x)
    (match (eq? x k)
      (#t v)
      (#f (env x)))))

(def init #:atomic (x) (error "empty environment"))

(def eval (term env k)
  (match term
    ([Integer n] (k n))
    ([String x] (k (env x)))
    ({Lam x body} (k (fun (val k) (eval body (extend env x val) k))))
    ({App fn arg} (eval fn env (fun (fn) (eval arg env (fun (arg) (fn arg k))))))
    ({Succ t} (eval t env (fun (n) (k (+ n 1)))))
    ({Reset t} (eval t env (fun (v) v)))
    ({Shift k1 t}
      (let c (fun (v k1) (k1 (k v))))
      (eval t (extend env k1 c) (fun (v) v)))))

(def main ([Term term]) (eval term init (fun (v) v)))

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