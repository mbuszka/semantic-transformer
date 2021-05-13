#lang racket

(require "../lib/idl.rkt")

; begin interpreter

(def-data Term
  {Var Integer}
  {App Term Term}
  {Abs Term})

(def-struct {Level Integer})
(def-struct {Fun Any})
(def-struct {Closure body env})
(def-struct {Fun2 cont})
(def-struct {Fun1 cont var3})
(def-struct {App2 cont var10})
(def-struct {App1 arg ceil cont})
(def-struct {App4 cont2 var12})
(def-struct {App3 arg cont2 env})
(def-struct {Cont cont4 var16})
(def-struct {Halt })
(def cons (val env)
  (fun (n)
    (match n
      (0 val)
      (_ (env (- n 1))))))

(def reify (ceil val cont)
  (match val
    ({Fun f} (apply1 f {Level ceil} {Fun1 cont (+ ceil 1)}))
    ({Level k} (continue1 cont {Var (- ceil (+ k 1))}))
    ({App f arg} (reify ceil f {App1 arg ceil cont}))))

(def apply (f arg cont1)
  (match f
    ({Fun f} (apply1 f arg cont1))
    (_ (continue cont1 {App f arg}))))

(def eval (expr env cont2)
  (match expr
    ({Var n} (continue cont2 (env n)))
    ({App f arg} (eval f env {App3 arg cont2 env}))
    ({Abs body} (continue cont2 {Fun {Closure body env}}))))

(def run (term cont4) (eval term (fun (x) (error "empty env")) {Cont cont4 0}))

(def apply1 (fn x cont3)
  (match fn ({Closure body env} (eval body (cons x env) cont3))))

(def continue1 (fn1 var11)
  (match fn1
    ({Fun2 cont} (continue1 cont {Abs var11}))
    ({App2 cont var10} (continue1 cont {App var10 var11}))
    ({App1 arg ceil cont} (reify ceil arg {App2 cont var11}))
    ({Halt } var11)))

(def continue (fn2 var13)
  (match fn2
    ({Fun1 cont var3} (reify var3 var13 {Fun2 cont}))
    ({App4 cont2 var12} (apply var12 var13 cont2))
    ({App3 arg cont2 env} (eval arg env {App4 cont2 var13}))
    ({Cont cont4 var16} (reify var16 var13 cont4))))

(def main ([Term term]) (run term {Halt }))

; end interpreter

(module+ test
  (require rackunit)
  (require
    (for-syntax syntax/parse
                racket/syntax))
  (define id {Abs {Var 0}})
  (check-equal? (main id) id)
  (check-equal? (main (App (Abs {Var 0}) id)) id)
  (check-equal? (main (Abs {App id id})) {Abs id})
  
  (define-syntax (app* stx)
    (syntax-parse stx
      [(_ f v) #'(App f v)]
      [(_ f v vs ...+) #'(app* (App f v) vs ...)]))

  (define (num-body n)
    (if (= n 0) {Var 0} {App {Var 1} (num-body (- n 1))}))

  (define (num n)
    {Abs {Abs (num-body n)}})

  (define (add n m) {Abs {Abs (app* n {Var 1} (app* m {Var 1} {Var 0}))}})

  (define (mul n m) {Abs {Abs (app* n (app* m {Var 1}) {Var 0})}})

  (check-equal? (main (add (num 3) (num 4))) (num 7))

  (check-equal? (main (mul (num 3) (num 4))) (num 12))

  ; (let*
  ;   ([omega {App {Abs {App {Var 0} {Var 0}}} {Abs {App {Var 0} {Var 0}}}}]
  ;    [const {Abs {Abs 1}}])
  ;   (check-equal? (main (app* const id omega)) id))
)