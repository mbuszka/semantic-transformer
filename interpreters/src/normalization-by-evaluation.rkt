#lang racket

(require "../lib/idl.rkt")

; begin interpreter

(def-data Term
  {Var Integer}
  {App Term Term}
  {Abs Term})

(def-struct {Level Integer})
(def-struct {Fun Any})

(def cons #:atomic (val env)
  (fun #:atomic #:no-defun (n)
    (match n
      (0 val)
      (_ (env (- n 1))))))

(def reify (ceil val)
  (match val
    ({Fun f}
      {Abs (reify (+ ceil 1) (f {Level ceil}))})
    ({Level k} {Var (- ceil (+ k 1))})
    ({App f arg} {App (reify ceil f) (reify ceil arg)})))

(def apply (f arg)
  (match f
    ({Fun f} (f arg))
    (_ {App f arg})))

(def eval (expr env)
  (match expr
    ({Var n} (env n))
    ({App f arg} (apply (eval f env) (eval arg env)))
    ({Abs body} {Fun (fun #:name Closure (x) (eval body (cons x env)))})))

(def run (term)
  (reify 0 
    (eval term (fun #:atomic #:no-defun (x) (error "empty env")))))

(def main ([Term term]) (run term))

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