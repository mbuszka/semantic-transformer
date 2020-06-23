#lang racket

(require "../lib/idl.rkt")

; begin interpreter

(def-data Term
  {Var Integer}
  {App Term Term}
  {Abs Term})

(def-data Env
  {Nil}
  {Cons Any Env})

(def-data Dom
  {Fun Any}
  {Level Integer}
  {DApp Dom Dom})

(def nth ([Env env] [Integer n])
  (match env
    ({Nil} (error "empty env"))
    ({Cons v env}
     (match n
       (0 v)
       (_ (nth env (- n 1)))))))

(def reify (ceil val)
  (match val
    ({Fun f}
      {Abs (reify (+ ceil 1) (f {Level ceil}))})
    ({Level k} {Var (- ceil (+ k 1))})
    ({DApp f arg} {App (reify ceil f) (reify ceil arg)})
  ))

(def apply (f arg)
  (match f
    ({Fun f} (f arg))
    (_ {DApp f arg})
    ))

(def eval ([Term expr] [Env env])
  (match expr
    ({Var n} (nth env n))
    ({App f arg} (apply (eval f env) (eval arg env)))
    ({Abs body} {Fun (fun #:name Closure (x) (eval body {Cons x env}))})
  ))

(def run (term) (reify 0 (eval term {Nil})))

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
  
  ; (define-syntax (app* stx)
  ;   (syntax-parse stx
  ;     [(_ f v) #'(App f v)]
  ;     [(_ f v vs ...+) #'(app* (App f v) vs ...)]))

  ; (let*
  ;   ([omega {App {Abs {App {Var 0} {Var 0}}} {Abs {App {Var 0} {Var 0}}}}]
  ;    [const {Abs {Abs 1}}])
  ;   (check-equal? (main (app* const id omega)) id))
)