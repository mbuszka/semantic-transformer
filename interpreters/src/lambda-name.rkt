#lang racket

(require "../lib/idl.rkt")

; begin interpreter

(def-data Term
  Integer
  {App Term Term}
  {Abs Term}
  {Unit})

(def-data Env
  {Nil}
  {Cons Any Env})

(def nth ([Env env] [Integer n])
  (match env
    ({Nil} (error "empty env"))
    ({Cons v env}
     (match n
       (0 v)
       (_ (nth env (- n 1)))))))

(def eval ([Term expr] [Env env])
  (match expr
    ([Integer n] ((nth env n)))
    ({App f x} ((eval f env) (lambda () (eval x env))))
    ({Abs body} (lambda (x) (eval body {Cons x env})))
    ({Unit} {Unit})
  ))

(def main ([Term term]) (eval term {Nil}))

; end interpreter

(module+ test
  (require rackunit)
  (require
    (for-syntax syntax/parse
                racket/syntax))
  (check-equal? (main {Unit}) {Unit})
  (check-equal? (main (App (Abs 0) {Unit})) {Unit})
  
  (define-syntax (app* stx)
    (syntax-parse stx
      [(_ f v) #'(App f v)]
      [(_ f v vs ...+) #'(app* (App f v) vs ...)]))

  (let*
    ([omega {App {Abs {App 0 0}} {Abs {App 0 0}}}]
     [const {Abs {Abs 1}}])
    (check-equal? (main (app* const {Unit} omega)) {Unit}))
)