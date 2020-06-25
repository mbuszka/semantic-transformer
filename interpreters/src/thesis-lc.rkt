#lang racket
(require "../lib/idl.rkt")

; begin interpreter
(def-data Term
  String            ;; variable
  {Abs String Term} ;; lambda abstraction
  {App Term Term}   ;; application
  {Unit})           ;; unit value (for testing)

(def eval (env [Term term])
  (match term
    ([String x] (env x))
    ({Abs x body}
     (fun (v) (eval (extend env x v) body)))
    ({App fn arg} ((eval env fn) (eval env arg)))
    ({Unit} {Unit})))

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
  (define pgm
    {App {Abs "x" "x"} {Unit}})
  (check-equal? (main pgm) {Unit})
) 