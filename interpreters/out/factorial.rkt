#lang racket

(require "../lib/idl.rkt")

; begin interpreter


(def-struct {Cont cont n})
(def-struct {Halt })
(def factorial (n cont)
  (match (< 0 n)
    (#t (factorial (- n 1) {Cont cont n}))
    (#f (continue cont 1))))

(def continue (fn var4)
  (match fn
    ({Cont cont n} (continue cont (* n var4)))
    ({Halt } var4)))

(def main ([Integer n]) (factorial n {Halt }))

; end interpreter

(module+ test
  (require rackunit)
  (check-equal? (main 5) 120))