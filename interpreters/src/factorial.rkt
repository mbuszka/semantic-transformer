#lang racket

(require "../lib/idl.rkt")

; begin interpreter

(def factorial (n)
  (match (< 0 n)
    (#t (* n (factorial (- n 1))))
    (#f 1)))

(def main ([Integer n]) (factorial n))

; end interpreter

(module+ test
  (require rackunit)
  (check-equal? (main 5) 120))