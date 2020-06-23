#lang racket

(require "../lib/idl.rkt")

; begin interpreter

(def-data Atom
  String
  {Or Goal Goal}
  {Cut}
  {Fail})

(def-data Goal
  {GNil}
  {GCons Atom Goal})

(def-data Program
  {PNil}
  {PCons String Goal Program})

(def run-goal #:atomic ([Goal goal] [Program program] success failure cut)
  (match goal
    ({GNil} (success failure))
    ({GCons atom goal} (run-seq atom goal program success failure cut))))

(def run-seq #:atomic (atom goal program success failure cut)
  (match goal
    ({GNil} (run-atom atom program success failure cut))
    ({GCons atom-1 goal}
      (run-atom atom program (fun #:atomic #:name Sequence (failure) (run-seq atom-1 goal program success failure cut)) failure cut))))

(def run-atom #:atomic (atom program success failure cut)
  (match atom
    ([String x]
      (match (lookup x program)
        (#f (failure))
        (g (run-goal g program success failure failure))))
    ({Or l r} (run-goal l program success (fun #:atomic #:name Backtrack () (run-goal r program success failure cut)) cut))
    ({Cut} (success cut))
    ({Fail} (failure))))

(def lookup #:atomic (x program)
  (match program
    ({PNil} #f)
    ({PCons i g p}
      (match (eq? x i)
        (#t g)
        (#f (lookup x p))))))

(def done #:atomic () #f)

(def main ([Program pgm] [Goal goal])
  (run-goal goal pgm (fun #:atomic (failure) #t) done done))

; end interpreter

(module+ test
  (require rackunit)
  (require (for-syntax syntax/parse))

  (define-syntax (pgm stx)
    (syntax-parse stx
      [(_) #'{PNil}]
      [(_ (name goals) rest ...)
       #'{PCons name goals (pgm rest ...)}]))

  (define program
    (pgm
      ("A" {GNil})
      ("B" {GNil})
      ("C" {GCons "A" {GNil}})
      ("D" {GCons {Or {GCons "E" {GNil}} {GCons "B" {GCons "C" {GNil}}}} {GNil}})
    ))

  (check-equal? (main program {GCons "D" {GNil}}) #t)
)
