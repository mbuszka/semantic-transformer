#lang racket

(require "../lib/idl.rkt")

; begin interpreter

(def-data Atom
  String
  {Or Goal Goal}
  {Cut }
  {Fail })

(def-data Goal
  {GNil }
  {GCons Atom Goal})

(def-data Program
  {PNil }
  {PCons String Goal Program})

(def-struct {Sequence atom-1 cut goal program success})
(def-struct {Backtrack cut failure program r success})
(def-struct {Fun })
(def-struct {Done })
(def run-goal ([Goal goal] [Program program] success failure cut)
  (match goal
    ({GNil } (apply success failure))
    ({GCons atom goal} (run-seq atom goal program success failure cut))))

(def run-seq (atom goal program success failure cut)
  (match goal
    ({GNil } (run-atom atom program success failure cut))
    ({GCons atom-1 goal}
      (run-atom
        atom program {Sequence atom-1 cut goal program success} failure cut))))

(def run-atom (atom program success failure cut)
  (match atom
    ([String x]
      (match (lookup x program)
        (#f (apply1 failure))
        (g (run-goal g program success failure failure))))
    ({Or l r}
      (run-goal
        l program success {Backtrack cut failure program r success} cut))
    ({Cut } (apply success cut))
    ({Fail } (apply1 failure))))

(def lookup (x program)
  (match program
    ({PNil } #f)
    ({PCons i g p}
      (match (eq? x i)
        (#t g)
        (#f (lookup x p))))))

(def done () #f)

(def apply (fn failure)
  (match fn
    ({Sequence atom-1 cut goal program success}
      (run-seq atom-1 goal program success failure cut))
    ({Fun } #t)))

(def apply1 (fn1)
  (match fn1
    ({Backtrack cut failure program r success}
      (run-goal r program success failure cut))
    ({Done } (done ))))

(def main ([Program pgm] [Goal goal])
  (run-goal goal pgm {Fun } {Done } {Done }))

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
