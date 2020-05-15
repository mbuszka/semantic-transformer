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

(def-data Thunk
  {Delayed Any}
  {Computed Any})

(def-struct {Heap [Integer next] contents})

(def-struct {Pair l r})

(def dereference #:atomic ([Heap heap] [Integer location])
  (match heap
    ({Heap _ contents} (contents location))))

(def update #:atomic ([Heap heap] [Integer location] value)
  (match heap
    ({Heap next contents}
     {Heap next (insert contents location value)})))

(def alloc #:atomic ([Heap heap] value)
  (match heap
    ({Heap next contents}
     {Pair next {Heap (+ next 1) (insert contents next value)}})))

(def insert #:atomic (store location value)
  (fun #:no-defun #:atomic (loc) (match (eq? loc location) (#t value) (#f (store loc)))))

(def nth ([Env env] [Integer n])
  (match env
    ({Nil} (error "empty env"))
    ({Cons v env}
     (match n
       (0 v)
       (_ (nth env (- n 1)))))))

(def eval ([Term expr] [Env env] [Heap heap])
  (match expr
    ([Integer n]
      (let location (nth env n))
      (match (dereference heap location)
        ({Computed value} {Pair value heap})
        ({Delayed thunk}
          (let {Pair value heap} (thunk heap))
          (let heap (update heap location {Computed value}))
          {Pair value heap})))
    ({App fn arg}
      (let {Pair f heap} (eval fn env heap))
      (let {Pair loc heap} 
        (alloc heap {Delayed (fun #:name Payload #:apply force (heap) (eval arg env heap))}))
      (f loc heap))
    ({Abs body}
      {Pair (fun #:name Closure (loc heap) (eval body {Cons loc env} heap)) heap})
    ({Unit} {Pair {Unit} heap})
    ))

(def main ([Term term])
  (let {Pair res _} (eval term {Nil} {Heap 0 (fun #:no-defun #:atomic (loc) (error "empty heap"))}))
  res)

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