#lang racket

(require "../lib/idl.rkt")

; begin interpreter

(def-data Term
  Integer
  {App Term Term}
  {Abs Term}
  {Unit })

(def-data Env
  {Nil }
  {Cons Any Env})

(def-data Thunk
  {Delayed Any}
  {Computed Any})

(def-struct {Heap [Integer next] contents})
(def-struct {Pair l r})
(def-struct {Payload arg env})
(def-struct {Closure body env})
(def-struct {Delayed1 cont1 location})
(def-struct {Cont cont1 heap})
(def-struct {App1 arg cont1 env})
(def-struct {Halt })
(def dereference ([Heap heap] [Integer location])
  (match heap ({Heap _ contents} (contents location))))

(def update ([Heap heap] [Integer location] value)
  (match heap
    ({Heap next contents} {Heap next (insert contents location value)})))

(def alloc ([Heap heap] value)
  (match heap
    ({Heap next contents}
      {Pair next {Heap (+ next 1) (insert contents next value)}})))

(def insert (store location value)
  (fun (loc)
    (match (eq? loc location)
      (#t value)
      (#f (store loc)))))

(def nth ([Env env] [Integer n] cont)
  (match env
    ({Nil } (error "empty env"))
    ({Cons v env}
      (match n
        (0 (continue cont v))
        (_ (nth env (- n 1) cont))))))

(def eval ([Term expr] [Env env] [Heap heap] cont1)
  (match expr
    ([Integer n] (nth env n {Cont cont1 heap}))
    ({App fn arg} (eval fn env heap {App1 arg cont1 env}))
    ({Abs body} (continue1 cont1 {Pair {Closure body env} heap}))
    ({Unit } (continue1 cont1 {Pair {Unit } heap}))))

(def force (fn1 heap cont2)
  (match fn1 ({Payload arg env} (eval arg env heap cont2))))

(def apply (fn2 loc heap cont3)
  (match fn2 ({Closure body env} (eval body {Cons loc env} heap cont3))))

(def continue1 (fn3 val2)
  (match fn3
    ({Delayed1 cont1 location}
      (let {Pair value heap} val2)
      (let heap (update heap location {Computed value}))
      (continue1 cont1 {Pair value heap}))
    ({App1 arg cont1 env}
      (let {Pair f heap} val2)
      (let {Pair loc heap} (alloc heap {Delayed {Payload arg env}}))
      (apply f loc heap cont1))
    ({Halt } val2)))

(def continue (fn4 location)
  (match fn4
    ({Cont cont1 heap}
      (match (dereference heap location)
        ({Computed value} (continue1 cont1 {Pair value heap}))
        ({Delayed thunk} (force thunk heap {Delayed1 cont1 location}))))))

(def main ([Term term])
  (let {Pair res _}
    (eval term {Nil } {Heap 0 (fun (loc) (error "empty heap"))} {Halt }))
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