#lang racket

#|
Monadic evaluator for programming language with coalgebraic effects
and cohandlers.

-- What are algebraic effects?

They consist of an algebraic theory with operations of the form

[[ op ]]   : P x |M|^A -> |M|

where given a parameter and continuation, we return new computation.
They are a way to model computational effects.

-- What are coalgebraic effects?

They consist of a coalgebraic theory with state-passing cooperations of the form

[[ coop ]] : P x |W| -> |W| × A

where given a parameter and current coalgebra (configuration), we obtain new
state of the coalgebra (new config) and a value generated by coalgebra.
They *may* be a way to model interaction with external resources.

In our implementaiton cohandlers are one-shot and tail recursive, which means
that result to which they evaluate can simply be passed to continuation. This
is an easy-to-code way of ensuring that usage of potential resources (and hence
continuations) is linear.

For more details about coeffects, see:
(1) Runners in action ~ D. Ahman, A. Bauer https://arxiv.org/abs/1910.11629
(2) https://github.com/Tomatosoup97/freak
|#

(require "../lib/idl.rkt")
(require
  (for-syntax syntax/parse
              racket/syntax))

; begin interpreter

(def-data Term
  String
  Integer
  {Unit }
  {Pair Term Term}
  {Fst Term}
  {Snd Term}
  {Lam String Term}
  {App Term Term}
  {Add Term Term}
  {Coeffect [String label] [Term param]}
  {Cohandle [Term body] Cohandler})

(def-struct {Cohandler [String AlgT]
                       [String label]
                       [String param]
                       [String conf]
                       [Term body]})
(def-struct {StateM state value})
(def-struct {ReturnCurryInner v})
(def-struct {Closure body env x})
(def-struct {CohandlingClosure algT body confVar env pVar})
(def-struct {ComposeInner f g})
(def-struct {Halt })
(def-struct {Fst1 cont1})
(def-struct {Snd1 cont1})
(def-struct {Pair2 cont1 lV})
(def-struct {Pair1 cont1 env r})
(def-struct {App2 cont1 fnV})
(def-struct {App1 arg cont1 env})
(def-struct {Add2 cont1 nV})
(def-struct {Add1 cont1 env m})
(def-struct {Coeffect1 cont1 env label})
(def-struct {Cohandle1 body cohandlerTerm cont1 env state})
(def-struct {Cohandler1 algT cont4 state})
(def-struct {Extend env k v})
(def-struct {Halt1 })
(def-struct {Fst2 })
(def-struct {Initial-env })
(def-struct {Returncurry })
(def-struct {Snd2 })
(def fst ([Term pair]) (match pair ({Pair l _} l)))

(def snd ([Term pair]) (match pair ({Pair _ r} r)))

(def compose (f g) {ComposeInner f g})

(def return (v state) {StateM state v})

(def returnCurry (v) {ReturnCurryInner v})

(def bind ([StateM m] f)
  (match m ({StateM state v} (apply3 (apply2 f v) state {Halt }))))

(def init-state ([String var]) 0)

(def update-state (state tgt val)
  (fun (var)
    (match (eq? tgt var)
      (#t val)
      (#f (state var)))))

(def initial-state (x) (fun (var) (init-state var)))

(def evalState (m) (match m ({StateM _ x} x)))

(def eval (env [Term term] state cont1)
  (match term
    ([String x] (continue1 cont1 (return (lookup env x) state)))
    ([Integer n] (continue1 cont1 (return n state)))
    ({Unit } (continue1 cont1 (return {Unit } state)))
    ({Lam x body}
      (let closure {Closure body env x})
      (continue1 cont1 (return closure state)))
    ({Fst p} (eval env p state {Fst1 cont1}))
    ({Snd p} (eval env p state {Snd1 cont1}))
    ({Pair l r} (eval env l state {Pair1 cont1 env r}))
    ({App fn arg} (eval env fn state {App1 arg cont1 env}))
    ({Add n m} (eval env n state {Add1 cont1 env m}))
    ({Coeffect label p} (eval env p state {Coeffect1 cont1 env label}))
    ({Cohandle body cohandlerTerm}
      (evalCohandler
        env
        state
        cohandlerTerm
        {Cohandle1 body cohandlerTerm cont1 env state}))))

(def evalCohandler (env state [Cohandler cohandler] cont3)
  (continue10
    cont3
    (match cohandler
      ({Cohandler algT effLabel pVar confVar body}
        {CohandlingClosure algT body confVar env pVar}))))

(def findCohandler (env label) (lookup env label))

(def getLabel ([Cohandler cohandler])
  (match cohandler ({Cohandler _ label _ _ _} label)))

(def initial-env (x) (error "Empty env!"))

(def extend (env k v) {Extend env k v})

(def apply3 (fn1 state cont)
  (match fn1 ({ReturnCurryInner v} (continue cont {StateM state v}))))

(def apply4 (fn2 pVal state cont2)
  (match fn2
    ({Closure body env x} (eval (extend env x pVal) body state cont2))
    ({CohandlingClosure algT body confVar env pVar}
      (let envParam (extend env pVar pVal))
      (let envParamConf (extend envParam confVar (state algT)))
      (eval envParamConf body state {Cohandler1 algT cont2 state}))))

(def apply2 (fn3 v) (match fn3 ({ComposeInner f g} (apply1 f (apply g v)))))

(def continue (fn4 x1) (match fn4 ({Halt } x1)))

(def continue1 (fn5 val11)
  (match fn5
    ({Fst1 cont1}
      (continue1 cont1 (bind val11 (compose {Returncurry } {Fst2 }))))
    ({Snd1 cont1}
      (continue1 cont1 (bind val11 (compose {Returncurry } {Snd2 }))))
    ({Pair2 cont1 lV}
      (let {StateM state rV} val11)
      (continue1 cont1 (return {Pair lV rV} state)))
    ({Pair1 cont1 env r}
      (let {StateM state lV} val11)
      (eval env r state {Pair2 cont1 lV}))
    ({App2 cont1 fnV}
      (let {StateM state argV} val11)
      (apply4 fnV argV state cont1))
    ({App1 arg cont1 env}
      (let {StateM state fnV} val11)
      (eval env arg state {App2 cont1 fnV}))
    ({Add2 cont1 nV}
      (let {StateM state mV} val11)
      (continue1 cont1 (return (+ nV mV) state)))
    ({Add1 cont1 env m}
      (let {StateM state nV} val11)
      (eval env m state {Add2 cont1 nV}))
    ({Coeffect1 cont1 env label}
      (let {StateM state pV} val11)
      (let cohandler (findCohandler env label))
      (apply4 cohandler pV state cont1))
    ({Cohandler1 algT cont4 state}
      (let {StateM newState coresult} val11)
      (let {Pair nextConf result} coresult)
      (let newState (update-state state algT nextConf))
      (continue1 cont4 (return result newState)))
    ({Halt1 } val11)))

(def continue10 (fn6 cohandlerF)
  (match fn6
    ({Cohandle1 body cohandlerTerm cont1 env state}
      (let label (getLabel cohandlerTerm))
      (let env (extend env label cohandlerF))
      (eval env body state cont1))))

(def lookup (fn7 x)
  (match fn7
    ({Extend env k v}
      (match (eq? x k)
        (#t v)
        (#f (lookup env x))))
    ({Initial-env } (initial-env x))))

(def apply (fn8 pair)
  (match fn8
    ({Fst2 } (fst pair))
    ({Snd2 } (snd pair))))

(def apply1 (fn9 v) (match fn9 ({Returncurry } (returnCurry v))))

(def main ([Term term])
  (evalState (eval {Initial-env } term (initial-state 0) {Halt1 })))

; end interpreter

(module+ test
  ;; --- Tests for pure part of calculus ---

  (def asMetaPair ([Pair mp])
    (let {Pair l v} mp) (cons l v)
  )

  (def letE (var v cont) (App (Lam var cont) v))

  (require rackunit)
  (check-equal? (main 42) 42)
  (check-equal? (main {Unit}) {Unit})
  (check-equal? (main (Add 1 2)) 3)
  (check-equal? (main (App (Lam "x" "x") 42)) 42)
  (check-equal? (asMetaPair (main (Pair 17 42))) (cons 17 42))
  (check-equal? (asMetaPair (main (Pair (Add 1 2) (Add 17 25)))) (cons 3 42))
  (check-equal? (main (Fst (Pair 17 42))) 17)
  (check-equal? (main (Snd (Pair 17 42))) 42)

  ;; ensuring that my let-sugar works as expected
  (check-equal? (main (letE "x" 1 (letE "x" 2 "x"))) 2)
  (check-equal? (main (letE "x" 1 (letE "y" 2 (Add "x" "y")))) 3)

  (define-syntax (lam* stx)
    (syntax-parse stx
      [(_ (v) body) #'(Lam v body)]
      [(_ (v vs ...+) body) #'(Lam v (lam* (vs ...) body))]))

  (define-syntax (app* stx)
    (syntax-parse stx
      [(_ f v) #'(App f v)]
      [(_ f v vs ...+) #'(app* (App f v) vs ...)]))

  (let*
    ([zero (lam* ("s" "z") "z")]
     [succ (lam* ("n" "s" "z") {App "s" (app* "n" "s" "z")})]
     [two {App succ {App succ zero}}]
     [plus (lam*  ("n" "m" "s" "z") (app* "n" "s" (app* "m" "s" "z")))]
     [pgm (app* plus two two {Lam "n" {Add "n" 1}} 0)])
    (check-equal? (main pgm) 4))

  ;; --- Tests for simple cohandler performing addition on param ---
  (let* (
    ;; Identity operation on configuration and +1 on param
    [algTheory "Monoid"]
    [cohandler (Cohandler algTheory "Increment" "p" "c" (Pair "c" (Add "p" 1)))]
    [cohandle (lambda (t) (Cohandle t cohandler))]
    [incEff (lambda (p) (Coeffect "Increment" p))]

    ;; Effect overriding the one above, identity op on conf and +3 on param
    [cohandlerInner (Cohandler algTheory "Increment" "p" "c" (Pair "c" (Add "p" 3)))]
    [cohandleInner (lambda (t) (Cohandle t cohandlerInner))]
   )
    (check-equal? (main (cohandle 42)) 42)

    (check-equal? (main (cohandle (incEff 0))) 1)
    (check-equal? (main (cohandle (incEff (incEff 0)))) 2)

    ;; Innermost cohandler should be evaluated
    (check-equal? (main (cohandle (cohandleInner (incEff 0)))) 3)
  )

  ;; --- State monad implementation in source language ---

  (let* (
    [algTheory "State"]
    [getCoh (Cohandler algTheory "Get" "_" "c" (Pair "c" "c"))]
    [setCoh (Cohandler algTheory "Set" "nextConf" "_" (Pair "nextConf" {Unit}))]

    [get (lambda (p) (Coeffect "Get" p))]
    [set (lambda (p) (Coeffect "Set" p))]

    [withState (lambda (t) (Cohandle (Cohandle t setCoh) getCoh))]
   )
    (check-equal? (main (withState (set 42))) {Unit})

    (check-equal? (main
      (withState
      (letE "_" (set 42)
      (get {Unit})))
    ) 42)

    (check-equal? (main
      (withState
      (letE "_" (set 1)
      (letE "_" (set 5)
      (get {Unit})))
    )) 5)

    (check-equal? (main
      ;; with a bit of syntactic sugar this language even looks readable!
      (withState
      (letE "_" (set 1)
      (letE "x" (get {Unit})
      (letE "y" (Add "x" 2)
      (letE "_" (set "y")
      (get {Unit})))))
    )) 3)
  )
)