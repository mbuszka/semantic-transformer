(def-data expr
  {foo string}
  {bar integer}
  {baz expr})

(def foo (a b c)
  (case a
    ({foo "abcdef"} (add 40 2))
    ({bar 7} {baz {bar (sub 44 2)}})
    ({foo "add"} (add b c))
    ({bar n} (let x 7 (mul n x)))
    ({baz add} (foo add 7 4))))

(def bar (some very long parameter list of javaesque provenance aaaaaaaaaaaaaaaaaaaaaaaaaa)
  error)

(def bax ()
  (case (some very complicated expression blahaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)
    (_ {true})))

(def qux ()
  (fun () (case {foo "abc"} (a a) (b b))))

(def main () panic)