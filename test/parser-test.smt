(def-data void)

(def foo (a b c)
  (case a
    ({foo z x} (b z x))
    ({bar a b}
      (let c (a b)
        {baz c}))
    ({qux} c)))

(def bar (some very long parameter list of javaesque provenance aaaaaaaaaaaaaaaaaaaaaaaaaa)
  error)

(def bax ()
  (case (some very complicated expression blahaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)
    (_ {true})))

(def qux ()
  (fun () (case a (a a) (b b))))