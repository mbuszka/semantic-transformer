(fun (a b c)
  (case a
    ({foo z x} (b z x))
    ({bar a b}
      (let c (a b)
        {baz c}))
    ({qux} c)))