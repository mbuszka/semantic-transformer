(def-data data)

(def zero () {zero})

(def succ (n) {succ n})

(def plus (n m)
  (case n
    ({zero} m)
    ({succ n} {succ (plus n m)})))

(def main ()
  (plus
    (succ (succ (succ (zero))))
    (succ (succ (succ (succ (zero)))))))

(def-test "should work"
  ()
  {succ {succ {succ {succ {succ {succ {succ {zero}}}}}}}})