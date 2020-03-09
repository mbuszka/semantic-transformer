(def zero () {zero})

(def succ (n) {succ n})

(def add (n m)
  (case n
    ({zero} m)
    ({succ n} {succ (add n m)})))

(def main ()
  (add
    (succ (succ (succ (zero))))
    (succ (succ (succ (succ (zero)))))))