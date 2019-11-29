def fact n -> match n
  | 0 -> 1
  | _ -> mul n (fact (sub n 1))

def fib n -> match n
  | 0 -> 1
  | 1 -> 1
  | _ -> add (fib (sub n 1)) (fib (sub n 2))