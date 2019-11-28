def fact n ->
  match n
  | 0 -> 1
  | _ -> mul n (fact (sub n 1))