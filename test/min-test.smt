def foo a b c -> 42

def bar void ->
  foo 1 2 3

def qux a ->
  match a
  | 5 -> 42
  | 7 -> 39
  | _ -> 0