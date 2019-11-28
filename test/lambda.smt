def extend env k v ->
  fun x -> match streq x k
    | True -> v
    | False -> env x

def eval env term ->
  match term
  | Var x -> env x
  | Lam x body ->
    fun v -> eval (extend env x v) body
  | App f x -> (eval env f) (eval env x)