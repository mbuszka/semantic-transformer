(def extend (env k v)
  (case (streq x k)
    ({true} v)
    ({false} (env x))))

(def eval (env term)
  (case term
    ({var x} (env x))
    ({lam x body}
      (fun (v) (eval (extend env x v) body)))
    ({app f x} ((eval env f) (eval env x)))
    ({unit} {unit})))

(def main (term)
  (eval (fun (x) (err {empty})) term))