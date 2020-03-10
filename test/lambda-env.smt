(def-data expr
  {var string}
  {lam string expr}
  {app expr expr}
  {unit})

(def streq (a b)
  (case a
    (_ {false})
    (_ {true})))

(def extend (env k v)
  {extend env k v})

(def lookup (env x)
  (case env
    ({extend env k v} 
      (case (streq x k)
        ({true} v)
        ({false} (lookup env x))))
    ({empty} error)))

(def eval (env term)
  (case term
    ({var x} (lookup env x))
    ({lam x body}
      (fun (v) (eval (extend env x v) body)))
    ({app f x} ((eval env f) (eval env x)))
    ({unit} {unit})))

(def main (term)
  (eval {empty} term))