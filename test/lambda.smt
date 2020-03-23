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
  (fun (x)
    (case (streq x k)
      ({true} v)
      ({false} (env x)))))

(def eval (env term)
  (case term
    ({var x} (env x))
    ({lam x body}
     (fun (v) (eval (extend env x v) body)))
    ({app f x} ((eval env f) (eval env x)))
    ({unit} {unit})))

(def init (x) panic)

(def main ([term expr])
  (eval init term))

(def-test "unit evaluates to unit"
  ({unit})
  {unit})