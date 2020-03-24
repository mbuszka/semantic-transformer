(def-data expr
  {var num}
  {lam expr}
  {app expr expr}
  {unit})

(def-data num
  {zero}
  {one num})

(def extend (env v)
  (fun (x)
    (case x
      ({zero} v)
      ({one x} (env x)))))

(def eval (env term)
  (case term
    ({var x} (env x))
    ({lam body}
     (fun (v) (eval (extend env v) body)))
    ({app f x} ((eval env f) (eval env x)))
    ({unit} {unit})))

(def init (x) panic)

(def main ([term expr])
  (eval init term))

(def-test "unit evaluates to unit"
  ({unit})
  {unit})

(def-test "simple call"
  ({app {lam {var {zero}}} {unit}})
  {unit})

(def-test "higher order call"
  ({app 
    {app
      {lam {lam 
        {app 
          {var {one {zero}}}
          {var {zero}}}}}
      {lam {var {zero}}}}
    {unit}})
  {unit})
  