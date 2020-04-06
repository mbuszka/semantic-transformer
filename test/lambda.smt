(def-data expr
  string
  integer
  {lam string expr}
  {app expr expr}
  {let_ string expr expr}
  {add_ expr expr})

(def extend (env k v)
  (fun (x)
    (case (eq x k)
      (#t v)
      (#f (env x)))))

(def eval (env term)
  (case term
    ([x string] (env x))
    ({lam x body}
     (fun (v) (eval (extend env x v) body)))
    ({let_ x e b}
     (eval env {app {lam x b} e}))
    ({app f x} ((eval env f) (eval env x)))
    ({add_ n m} (add (eval env n) (eval env m)))
    ([n integer] n)))

(def init (x) panic)

(def main ([term expr])
  (eval init term))

(def-test "number evaluates to number"
  (42)
  42)

(def-test "simple call"
  ({app {lam "x" "x"} 42})
  42)

(def-test "higher order call"
  ({app 
    {app
      {lam "f" {lam "x"
        {app "f" "x"}}}
      {lam "x" "x"}}
    42})
  42)

(def-test "let binding"
  ({let_ "id" {lam "x" "x"}
      {app "id" 42}})
  42)

(def-test "simple Church arithmetic"
  ({let_ "zero" {lam "s" {lam "z" "z"}}
   {let_ "succ" {lam "n" {lam "s" {lam "z" 
      {app "s" {app {app "n" "s"} "z"}}}}}
   {let_ "plus" {lam "n" {lam "m" {lam "s" {lam "z"
      {app {app "n" "s"} {app {app "m" "s"} "z"}}}}}}
   {let_ "plus1" {lam "n" {add_ "n" 1}}
   {let_ "one" {app "succ" "zero"}
   {let_ "two" {app {app "plus" "one"} "one"}
    {app {app "two" "plus1"} 0}}}}}}})
  2)
  