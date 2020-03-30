(def-data expr
  {var string}
  {lam string expr}
  {app expr expr}
  {num integer}
  {let_ string expr expr}
  {add_ expr expr})

(def extend (env k v)
  (fun (x)
    (case (eq x k)
      (#t v)
      (#f (env x)))))

(def eval (env term)
  (case term
    ({var x} (env x))
    ({lam x body}
     (fun (v) (eval (extend env x v) body)))
    ({let_ x e b}
     (eval env {app {lam x b} e}))
    ({app f x} ((eval env f) (eval env x)))
    ({add_ n m} (add (eval env n) (eval env m)))
    ({num n} n)))

(def init (x) panic)

(def main ([term expr])
  (eval init term))

(def-test "number evaluates to number"
  ({num 42})
  42)

(def-test "simple call"
  ({app {lam "x" {var "x"}} {num 42}})
  42)

(def-test "higher order call"
  ({app 
    {app
      {lam "f" {lam "x"
        {app 
          {var "f"}
          {var "x"}}}}
      {lam "x" {var "x"}}}
    {num 42}})
  42)

(def-test "let binding"
  ({let_ "id" {lam "x" {var "x"}}
      {app {var "id"} {num 42}}})
  42)

(def-test "simple Church arithmetic"
  ({let_ "zero" {lam "s" {lam "z" {var "z"}}}
   {let_ "succ" {lam "n" {lam "s" {lam "z" 
      {app {var "s"} {app {app {var "n"} {var "s"}} {var "z"}}}}}}
   {let_ "plus" {lam "n" {lam "m" {lam "s" {lam "z"
      {app {app {var "n"} {var "s"}} {app {app {var "m"} {var "s"}} {var "z"}}}}}}}
   {let_ "plus1" {lam "n" {add_ {var "n"} {num 1}}}
   {let_ "one" {app {var "succ"} {var "zero"}}
   {let_ "two" {app {app {var "plus"} {var "one"}} {var "one"}}
    {app {app {var "two"} {var "plus1"}} {num 0}}}}}}}})
  2)
  