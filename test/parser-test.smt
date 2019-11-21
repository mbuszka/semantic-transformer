(def foo (a b c) 42)
(def bar (void) (foo 14))
(def x (void) (Foo 1 2))
(def match (x)
  (case x
    ((Foo a b) b)
    ((Bar a) a)
    else 42))