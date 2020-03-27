(def-data void)

(def main ([op string])
  (case op
    ("add" (add 40 2))
    ("sub" (sub 44 2))
    ("mul" (mul 21 2))
    ("div" (div 84 2))
    ("and" (and #t #f))
    ("or" (or #f #t))
    ("not" (not #t))
    ("neg" (neg -42))
    ("eq" (eq "foo" "bar"))))

(def-test "add" ("add") 42)
(def-test "sub" ("sub") 42)
(def-test "mul" ("mul") 42)
(def-test "div" ("div") 42)