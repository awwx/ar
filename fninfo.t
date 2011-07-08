(use test-by-example)

(let r (runtime '())
  (= r!arcdir* arcdir*)
  (r!use-load 'fninfo)
  (example-test r #<<.

> (fn-args isa)
(x y)

> (fn-body isa)
((is (type x) y))

> (def foo (a b c) (+ 100 a b c))

> (foo 1 2 3)
106

> (fn-args foo)
(a b c)

> (fn-body foo)
((+ 100 a b c))

.
))
