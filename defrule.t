(use test-by-example)

(example-test (runtime '(arc)) #<<.

> (def foo (x) "foo")

> (defrule foo (and (is x 2) (+ x 14))
    (list "qux" it orig (orig x)))

> (foo 1)
"foo"

> (foo 2)
("qux" 16 #<fn> "foo")

.
)
