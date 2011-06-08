(use test-by-example)

(example-test (runtime '(arc)) #<<.

> (def foo (x) "foo")

> (foo 1)
"foo"

> (foo 2)
"foo"

> (foo 5)
"foo"

> (extend foo (x) (is x 5)
    (list "bar" it orig (orig x)))

> (foo 1)
"foo"

> (foo 2)
"foo"

> (foo 5)
("bar" t #<fn> "foo")

.
)
