(use test-by-example)

(example-test (runtime '(label)) #<<.

> (deflabel foo)

> (= x (table))

> (foo x)
nil

> (= (foo x) 123)
123

> (foo x)
123

.
)
