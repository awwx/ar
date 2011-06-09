(use test-by-example)

(example-test (runtime '(values)) #<<.

> (values (q r) (racket-quotient/remainder 14 3)
    (list q r))
(4 2)

.
)
