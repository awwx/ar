(use test-by-example)

(example-test (runtime '(ret)) #<<.

> (ret x 33
    (++ x))
34

.
)
