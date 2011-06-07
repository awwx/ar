(use test-by-example)

(example-test (runtime '(defcall)) #<<.

> (defcall char (c n)
    (string (n-of n c)))

> (#\A 6)
"AAAAAA"

.
)
