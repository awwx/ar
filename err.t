(use test-by-example)

(example-test (runtime '(err)) #<<.

> (err-message "foo")
"foo"

> (err-message "foo" 1 2 3)
"foo: 1 2 3"

> (err-message "foo" '("abc" "def"))
"foo: (\"abc\" \"def\")"

> (err "foo" 1 2)
err: foo: 1 2

.
)
