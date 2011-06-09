(use test-by-example)

(example-test (runtime '(system-code err)) #<<.

> (system-code "/bin/true")
0

> (system-code "/bin/false")
1

> (check-system "/bin/true")
t

> (check-system "/bin/false")
err: failed: 1 ("/bin/false")

.
)
