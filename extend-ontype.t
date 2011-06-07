(use test-by-example)

(example-test (runtime '(extend-ontype)) #<<.

> (def rock (n)
    (annotate 'rock n))

> (= x (rock 3))

> (type x)
rock

> (rep x)
3

> (extend-ontype * rock (x)
    (string (n-of (rep x) "rock")))

> (* x)
"rockrockrock"
.
)
