(use extend-ontype pictorial-test)

(pictorial-test (this-runtime) #<<.

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
