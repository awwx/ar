(use runtime test-by-example)

(example-test (runtime '(parsimonious-urlencode)) #<<.

> (bytehex 65)
prints: 41

> (urlsafe #\*)
nil

> (urlsafe #\a)
t

> (charutf8 #\A)
(65)

> (charutf8 #\λ)
(206 187)

> (parsimonious-urlencode "abc")
"abc"

> (parsimonious-urlencode "abc=x&def=y")
"abc%3Dx%26def%3Dy"

.
)
