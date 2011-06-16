(use test-by-example)

(example-test (runtime '(re)) #<<.

> (re-match "^abc" "abcdef")
("abc")

> (re-match "^abc" "defghi")
nil

> (re-match "^a(b)c" "abcdef")
("abc" "b")

> (re-match "^abc" (instring "abcdef"))
("abc")

> (let in (instring "abcdef")
    (re-match "bcd" in)
    (readline in))
"ef"

> (let in (instring "abcdef")
    (re-match "abx" in)
    (readline in))
"abcdef"

.
)
