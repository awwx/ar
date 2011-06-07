(use strings test-by-example)

(example-test runtime* #<<.

> (tokens "abc def ghi")
("abc" "def" "ghi")

> (halve "first second third")
("first" " second third")

> (positions digit "abc6d78defg9")
(3 5 6 11)

> (lines "one\ntwo\nthree\n")
("one" "two" "three" "")

> (slices "abc6d78defg9" digit)
("abc" "d" "" "defg" "")

> (urldecode "ab%20cd")
"ab cd"

> (urldecode (urlencode "foobar"))
"foobar"

> (litmatch "<p>" "ab<p>cd" 2)
t

> (endmatch ".arc" "foo.t")
nil

> (endmatch ".arc" "foo.arc")
t

> (posmatch "xy" "abxcyxydef")
5

> (begins "abcdef" "foo")
nil

> (begins "foobar" "foo")
t

> (subst "X" "foo" "abc foo def")
"abc X def"

> (multisubst '(("foo" "X") ("bar" "Y")) "one bar two foo three")
"one Y two X three"

> (findsubseq "foo" "abcfoodef")
3

> (blank "")
t

> (blank " abc ")
nil

> (blank "    ")
t

> (nonblank "  abc ")
"  abc "

> (nonblank "      ")
nil

> (trim "  abc def   ")
"abc def"

> (num 3000.1478)
"3,000.15"

> (pluralize 5 "apple")
"apples"

> (pluralize 1 "apple")
"apple"

> (pluralize '(x) "apple")
"apple"

> (pluralize '(x y z) "apple")
"apples"

> (plural 5 "apple")
"5 apples"

.
)
