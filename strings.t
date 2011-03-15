(testis (tokens "abc def ghi") '("abc" "def" "ghi"))

(testis (halve "first second third") '("first" " second third"))

(testis (positions digit "abc6d78defg9") '(3 5 6 11))

(testis (lines "one\ntwo\nthree\n") '("one" "two" "three" ""))

(testis (slices "abc6d78defg9" digit) '("abc" "d" "" "defg" ""))

(testis (urldecode "ab%20cd") "ab cd")

(testis (urldecode (urlencode "foobar")) "foobar")

(testis (litmatch "<p>" "ab<p>cd" 2) t)

(testis (endmatch ".arc" "foo.t") nil)
(testis (endmatch ".arc" "foo.arc") t)

(testis (posmatch "xy" "abxcyxydef") 5)

(testis (begins "abcdef" "foo") nil)
(testis (begins "foobar" "foo") t)

(testis (subst "X" "foo" "abc foo def") "abc X def")

(testis (multisubst '(("foo" "X") ("bar" "Y")) "one bar two foo three")
        "one Y two X three")

(testis (findsubseq "foo" "abcfoodef") 3)

(testis (blank "") t)
(testis (blank " abc ") nil)
(testis (blank "    ") t)

(testis (nonblank "  abc ") "  abc ")
(testis (nonblank "      ") nil)

(testis (trim "  abc def   ") "abc def")

(testis (num 3000.1478) "3,000.15")

(testis (pluralize 5 "apple") "apples")
(testis (pluralize 1 "apple") "apple")
(testis (pluralize '(x) "apple") "apple")
(testis (pluralize '(x y z) "apple") "apples")

(testis (plural 5 "apple") "5 apples")
