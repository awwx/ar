(use test-by-example test)

(let in (instring "abcde")
  (testis (re-match "ab?" in) '("ab"))
  (testis (readc in) #\c))

(let in (instring "acxy")
  (testis (re-match "ab?" in) '("a"))
  (testis (readc in) #\c))

(let in (instring "bcde")
  (testis (re-match "ab?" in) nil)
  (testis (readc in) #\b))

(let in (instring "abcd")
  (testis (re-match "a(b)" in) '("ab" "b"))
  (testis (readc in) #\c))

(let in (instring "abcde")
  (testis (re-looking-at "ab?" in) t)
  (testis (readc in) #\a))

(let in (instring "acxy")
  (testis (re-looking-at "ab?" in) t)
  (testis (readc in) #\a))

(let in (instring "bcde")
  (testis (re-looking-at "ab?" in) nil)
  (testis (readc in) #\b))


(let in (instring "ABC")
  (skip-newlines in)
  (testis (readc in) #\A))

(let in (instring "   ABC")
  (skip-newlines in)
  (testis (readc in) #\space))

(let in (instring "\nABC")
  (skip-newlines in)
  (testis (readc in) #\A))

(let in (instring "    \n  \n\n  \nABC")
  (skip-newlines in)
  (testis (readc in) #\A))


(fromstring "\nabc"
  (testis (read-upto-empty-line) "")
  (testis (readline) "abc"))

(fromstring "abc"
  (testis (read-upto-empty-line) "abc\n")
  (testis (readline) nil))

(fromstring "abc\n\ndef"
  (testis (read-upto-empty-line) "abc\n")
  (testis (readline) "def"))

(fromstring "abc\ndef\n\nghi"
  (testis (read-upto-empty-line) "abc\ndef\n")
  (testis (readline) "ghi"))


(fromstring "" (testis (parse-test-result) nil))

(fromstring "(a b c\n   d e f)\n"
  (testis (parse-test-result) '((val "(a b c\n   d e f)\n"))))

(fromstring "prints: abc\n"
  (testis (parse-test-result) '((prints "abc"))))

(fromstring "err: foo\n"
  (testis (parse-test-result) '((err "foo"))))

(fromstring "stderr: abc\nprints: foo\n(123\n 456)\n"
  (testis (parse-test-result)
          '((errout "abc") (prints "foo") (val "(123\n 456)\n"))))


(fromstring #<<.
> (+ 3 4)

.
  (testis (parse-one-test-spec)
          '((expr "(+ 3 4)\n"))))

(fromstring #<<.
> (+ 3
     4)

.
  (testis (parse-one-test-spec)
          '((expr "(+ 3\n   4)\n"))))


(fromstring #<<.
> (+ '(a b c d) '(e f))
(a b c
   d e f)

.
  (testis (parse-one-test-spec)
          '((expr "(+ '(a b c d) '(e f))\n")
            (val  "(a b c\n   d e f)\n"))))

(fromstring #<<.
> (just-for-side-effect)
.
  (testis (parse-one-test-spec)
          '((expr "(just-for-side-effect)\n"))))


(fromstring "" (testis (parse-test-specs) nil))

(fromstring #<<.
> (abc)

> (def)
.
  (testis (parse-test-specs)
          '(((expr "(abc)\n")) ((expr "(def)\n")))))

(fromstring #<<.
> (prn "hi")
prints: hi\n
.
  (testis (parse-test-specs)
          '(((expr "(prn \"hi\")\n") (prints "hi\n")))))

(let spec (fromstring #<<.
> (+ 3 5)
8
.
            (car (parse-test-specs)))
  (testis (eval-test runtime* spec)
          '((val 8))))

(let spec (fromstring #<<.
> (+ 3 5)
4
.
            (car (parse-test-specs)))
  (testis (check-test-result runtime* spec
                             (eval-test runtime* spec))
          "expected val 4, actual 8"))
