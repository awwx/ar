; these are tests of the Arc runtime that are easier to write in Arc.

(testis (w/instring s "" (readc s 'end)) 'end)

(testis (sym "foo") 'foo)

(testis (int "123") 123)
(testis (int "100" 16) 256)

(testis (<=) t)
(testis (<= 3) t)
(testis (<= 3 4) t)
(testis (<= 4 4) t)
(testis (<= 5 4) nil)

(testis (>=) t)
(testis (>= 3) t)
(testis (>= 3 4) nil)
(testis (>= 4 4) t)
(testis (>= 5 4) t)

(testis (whitec #\a) nil)
(testis (whitec #\space) t)
(testis (letter #\a) t)
(testis (letter #\0) nil)
(testis (digit #\a) nil)
(testis (digit #\0) t)
(testis (punc #\;) t)

(mac achtung (x) `(+ ,x 2))
(let achtung [+ _ 5]
  (testis (achtung 0) 5))
