; these are tests of the Arc runtime that are easier to write in Arc.

(testis (w/instring s "" (readc s 'end)) 'end)

(testis (sym "foo") 'foo)

(testis (int "123") 123)
(testis (int "100" 16) 256)
