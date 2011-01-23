; these are tests of the Arc runtime that are easier to write in Arc.

(testis (w/instring s "" (readc s 'end)) 'end)
