(use test-by-example)

(example-test (runtime '(print-table)) #<<.

> (obj c 5 b 8 a 6)
#table((a 6) (b 8) (c 5))

> (let p (tostring (write (listtab `((,+ plus) (,- minus)))))
    (or (is p "#table((#<fn:+> plus) (#<fn:-> minus))")
        (is p "#table((#<fn:-> minus) (#<fn:+> plus))")))
t

.
)
