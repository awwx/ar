(use capture test)

(testis (capture-out    (fn () (pr "xyz")))  '((out "xyz")))
(testis (capture-errout (fn () (ero "foo"))) '((errout "\"foo\" \n")))
(testis (capture-val    (fn () 123))         '((val 123)))
(testis (capture-val    (fn () (err "foo"))) '((err "foo")))

(testis (capture-val-out (fn ()
                           (pr "xyz")
                           (ero 'ignored)
                           123))
        '((val 123) (out "xyz")))
               
(testis (capture-val-out-errout (fn () 123))
        '((val 123)))

(testis (capture-val-out-errout (fn ()
                                  (pr "xyz")
                                  (ero 'foo)
                                  123))
        '((val 123) (out "xyz") (errout "foo \n")))

(testis (capture-val-out-errout (fn ()
                                  (pr "abc")
                                  (ero 'def)
                                  (err "foo")))
        '((err "foo") (out "abc") (errout "def \n")))
