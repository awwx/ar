(use test-by-example)

(example-test (runtime '(capture)) #<<.

> (capture-out (fn () (pr "xyz")))
((prints "xyz"))

> (capture-errout (fn () (ero "foo")))
((errout "\"foo\" \n"))

> (capture-val (fn () 123))
((val 123))

> (capture-val (fn () (err "foo")))
((err "foo"))

> (capture-val-out (fn ()
                     (pr "xyz")
                     (ero 'ignored)
                     123))
((val 123) (prints "xyz"))

> (capture-val-out-errout (fn () 123))
((val 123))

> (capture-val-out-errout (fn ()
                            (pr "xyz")
                            (ero 'foo)
                            123))
((val 123) (prints "xyz") (errout "foo \n"))

> (capture-val-out-errout (fn ()
                            (pr "abc")
                            (ero 'def)
                            (err "foo")))
((err "foo") (prints "abc") (errout "def \n"))

.
)
