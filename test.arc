; todo not sure about these names

(assign test-verbose* t)

(def test-iso3 (desc result expected)
  (if (equal-wrt-testing expected result)
       (when test-verbose*
         (do (pr "ok " desc " => ")
             (write result)
             (prn)))
       (do (pr "FAIL " desc " => ")
           (write result)
           (pr ", not the expected result ")
           (write expected)
           (prn)
           (err "test failed"))))

(mac test-iso2 (expr expected)
  `(test-iso3 ',expr ,expr ,expected))

(mac test-iso args
  (if (is (len args) 2)
       `(test-iso2 ,(args 0) ,(args 1))
       `(test-iso3 ,(args 0) ,(args 1) ,(args 2))))

(mac catcherr body
  `(on-err idfn (fn () ,@body)))

(def makeerr (msg)
  (catcherr (err msg)))

; lexical marker to run code as a test instead of
; including in the runtime
(mac test-marker body
  `(do ,@body))

(mac testis (expr expected)
  `(test-iso (tostring (write ',expr)) (catcherr ,expr) ,expected))

(mac testf (input f qf expected)
  (w/uniq (gexpected gresult)
    `(with (,gexpected  ,expected
            ,gresult (catcherr (,f ,input)))
       (test-iso (tostring (write ',input) (pr " ") (write ',qf))
                 ,gresult ,gexpected))))

(mac testt (expr)
  `(if ,expr
        (do (pr "ok ")
            (write ',expr)
            (pr " is true\n"))
        (do (pr "FAIL ")
            (write ',expr)
            (pr " is nil\n")
            (err "test failed"))))

(mac testnil (expr)
  `(if (no ,expr)
        (do (pr "ok ")
            (write ',expr)
            (pr " is nil\n"))
        (do (pr "FAIL ")
            (write ',expr)
            (pr " is not nil\n")
            (err "test failed"))))
