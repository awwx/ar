(use arc equal-wrt-testing)

(def test-iso (desc result expected)
  (if (equal-wrt-testing expected result)
       (do (pr "ok " desc " => ")
           (write result)
           (prn))
       (do (pr "FAIL " desc " => ")
           (write result)
           (pr ", not the expected result ")
           (write expected)
           (prn)
           (err "test failed"))))

(mac catcherr body
  `(on-err idfn (fn () ,@body)))

(def makeerr (msg)
  (catcherr (err msg)))

(mac testis (expr expected)
  `(test-iso (tostring (write ',expr)) (catcherr ,expr) ,expected))

(assign td "/tmp/SR0hwhic5P")

(def clean ()
  (system (string "rm -rf " td))
  (system (string "mkdir " td)))
