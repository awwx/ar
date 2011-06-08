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

(mac w/testdir body
  `(let testdir (+ "/tmp/" (uniq))
     (system (+ "rm -rf " testdir))
     (system (+ "mkdir " testdir))
     ,@body))

(mac w/foofile body
  `(w/testdir (let foofile (+ testdir "/foo") ,@body)))
