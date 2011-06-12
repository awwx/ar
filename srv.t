(use runtime test test-by-example cwd)

(w/testdir
 (w/cwd testdir

   (let r (runtime '(srv))

     (example-test r #<<.
> (sortable (obj a 99 b 3 c 25))
((a 99) (c 25) (b 3))
.
     )

     ;; maybe use a custodian to shutdown everything when the test is
     ;; finished?

     (r!eval '(thread:serve))

     ;; todo
     (sleep 1)

     (testis (tostring (system "curl -s -S http://localhost:8080/"))
             "It's alive."))))
