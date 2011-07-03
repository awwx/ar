(use runtime test-by-example test path)

(example-test runtime* #<<.

> (bound 'foo)
nil

> (= foo 123)

> (bound 'foo)
t

> runtime*!foo
123

> (= runtime*!foo 456)

> foo
456

> (= rt (runtime '(fooble-example)))

> type.rt
runtime

> rt!fooble
1234

.
)

(let r (runtime '())
  (testis (bound 'foo r) nil)
  (= r!foo 123)
  (testis (bound 'foo r) t))

(w/testdir
  (let adir (path testdir "a")
    (ensure-dir adir)
    (writefile '(prn "this is foo") (path adir "foo.arc"))
    (let r (runtime '(arc))
      (r!eval `(use ,(+ adir "/")))
      (example-test r #<<.
> (use foo)
prints: this is foo\n
.
      ))))
