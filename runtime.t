(use runtime pictorial-test)

(pictorial-test (this-runtime) #<<.

> (= foo 123)

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
