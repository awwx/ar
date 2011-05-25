(load "equal-wrt-testing.arc")
(load "test.arc")
(load "embed.arc")

(def matches (pattern form)
  (iso (firstn len.pattern form) pattern))

(def ac-upto (pattern)
  (prn)
  (write pattern) (prn)
  (let arc (new-arc (racket-path->string (racket-current-directory)))
    (catch
     (each form (readfile "ac.ail")
       (arc!ar-racket-eval arc!arc* (ar-deep-fromarc form))
       (when (matches pattern form) (throw nil)))
     (err "pattern not found in source" pattern))
    arc))

(let a (ac-upto '(racket-define -))
  (testis (a!- 5)        -5)
  (testis (a!- 20 4 3 2) 11))

(mac rq (lit)
  `(racket ,(+ "(racket-quote " lit ")")))

(let a (ac-upto '(racket-define (ar-r/list-toarc x)))
  (testis (a!ar-r/list-toarc (rq "()"))        'nil)
  (testis (a!ar-r/list-toarc (rq "(1 2 3)"))   '(1 2 3))
  (testis (a!ar-r/list-toarc (rq "(1 2 . 3)")) '(1 2 . 3)))
