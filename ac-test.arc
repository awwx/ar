(load "equal-wrt-testing.arc")
(load "test.arc")
(load "embed.arc")

(def matches (pattern form)
  (iso (firstn len.pattern form) pattern))

;; returns an Arc list of Racket forms

(def rreadfile (filename)
  (w/infile in filename
    (accum a
      ((afn ()
        (racket
          (racket-let ((form (racket-read in)))
            (racket-unless (racket-eof-object? form)
              (a form)
              (self)))))))))

(def ac-upto (pattern)
  (prn)
  (write pattern) (prn)
  (let arc (new-arc (racket-path->string (racket-current-directory)))
    (catch
     (each form (rreadfile "ac.ail")
       (arc!ar-racket-eval arc!arc* form)
       (when (matches pattern (ar-toarc form)) (throw nil)))
     (err "pattern not found in source" pattern))
    arc))

(mac rq (lit)
  `(racket ,(+ "(racket-quote " lit ")")))

(mac testfor (pattern . body)
  `(let a (ac-upto ',pattern)
     ,@body))

(testfor (racket-define -)
  (testis (a!- 5)        -5)
  (testis (a!- 20 4 3 2) 11))

(testfor (racket-define /)
  (testis (a!/ 24 3 2) 4))

(testfor (racket-define *)
  (testis (a!*)       1)
  (testis (a!* 5)     5)
  (testis (a!* 3 4 7) 84))

(testfor (racket-define cons)
  (testis (a!cons 1 2) '(1 . 2)))

(testfor (racket-define inside)
  (let s (outstring)
    (disp "foo" s)
    (testis (a!inside s) "foo")))

(testfor (racket-define instring)
  (let s (a!instring "foo")
    (testis (n-of 3 (readc s)) '(#\f #\o #\o))))

(testfor (racket-define nil)
  (testis a!nil nil))

(testfor (racket-define outstring)
  (let s (a!outstring)
    (disp "foo" s)
    (testis (inside s) "foo")))

(testfor (racket-define t)
  (testis a!t t))

(testfor (racket-define uniq)
  (testis (type (a!uniq)) 'sym))

(testfor (racket-define (ar-r/list-toarc x))
  (testis (a!ar-r/list-toarc (rq "()"))        'nil)
  (testis (a!ar-r/list-toarc (rq "(1 2 3)"))   '(1 2 3))
  (testis (a!ar-r/list-toarc (rq "(1 2 . 3)")) '(1 2 . 3)))

(testfor (racket-define (list . args))
  (testis (a!list 1 2 3) '(1 2 3)))

(testfor (racket-define (ar-list-fromarc x))
  (testis (racket-equal? (a!ar-list-fromarc '())
                         (rq "()"))
          (rq "#t"))
  (testis (racket-equal? (a!ar-list-fromarc '(1 2))
                         (rq "(1 2)"))
          (rq "#t"))
  (testis (racket-equal? (a!ar-list-fromarc '(1 . 2))
                         (rq "(1 . 2)"))
          (rq "#t")))

(testfor (racket-define (ar-toarc x))
  (testis (a!ar-toarc (rq "(1 2 (3 . 4) 5 () 6)"))
          '(1 2 (3 . 4) 5 nil 6)))
