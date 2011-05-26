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

(testfor (racket-define (car x))
  (testis (a!car 'nil)     'nil)
  (testis (a!car '(1 2 3)) 1))

(testfor (racket-define (cdr x))
  (testis (a!cdr 'nil)     'nil)
  (testis (a!cdr '(1 2 3)) '(2 3)))

(testfor (racket-define (cadr x))
  (testis (a!cadr '(1 2 3)) 2))

(testfor (racket-define (cddr x))
  (testis (a!cddr '(1 2 3 4)) '(3 4)))

(testfor (racket-define (annotate totype rep))
  (let x (a!annotate 'mytype 'foo)
    (testis (a!type x) 'mytype)
    (testis (a!rep x) 'foo)))

(testfor (racket-define (map1 f xs))
  (testis (a!map1 odd '(1 2 3 4))
          '(t nil t nil)))

(testfor (racket-define (coerce x totype . args))
  (testis (a!coerce #\A           'int)       65)
  (testis (a!coerce #\A           'string)    "A")
  (testis (a!coerce #\A           'sym)       'A)
  (testis (a!coerce 123           'num)       123)
  (testis (a!coerce 65            'char)      #\A)
  (testis (a!coerce 123           'string)    "123")
  (testis (a!coerce 128           'string 16) "80")
  (testis (a!coerce 13.4          'int)       13)
  (testis (a!coerce 65.0          'char)      #\A)
  (testis (a!coerce 14.5          'string)    "14.5")
  (testis (a!coerce "foo"         'sym)       'foo)
  (testis (a!coerce "foo"         'cons)      '(#\f #\o #\o))
  (testis (a!coerce "123.5"       'num)       123.5)
  (testis (a!coerce "123"         'int)       123)
  (testis (a!coerce '("a" b #\c)  'string)    "abc")
  (testis (a!coerce 'nil          'string)    "")
  (testis (a!coerce 'nil          'cons)      'nil))

(testfor (racket-define (is . args))
  (testis (a!is)       't)
  (testis (a!is 4)     't)
  (testis (a!is 3 4)   'nil)
  (testis (a!is 4 4)   't)
  (testis (a!is 4 4 5) 'nil)
  (testis (a!is 4 4 4) 't))

(testfor (racket-define (caris x val))
  (testis (a!caris 4 'x)      'nil)
  (testis (a!caris '(y z) 'x) 'nil)
  (testis (a!caris '(x y) 'x) 't))
