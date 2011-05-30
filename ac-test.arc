(load "equal-wrt-testing.arc")
(load "test.arc")
(load "embed.arc")

(def matches (pattern form)
  ;; todo the cadr is cheating; what I really want is a recursive
  ;; match
  (iso (firstn len.pattern cadr.form) pattern))

(def ac-upto (pattern)
  (prn)
  (write pattern) (prn)
  (let arc (new-arc (racket-path->string (racket-current-directory)))
    (arc!ar-ail-load arc!arc* "ar.ail")
    (arc!ar-load "ac.arc")
    (catch
      (each form (readfile "ac.arc")
        (arc!eval form)
        (when (matches pattern form) (throw nil)))
      (err "pattern not found in source" pattern))
    arc))

(mac testfor (pattern . body)
  `(let a (ac-upto ',pattern)
     ,@body))

(testfor (ar-def ac-literal? (x))
  (testis (a!eval 123)   123)
  (testis (a!eval #\a)   #\a)
  (testis (a!eval "abc") "abc")
  (testis (a!eval car)   car))

(testfor (ar-def ac-lex? (v env))
  (testis (a!ac-lex? 'y '(x y z)) t)
  (testis (a!ac-lex? 'w '(x y z)) nil))

(testfor (ar-extend ac (s env) (ar-tnil (racket-symbol? s)))
  (testis (a!eval 'foo)
          (makeerr "reference to undefined identifier: foo"))
  (testis (a!eval 'car) a!car)
  (testis (a!eval 't)   t))

(testfor (ar-extend ac (s env)
           (ar-tnil
            (racket-and
             (racket-mpair? s)
             (racket-not (ar-true (is (car s) (racket-quote ail-code)))))))
  (testis (a!eval '(+))           0)
  (testis (a!eval '(+ 1 2))       3)
  (testis (a!eval '(+ 1 2 3))     6)
  (testis (a!eval '(+ 1 2 3 4))   10)
  (testis (a!eval '(+ 1 2 3 4 5)) 15))

(testfor (ar-extend ac (s env) (caris s (racket-quote quote)))
  (testis (a!eval ''abc)     'abc)
  (testis (a!eval ''())      'nil)
  (testis (a!eval ''(a))     '(a))
  (testis (a!eval ''(nil))   '(nil))
  (testis (a!eval ''(a . b)) '(a . b))

  (testis (a!eval '(apply list 1 2 '(3 4))) '(1 2 3 4))

  (testis (a!eval '(apply +))            0)
  (testis (a!eval '(apply + nil))        0)
  (testis (a!eval '(apply + '(1)))       1)
  (testis (a!eval '(apply + '(1 2 3 4))) 10)
  (testis (a!eval '(apply + 1 2 nil))    3)
  (testis (a!eval '(apply + 1 2 '(3 4))) 10))

(testfor (ar-def ac-body (body env))
  (testis (a!ac-body '(1 2 3) nil) '(1 2 3)))

(testfor (ar-def ac-dotted-list? (x))
  (testis (a!ac-dotted-list? '()) nil))

(testfor (ar-extend ac (s env) (caris s (racket-quote fn)))
  (testis (a!eval '((fn ())))                  nil)
  (testis (a!eval '((fn () 3)))                3)
  (testis (a!eval '((fn (a) a) 3))             3)
  (testis (a!eval '((fn (a b) b) 1 2))         2)
  (testis (a!eval '((fn (a b) (+ a b 3)) 1 2)) 6))

(testfor (ar-extend ac (s env) (caris s (racket-quote quasiquote)))
  (testis (a!eval '`nil) nil)
  (testis (a!eval '`3) 3)
  (testis (a!eval '`a) 'a)
  (testis (a!eval '`()) nil)
  (testis (a!eval '`(1)) '(1))
  (testis (a!eval '`(1 . 2)) '(1 . 2))
  (testis (a!eval '`(1 2)) '(1 2))
  (testis (a!eval '`((1 2))) '((1 2)))

  (testis (a!eval '`,(+ 1 2)) 3)
  (testis (a!eval '`(,(+ 1 2))) (list 3))
  (testis (a!eval '`(1 2 ,(+ 1 2) 4)) '(1 2 3 4))
       
  (testis (a!eval '(eval ``3)) 3)
  (testis (a!eval '(eval ``,,3)) 3)
  (testis (a!eval '(eval ``,,(+ 1 2))) 3)

  (testis (a!eval '`(1 ,@(list 2 3) 4)) '(1 2 3 4))
  (testis (a!eval '(eval ``,(+ 1 ,@(list 2 3) 4))) 10)

  ;; Note the following gives the wrong answer in Arc3.1 because of
  ;; Racket's nested list splicing bug.

  (testis (a!eval '(eval (eval ``(+ 1 ,,@(list 2 3) 4)))) 10))

(testfor (ar-extend ac (s env) (caris s (racket-quote if)))
  (testis (a!eval '(if))           nil)
  (testis (a!eval '(if nil))       nil)
  (testis (a!eval '(if 9))         9)
  (testis (a!eval '(if nil 1 2))   2)
  (testis (a!eval '(if 9 1 2))     1)
  (testis (a!eval '(if 9 1 2 3))   1)
  (testis (a!eval '(if nil 1 2 3)) 3))

(testfor (ar-extend ac (s env) (caris s (racket-quote assign)))
  (testis (a!eval '(assign x 123)) 123)

  (testis (a!eval '((fn ()
                      (assign x 123)
                      x)))
          123)

  (testis (a!eval '((fn (x)
                      (assign x 123))
                    456))
          123)

  (testis (a!eval '((fn (x)
                      (assign x 123)
                      x)
                    456))
          123)

  (testis (a!eval '((fn (a b)
                      (assign a 11)
                      (assign b 22)
                      (list a b))
                    1 2))
          '(11 22)))

(testfor (ar-def ac-macro?)
  (testis (a!eval '(ac-macro? 5))                   nil)
  (testis (a!eval '(ac-macro? 'foo))                nil)
  (testis (a!eval '(ac-macro? (annotate 'mac 123))) 123)

  (testis (a!eval '((fn ()
                      (assign foo 5)
                      (ac-macro? 'foo))))
          nil)

  (testis (a!eval '((fn ()
                      (assign foo (annotate 'mac 123))
                      (ac-macro? 'foo))))
          123))

(testfor (ar-extend ac-call (fn args env)
           (racket-if (ar-true (ac-lex? fn env)) nil (ac-macro? fn)))
  (a!eval '(assign foo (annotate 'mac (fn (x) x))))
  (testis (a!eval '(foo 123)) 123))

(testfor (ar-def ac-fn-rest (args body env))
  (testis (a!eval '((fn args (car args)) 1 2))             1)
  (testis (a!eval '(cdr ((fn args args) 1)))               nil)
  (testis (a!eval '((fn (a b . rest) (car rest)) 1 2 3 4)) 3))

(testfor (racket-define bound)
  (testis (a!eval '(bound 'QmrQOCYWOy)) nil)

  (a!eval '(assign foo nil))
  (testis (a!eval '(bound 'foo)) t)

  (a!eval '(assign foo 123))
  (testis (a!eval '(bound 'foo)) t))

(testfor (racket-define (ar-disp x (port (racket-current-output-port))))
  (let port (outstring)
    (a!eval `(ar-disp "a" ',port))
    (testis (inside port) "a"))
  (testis (tostring (a!ar-disp "abc")) "abc"))

(testfor (racket-define (ar-write x (port (racket-current-output-port))))
  (testis (tostring (a!eval '(ar-write "a"))) "\"a\""))

(testfor (racket-define (table (init nil)))
  (testis (a!eval '(table)) (table))
  (testis (a!eval `(table ,(fn (k) (= k!a 3)))) (obj a 3)))

(testfor (ar-def sref (com val ind))
  (a!eval '(assign a '(x y z)))
  (testis (a!eval '(sref a 'M 1)) 'M)
  (testis a!a '(x M z))

  (a!eval '(assign a (table)))
  (a!eval '(sref a 55 'x))
  (testis a!a (obj x 55))

  (testis (a!eval '(table (fn (h)
                            (sref h 55 'x)
                            (sref h 66 'y))))
          (obj x 55 y 66))

  (a!eval '(assign a "abcd"))
  (a!eval '(sref a #\M 2))
  (testis a!a "abMd"))
