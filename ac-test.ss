#lang scheme

(require mzlib/defmacro)

(require (only-in "ar.ss"
           arc-list hash
           write-to-string))
(require (only-in "ac.ss"
           arc-eval new-arc ac-build-steps get g))

(define (test-expect-error-impl source thunk expected-error-message)
  (let ((actual-error
         (with-handlers ((exn:fail? (lambda (c) (exn-message c))))
           (thunk)
           'oops-no-error-after-all)))
    (if (and (string? actual-error)
             (>= (string-length actual-error)
                 (string-length expected-error-message))
             (equal? (substring actual-error 0 (string-length expected-error-message))
                     expected-error-message))
         (begin (display "ok ")
                (write source)
                (display " => ")
                (write actual-error)
                (newline))
         (begin (display "bzzt! ")
                (write source)
                (display " => ")
                (write actual-error)
                (display " not ")
                (write expected-error-message)
                (newline)
                (error "test failed")))))

(define-syntax test-expect-error
  (syntax-rules ()
    ((test-expect-error expr expected-error-message)
     (test-expect-error-impl 'expr (lambda () expr) expected-error-message))))

(define (test-equal-impl source thunk expected)
 (let ((result (thunk)))
   (unless (equal? result expected)
     (error (string-append
             "bzzt! "
             (write-to-string source)
             " => "
             (write-to-string result)
             " not "
             (write-to-string expected))))))

(define-syntax test-equal
  (syntax-rules ()
    ((test-equal expr expected)
     (test-equal-impl 'expr (lambda () expr) expected))))

(define (test-impl source-string expected actual)
  (if (equal? expected actual)
       (begin (display "ok ")
              (display source-string)
              (display "=> ")
              (write actual)
              (newline))
       (begin
         (display "bzzt! ")
         (display source-string)
         (display "=> ")
         (write actual)
         (display " not ")
         (write expected)
         (newline)
         (error "test failed"))))

(define-syntax test
  (syntax-rules ()
    ((test expr expected)
     (let ((r expr))
       (test-impl 'expr expected r)))))

(define (begins s lst)
  (cond ((null? lst)
         #t)
        ((null? s)
         #f)
        ((not (equal? (car s) (car lst)))
         #f)
        (else
         (begins (cdr s) (cdr lst)))))

(test (begins '(a b c d e) '(a b c)) #t)
(test (begins '(a (b c) d e) '(a (b c))) #t)
(test (begins '(a b c d e) '(a x)) #f)
(test (begins '(a b c d e) '(x)) #f)

(define (test-t-impl arc thunk)
  ;; todo
  (unless ((g ar-true) (thunk))
    (error "not true")))

(define (test-nil-impl arc thunk)
  ;; todo
  (unless ((g ar-no) (thunk))
     (error "not nil")))

(define-syntax test-t
  (syntax-rules ()
    ((test-t arc body ...)
     (test-t-impl arc (lambda () body ...)))))

(define-syntax test-nil
  (syntax-rules ()
    ((test-nil arc body ...)
     (test-nil-impl arc (lambda () body ...)))))

(define (arc-test-eval r/arc-program arc)
  (let ((final 'nil))
    (for-each (lambda (r/form)
                (set! final (arc-eval arc ((g ar-toarc) r/form))))
              r/arc-program)
    final))

(define (writes-to-string xs)
  (let ((port (open-output-string)))
    (for-each (lambda (x)
                (write x port)
                (display " " port))
              xs)
    (close-output-port port)
    (get-output-string port)))

(define build-steps (make-parameter ac-build-steps))
(define test-inline (make-parameter #t))

(define (arc-test-impl arc source expected)
  (let ((result (arc-test-eval source arc)))
    (test-impl (writes-to-string source) expected result)))

(defmacro arc-test (source expected)
  `(let ((arc (test-arc)))
     (arc-test-impl arc ',source ,expected)))

(define (take match)
  (define (step lst acc)
    (cond ((null? lst)
           (reverse acc))
          ((let ((s (cadr (car lst))))
             (if s (match s) #f))
           (reverse (cons (car lst) acc)))
          (else
           (step (cdr lst) (cons (car lst) acc)))))
  (step ac-build-steps '()))

(define (after-impl pattern thunk)
  (parameterize ((build-steps (take (lambda (s) (begins s pattern)))))
    (thunk)))

(define (test-arc)
  (let ((options (hash)))
    (hash-set! options 'build-steps (build-steps))
    (new-arc options)))

(define-syntax after
  (syntax-rules ()
    ((after pattern body ...)
     (after-impl pattern (lambda () body ...)))))

(define (tostringf f)
  (let ((port (open-output-string)))
    (parameterize ((current-output-port port))
      (f))
    (get-output-string port)))

(define (run-ac-tests test-inline?)
  (parameterize ((test-inline test-inline?))

    (after '(ar-def ar-list-fromarc)
      (let ((arc (test-arc)))
        (test ((g ar-list-fromarc) 'nil)           '())
        (test ((g ar-list-fromarc) (arc-list 1 2)) '(1 2))
        (test ((g ar-list-fromarc) (mcons 1 2))    '(1 . 2))))

    (after '(ac-def car)
      (let ((arc (test-arc)))
        (test ((g car) 'nil)             'nil)
        (test ((g car) ((g list) 1 2 3)) 1)))

    (after '(ac-def cdr)
      (let ((arc (test-arc)))
        (test ((g cdr) 'nil)             'nil)
        (test ((g cdr) ((g list) 1 2 3)) ((g list) 2 3))))

    (after '(ar-def cadr)
      (let ((arc (test-arc)))
        (test ((g cadr) ((g list) 1 2 3)) 2)))

    (after '(ar-def cddr)
      (let ((arc (test-arc)))
        (test ((g cddr) ((g list) 1 2 3 4)) ((g list) 3 4))))

    (after '(ar-def annotate)
      (let ((arc (test-arc)))
        (let ((x ((g annotate) 'mytype 'foo)))
          (test ((g type) x) 'mytype)
          (test ((g rep) x)  'foo))))

    (after '(ar-def is)
      (let ((arc (test-arc)))
        (test ((g is))       't)
        (test ((g is) 4)     't)
        (test ((g is) 3 4)   'nil)
        (test ((g is) 4 4)   't)
        (test ((g is) 4 4 5) 'nil)
        (test ((g is) 4 4 4) 't)))

    (after '(ar-def caris)
      (let ((arc (test-arc)))
        (test ((g caris) 4 'x)                'nil)
        (test ((g caris) ((g list) 'y 'z) 'x) 'nil)
        (test ((g caris) ((g list) 'x 'y) 'x) 't)))

    (after '(ar-def map1)
      (let ((arc (test-arc)))
        (test ((g map1)
               (lambda (x)
                 ((g ar-tnil) (odd? x)))
               ((g list) 1 2 3 4))
              ((g list) 't 'nil 't 'nil))))

    (after '(ar-def coerce)
      (let ((arc (test-arc)))
        (test ((g coerce) #\A                   'int)       65)
        (test ((g coerce) #\A                   'string)    "A")
        (test ((g coerce) #\A                   'sym)       'A)
        (test ((g coerce) 123                   'num)       123)
        (test ((g coerce) 65                    'char)      #\A)
        (test ((g coerce) 123                   'string)    "123")
        (test ((g coerce) 128                   'string 16) "80")
        (test ((g coerce) 13.4                  'int)       13)
        (test ((g coerce) 65.0                  'char)      #\A)
        (test ((g coerce) 14.5                  'string)    "14.5")
        (test ((g coerce) "foo"                 'sym)       'foo)
        (test ((g coerce) "foo"                 'cons)      (arc-list #\f #\o #\o))
        (test ((g coerce) "123.5"               'num)       123.5)
        (test ((g coerce) "123"                 'int)       123)
        (test ((g coerce) (arc-list "a" 'b #\c) 'string)    "abc")
        (test ((g coerce) 'nil                  'string)    "")
        (test ((g coerce) 'nil                  'cons)      'nil)
        ))

    (after '(ar-def >)
      (let ((arc (test-arc)))
        (test ((g <))             't)
        (test ((g <) 1)           't)
        (test ((g <) 1 2)         't)
        (test ((g <) 2 1)         'nil)
        (test ((g <) 1 2 3)       't)
        (test ((g <) 1 3 2)       'nil)
        (test ((g <) 3 1 2)       'nil)
        (test ((g <) 1 2 3 4)     't)
        (test ((g <) 1 3 2 4)     'nil)
        (test ((g <) "abc" "def") 't)
        (test ((g <) "def" "abc") 'nil)
        (test ((g <) 'abc 'def)   't)
        (test ((g <) 'def 'abc)   'nil)
        (test ((g <) #\a #\b)     't)
        (test ((g <) #\b #\a)     'nil)

        (test ((g >))             't)
        (test ((g >) 1)           't)
        (test ((g >) 2 1)         't)
        (test ((g >) 1 2)         'nil)
        (test ((g >) 3 2 1)       't)
        (test ((g >) 3 1 2)       'nil)
        (test ((g >) 2 3 1)       'nil)
        (test ((g >) 4 3 2 1)     't)
        (test ((g >) 4 3 1 2)     'nil)
        (test ((g >) "def" "abc") 't)
        (test ((g >) "abc" "def") 'nil)
        (test ((g >) 'def 'abc)   't)
        (test ((g >) 'abc 'def)   'nil)
        (test ((g >) #\b #\a)     't)
        (test ((g >) #\a #\b)     'nil)
        ))

    (after '(ar-def len)
      (let ((arc (test-arc)))
        (test ((g len) "abc")            3)
        (test ((g len) (hash 'a 1 'b 2)) 2)
        (test ((g len) (arc-list))       0)
        (test ((g len) (arc-list 1))     1)
        (test ((g len) (arc-list 1 2))   2)
        (test ((g len) (arc-list 1 2 3)) 3)))

    (after '(ac-def join)
      (let ((arc (test-arc)))
        (test ((g join)) 'nil)
        (test ((g join) (arc-list)) 'nil)
        (test ((g join) 1) 1)
        (test ((g join) (arc-list 1 2)) (arc-list 1 2))
        (test ((g join) (arc-list) (arc-list)) 'nil)
        (test ((g join) (arc-list 1 2) (arc-list)) (arc-list 1 2))
        (test ((g join) (arc-list 1 2) 3) (mcons 1 (mcons 2 3)))
        (test ((g join) (arc-list) (arc-list 1 2)) (arc-list 1 2))
        (test ((g join) (arc-list 1 2) (arc-list 3 4)) (arc-list 1 2 3 4))
        (test ((g join) (arc-list 1 2) 3) (mcons 1 (mcons 2 3)))
        (test ((g join) (arc-list 1) (arc-list 2) (arc-list 3)) (arc-list 1 2 3))))

    (after '(ar-def +)
      (let ((arc (test-arc)))
        (test ((g +))                             0)
        (test ((g +) #\a "b" 'c 3)                "abc3")
        (test ((g +) "a" 'b #\c)                  "abc")
        (test ((g +) 'nil (arc-list 1 2 3))       (arc-list 1 2 3))
        (test ((g +) (arc-list 1 2) (arc-list 3)) (arc-list 1 2 3))
        (test ((g +) 1 2 3)                       6)))

    (after '(ac-def ar-apply)
      (let ((arc (test-arc)))
        (test ((g ar-apply) + 1 2 3) 6)
        (test ((g ar-apply) ((g list) 1 2 3) 1) 2)
        (test ((g ar-apply) "abcde" 2) #\c)
        (test ((g ar-apply) (hash 'a 1 'b 2) 'b) 2)
        (test ((g ar-apply) (hash 'a 1 'b 2) 'x) 'nil)
        (test ((g ar-apply) (hash 'a 1 'b 2) 'x 3) 3)
        ))

    (after '(ac-def ar-funcall4)
      (let ((arc (test-arc)))
        (test ((g ar-funcall0) +)                     0)
        (test ((g ar-funcall1) + 3)                   3)
        (test ((g ar-funcall1) "abcd" 2)              #\c)
        (test ((g ar-funcall2) + 3 4)                 7)
        (test ((g ar-funcall2) (hash 'a 1 'b 2) 'x 3) 3)
        (test ((g ar-funcall3) + 3 4 5)               12)
        (test ((g ar-funcall4) + 3 4 5 6)             18)))

    (after '(ac-def-sig ar-combine-args)
      (let ((arc (test-arc)))
        (test ((g ar-combine-args) (list))                        '())
        (test ((g ar-combine-args) (list ((g list))))             '())
        (test ((g ar-combine-args) (list ((g list) 'a)))          '(a))
        (test ((g ar-combine-args) (list ((g list) 'a 'b 'c)))    '(a b c))
        (test ((g ar-combine-args) (list 'a ((g list))))          '(a))
        (test ((g ar-combine-args) (list 'a ((g list) 'b 'c 'd))) '(a b c d))
        (test ((g ar-combine-args) (list 'a 'b ((g list) 'c 'd))) '(a b c d))))

    (after '(ar-def apply)
      (let ((arc (test-arc)))
        (test ((g apply) +) 0)
        (test ((g apply) (g join) 'nil ((g ar-toarc) '((a b) (c d))))
              ((g ar-toarc) '(a b c d)))
        (test ((g apply) + 1 2 ((g list) 3 4)) 10)))

    (after '(ac-def ac (s env))
      (test-expect-error
       (let ((arc (test-arc)))
         ((g ac) (lambda () 'foo) 'nil))
       "Bad object in expression"))

    (after '(extend ac (s env) ((g ac-literal?) s))
      (arc-test ( 123   ) 123)
      (arc-test ( #\a   ) #\a)
      (arc-test ( "abc" ) "abc"))

    (after '(ac-def ac-lex?)
      (let ((arc (test-arc)))
        (test-t arc
                ((g ac-lex?)
                 'y
                 (arc-list 'x 'y 'z)))
        (test-nil arc
                  ((g ac-lex?)
                   'w
                   (arc-list 'x 'y 'z)))))

    (after '(extend ac (s env) (ar-and ((g ar-tnil) (not ((g ar-no) s))) ((g ar-tnil) (symbol? s))))
      (let ((arc (test-arc)))
        (test-expect-error
          (arc-test-eval '( foo ) arc)
          "reference to undefined identifier: foo"))

      (let ((arc (test-arc)))
        (arc-test ( car ) (g car))
        (arc-test ( nil ) 'nil)))

    (after '(extend ac (s env) ((g ar-tnil) (mpair? s)))
      (arc-test ( (+)           ) 0)
      (arc-test ( (+ 1 2)       ) 3)
      (arc-test ( (+ 1 2 3)     ) 6)
      (arc-test ( (+ 1 2 3 4)   ) 10)
      (arc-test ( (+ 1 2 3 4 5) ) 15))

    (after '(extend ac (s env) ((g caris) s 'quote))
      (arc-test ( 'abc     ) 'abc)
      (arc-test ( '()      ) 'nil)
      (arc-test ( '(a)     ) (arc-list 'a))
      (arc-test ( '(nil)   ) (arc-list 'nil))
      (arc-test ( '(a . b) ) (mcons 'a 'b))

      (arc-test ( (apply list 1 2 '(3 4)) ) ((g ar-toarc) '(1 2 3 4)))

      (arc-test ( (apply +)            ) 0)
      (arc-test ( (apply + nil)        ) 0)
      (arc-test ( (apply + '(1))       ) 1)
      (arc-test ( (apply + '(1 2 3 4)) ) 10)
      (arc-test ( (apply + 1 2 nil)    ) 3)
      (arc-test ( (apply + 1 2 '(3 4)) ) 10))

    (after '(ac-def ac-body)
      (test
       (let ((arc (test-arc)))
         ((g ac-body)
          (arc-list 1 2 3)
          'nil))
       (arc-list 1 2 3)))

    (after '(extend ac (s env) ((g caris) s 'fn))
      (arc-test ( ((fn ()))                  ) 'nil)
      (arc-test ( ((fn () 3))                ) 3)
      (arc-test ( ((fn (a) a) 3)             ) 3)
      (arc-test ( ((fn (a b) b) 1 2)         ) 2)
      (arc-test ( ((fn (a b) (+ a b 3)) 1 2) ) 6))

    (after '(ac-def eval)
      (arc-test ( (eval 3)        ) 3)
      (arc-test ( (eval '(+ 1 2)) ) 3))

    (after '(extend ac (s env) ((g caris) s 'quasiquote))
       (arc-test ( `nil     ) 'nil)
       (arc-test ( `3       ) 3)
       (arc-test ( `a       ) 'a)
       (arc-test ( `()      ) 'nil)
       (arc-test ( `(1)     ) (arc-list 1))
       (arc-test ( `(1 . 2) ) (mcons 1 2))
       (arc-test ( `(1 2)   ) (arc-list 1 2))
       (arc-test ( `((1 2)) ) (arc-list (arc-list 1 2)))

       (arc-test ( `,(+ 1 2)         ) 3)
       (arc-test ( `(,(+ 1 2))       ) (arc-list 3))
       (arc-test ( `(1 2 ,(+ 1 2) 4) ) (arc-list 1 2 3 4))
       
       (arc-test ( (eval ``3)         ) 3)
       (arc-test ( (eval ``,,3)       ) 3)
       (arc-test ( (eval ``,,(+ 1 2)) ) 3)

       (arc-test ( `(1 ,@(list 2 3) 4)                   ) (arc-list 1 2 3 4))
       (arc-test ( (eval ``,(+ 1 ,@(list 2 3) 4))        ) 10)

       ;; Note the following gives the wrong answer in Arc3.1 because of
       ;; Racket's nested list splicing bug.

       (arc-test ( (eval (eval ``(+ 1 ,,@(list 2 3) 4))) ) 10))

    (after '(extend ac (s env) ((g caris) s 'if))
      (arc-test ( (if)           ) 'nil)
      (arc-test ( (if nil)       ) 'nil)
      (arc-test ( (if 9)         ) 9)
      (arc-test ( (if nil 1 2)   ) 2)
      (arc-test ( (if 9 1 2)     ) 1)
      (arc-test ( (if 9 1 2 3)   ) 1)
      (arc-test ( (if nil 1 2 3) ) 3))

    (after '(extend ac (s env) (ar-caris s 'assign))
      (arc-test ( (assign x 123) ) 123)

      (arc-test ( ((fn ()
                      (assign x 123)
                      x)) )
                 123)

      (arc-test ( ((fn (x)
                      (assign x 123))
                    456) )
                 123)

      (arc-test ( ((fn (x)
                      (assign x 123)
                      x)
                    456) )
                 123)

      (arc-test ( ((fn (a b)
                      (assign a 11)
                      (assign b 22)
                      (list a b))
                    1 2) )
                 (arc-list 11 22)))

    (after '(ac-def ac-macro?)
      (arc-test ( (ac-macro? 5)    ) 'nil)
      (arc-test ( (ac-macro? 'foo) ) 'nil)

      (arc-test ( (assign foo 5)
                   (ac-macro? 'foo) )
                 'nil)

      (arc-test ( (assign foo (annotate 'mac 123))
                   (ac-macro? 'foo) )
                 123))

    (after '(extend ac-call (fn args env)
              (if ((g ar-true) ((g ac-lex?) fn env))
                   'nil
                   ((g ac-macro?) fn)))
      (arc-test ( (assign foo (annotate 'mac (fn (x) x)))
                   (foo 123) )
                 123))

    (after '(ac-def ac-fn-rest)
      (arc-test ( ((fn args (car args)) 1 2)             ) 1)
      (arc-test ( (cdr ((fn args args) 1))               ) 'nil)
      (arc-test ( ((fn (a b . rest) (car rest)) 1 2 3 4) ) 3))

    (after '(ac-def bound)
      (arc-test ( (bound 'QmrQOCYWOy) )
                 'nil)

      (arc-test ( (assign foo nil)
                   (bound 'foo) )
                 't)

      (arc-test ( (assign foo 123)
                   (bound 'foo) )
                 't))

    (after '(ac-def racket-disp)
      (test-equal
       (let ((port (open-output-string))
             (arc (test-arc)))
         (hash-set! arc 'port port)
         (arc-test-eval `( (racket-disp "a" ',port) ) arc)
         (get-output-string port))
       "a")

      (test-equal
       (let ((arc (test-arc)))
         (tostringf (lambda ()
                      (arc-test-eval '( (racket-disp "abc") ) arc))))
       "abc"))

    (after '(ac-def racket-write)
      (test-equal
       (let ((arc (test-arc)))
         (tostringf (lambda ()
                      (arc-test-eval '( (racket-write "a") ) arc))))
       "\"a\""))

    (after '(ac-def table)
      (arc-test ( (table) ) (hash)))

    (after '(ac-def sref)
      (arc-test
       ( (assign a '(x y z))
         (sref a 'M 1)
         a )
       (arc-list 'x 'M 'z))

      (arc-test ( (assign a (table))
                   (sref a 55 'x)
                   a)
                 (hash 'x 55))

      (arc-test ( (table (fn (h)
                            (sref h 55 'x)
                            (sref h 66 'y))) )
                 (hash 'x 55 'y 66))

      (arc-test ( (assign a "abcd")
                   (sref a #\M 2)
                   a )
                 "abMd"))

    (after '(ac-def details)
      (arc-test
       ( (on-err details (fn () (/ 1 0))) )
       "/: division by zero"))

    (after '(ac-def parameter)
      (arc-test
       ( (type (parameter 3)) ) 'parameter))

    ))

(run-ac-tests #t)
