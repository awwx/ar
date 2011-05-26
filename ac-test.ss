#lang scheme

(require mzlib/defmacro)

(require (only-in "ac.ss"
           arc-eval new-arc ac-build-steps get set g))

;; todo code duplication with arc

(define arcdir*
  (path->string
   (let-values (((base _2 _3)
                 (split-path (normalize-path
                              (find-system-path 'run-file)))))
     base)))

(define (pair xs)
  (cond ((null? xs)
         '())
        (else
         (cons (list (car xs) (cadr xs))
               (pair (cddr xs))))))

(define (add-to-hash h args)
  (map (lambda (p)
         (hash-set! h (car p) (cadr p)))
       (pair args)))

(define (hash . args)
  (let ((h (make-hash)))
    (add-to-hash h args)
    h))

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

(define (write-to-string x)
  (let ((port (open-output-string)))
    (write x port)
    (close-output-port port)
    (get-output-string port)))

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
           (error "source not found"))
          ((let ((s (cadr (car lst))))
             (if s (match s) #f))
           (reverse (cons (car lst) acc)))
          (else
           (step (cdr lst) (cons (car lst) acc)))))
  (step ac-build-steps '()))

(define (after-impl pattern thunk)
  (newline)
  (write pattern) (newline)
  (parameterize ((build-steps (take (lambda (s) (begins s pattern)))))
    (thunk)))

(define (test-arc)
  (let ((options (make-hash)))
    (hash-set! options 'build-steps (build-steps))
    (new-arc arcdir* options)))

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

    (after '(ar-def +)
      (let ((arc (test-arc)))
        (test ((g +))                             0)
        (test ((g +) #\a "b" 'c 3)                "abc3")
        (test ((g +) "a" 'b #\c)                  "abc")
        (test ((g +) 'nil ((g list) 1 2 3))       ((g list) 1 2 3))
        (test ((g +) ((g list) 1 2) ((g list) 3)) ((g list) 1 2 3))
        (test ((g +) 1 2 3)                       6)))

    (after '(ar-def ar-apply)
      (let ((arc (test-arc)))
        (test ((g ar-apply) + 1 2 3) 6)
        (test ((g ar-apply) ((g list) 1 2 3) 1) 2)
        (test ((g ar-apply) "abcde" 2) #\c)
        (test ((g ar-apply) (hash 'a 1 'b 2) 'b) 2)
        (test ((g ar-apply) (hash 'a 1 'b 2) 'x) 'nil)
        (test ((g ar-apply) (hash 'a 1 'b 2) 'x 3) 3)
        ))

    (after '(ar-def ar-funcall4)
      (let ((arc (test-arc)))
        (test ((g ar-funcall0) +)                     0)
        (test ((g ar-funcall1) + 3)                   3)
        (test ((g ar-funcall1) "abcd" 2)              #\c)
        (test ((g ar-funcall2) + 3 4)                 7)
        (test ((g ar-funcall2) (hash 'a 1 'b 2) 'x 3) 3)
        (test ((g ar-funcall3) + 3 4 5)               12)
        (test ((g ar-funcall4) + 3 4 5 6)             18)))

    (after '(ar-def ar-combine-args)
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

    (after '(ar-def ac)
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
                 ((g list) 'x 'y 'z)))
        (test-nil arc
                  ((g ac-lex?)
                   'w
                   ((g list) 'x 'y 'z)))))

    (after '(extend ac (s env) ((g ar-tnil) (symbol? s)))
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

    (after '(extend ac (s env) ((g caris) s (quote quote)))
      (arc-test ( 'abc     ) 'abc)
      (arc-test ( '()      ) 'nil)
      (arc-test ( '(a)     ) ((g list) 'a))
      (arc-test ( '(nil)   ) ((g list) 'nil))
      (arc-test ( '(a . b) ) (mcons 'a 'b))

      (arc-test ( (apply list 1 2 '(3 4)) ) ((g ar-toarc) '(1 2 3 4)))

      (arc-test ( (apply +)            ) 0)
      (arc-test ( (apply + nil)        ) 0)
      (arc-test ( (apply + '(1))       ) 1)
      (arc-test ( (apply + '(1 2 3 4)) ) 10)
      (arc-test ( (apply + 1 2 nil)    ) 3)
      (arc-test ( (apply + 1 2 '(3 4)) ) 10))

    (after '(ac-def ac-body)
      (let ((arc (test-arc)))
        (test
         ((g ac-body)
          ((g list) 1 2 3)
          'nil)
         ((g list) 1 2 3))))

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
       (arc-test ( `(1)     ) ((g list) 1))
       (arc-test ( `(1 . 2) ) (mcons 1 2))
       (arc-test ( `(1 2)   ) ((g list) 1 2))
       (arc-test ( `((1 2)) ) ((g list) ((g list) 1 2)))

       (arc-test ( `,(+ 1 2)         ) 3)
       (arc-test ( `(,(+ 1 2))       ) ((g list) 3))
       (arc-test ( `(1 2 ,(+ 1 2) 4) ) ((g list) 1 2 3 4))
       
       (arc-test ( (eval ``3)         ) 3)
       (arc-test ( (eval ``,,3)       ) 3)
       (arc-test ( (eval ``,,(+ 1 2)) ) 3)

       (arc-test ( `(1 ,@(list 2 3) 4)                   ) ((g list) 1 2 3 4))
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

    (after '(extend ac (s env) ((g caris) s (quote assign)))
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
                 ((g list) 11 22)))

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
       ((g list) 'x 'M 'z))

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
