#lang scheme

(require (only-in "ar.ss"
           arc-car arc-list deep-fromarc hash no? toarc true? write-to-string))
(require (only-in "ac.ss"
           new-ac ac-build-steps g))

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

(define (test-t-impl thunk)
  ;; todo
  (unless (true? (thunk))
    (error "not true")))

(define (test-nil-impl thunk)
  ;; todo
  (unless (no? (thunk))
     (error "not nil")))

(define-syntax test-t 
  (syntax-rules ()
    ((test-t body ...)
     (test-t-impl (lambda () body ...)))))

(define-syntax test-nil
  (syntax-rules ()
    ((test-nil body ...)
     (test-nil-impl (lambda () body ...)))))

(define (arc-test-eval r/arc-program globals)
  (let ((final 'nil))
    (for-each (lambda (r/source)
                (let ((a/source (toarc r/source)))
                  (let ((a/compiled ((hash-ref globals 'ac) a/source 'nil)))
                    (let ((r/compiled (deep-fromarc a/compiled)))
                      (let ((result (eval r/compiled (hash-ref globals 'racket-namespace*))))
                        (set! final result))))))
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

(define (arc-test-impl source expected)
  (let ((globals* (new-ac (build-steps))))
    (let ((result (arc-test-eval source globals*)))
      (test-impl (writes-to-string source) expected result))))

(define-syntax arc-test
  (syntax-rules ()
    ((arc-test (source expected) ...)
     (begin (arc-test-impl 'source expected)
            ...))))

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

    (after '(ac-def ac (s env))
      (test-expect-error
       (let ((globals* (new-ac (build-steps))))
         ((g ac) (lambda () 'foo) 'nil))
       "Bad object in expression"))

    (after '(extend ac (s env) ((g ac-literal?) s))
      (arc-test
       (( 123   ) 123)
       (( #\a   ) #\a)
       (( "abc" ) "abc")))

    (after '(extend ac (s env) (tnil (eq? s 'nil)))
      (arc-test
       (( nil ) 'nil)))

    (after '(ac-def ac-lex?)
      (test-t (let ((globals* (new-ac (build-steps))))
                ((g ac-lex?)
                 'y
                 (arc-list 'x 'y 'z))))
      (test-nil (let ((globals* (new-ac (build-steps))))
                  ((g ac-lex?)
                   'w
                   (arc-list 'x 'y 'z)))))

    (after '(extend ac (s env) (ar-and (tnil (not (no? s))) (tnil (symbol? s))))
      (test-expect-error
       (let ((globals* (new-ac (build-steps))))
         (arc-test-eval '( foo ) globals*))
       "undefined global variable: foo")

      (arc-test
       (( car ) arc-car)))

    (after '(extend ac (s env) (tnil (mpair? s)))
      (arc-test
       (( (+)           ) 0)
       (( (+ 1 2)       ) 3)
       (( (+ 1 2 3)     ) 6)
       (( (+ 1 2 3 4)   ) 10)
       (( (+ 1 2 3 4 5) ) 15)))

    (after '(extend ac (s env) ((g caris) s 'quote))
      (arc-test
       (( 'abc     ) 'abc)
       (( '()      ) 'nil)
       (( '(a)     ) (arc-list 'a))
       (( '(nil)   ) (arc-list 'nil))
       (( '(a . b) ) (mcons 'a 'b)))

      (arc-test
       (( (apply list 1 2 '(3 4)) ) (toarc '(1 2 3 4)))))

    (after '(ac-def ac-body)
      (test
       (let ((globals* (new-ac (build-steps))))
         ((g ac-body)
          (arc-list 1 2 3)
          'nil))
       (arc-list 1 2 3)))

    (after '(extend ac (s env) ((g caris) s 'fn))
      (arc-test
       (( ((fn ()))                  ) 'nil)
       (( ((fn () 3))                ) 3)
       (( ((fn (a) a) 3)             ) 3)
       (( ((fn (a b) b) 1 2)         ) 2)
       (( ((fn (a b) (+ a b 3)) 1 2) ) 6)))

    (after '(ac-def eval)
      (arc-test
       (( (eval 3)        ) 3)
       (( (eval '(+ 1 2)) ) 3)))

    (after '(extend ac (s env) ((g caris) s 'quasiquote))
      (arc-test
       (( `nil     ) 'nil)
       (( `3       ) 3)
       (( `a       ) 'a)
       (( `()      ) 'nil)
       (( `(1)     ) (arc-list 1))
       (( `(1 . 2) ) (mcons 1 2))
       (( `(1 2)   ) (arc-list 1 2))
       (( `((1 2)) ) (arc-list (arc-list 1 2)))

       (( `,(+ 1 2)         ) 3)
       (( `(,(+ 1 2))       ) (arc-list 3))
       (( `(1 2 ,(+ 1 2) 4) ) (arc-list 1 2 3 4))

       (( (eval ``3)         ) 3)
       (( (eval ``,,3)       ) 3)
       (( (eval ``,,(+ 1 2)) ) 3)

       (( `(1 ,@(list 2 3) 4)                   ) (arc-list 1 2 3 4))
       (( (eval ``,(+ 1 ,@(list 2 3) 4))        ) 10)

       ;; Note the following gives the wrong answer in Arc3.1 because of
       ;; Racket's nested list splicing bug.

       (( (eval (eval ``(+ 1 ,,@(list 2 3) 4))) ) 10)))

    (after '(extend ac (s env) ((g caris) s 'if))
      (arc-test
       (( (if)           ) 'nil)
       (( (if nil)       ) 'nil)
       (( (if 9)         ) 9)
       (( (if nil 1 2)   ) 2)
       (( (if 9 1 2)     ) 1)
       (( (if 9 1 2 3)   ) 1)
       (( (if nil 1 2 3) ) 3)))

    (after '(extend ac (s env) (ar-caris s 'assign))
      (arc-test
       (( (assign x 123) ) 123)

       (( ((fn ()
             (assign x 123)
             x)) )
        123)

       (( ((fn (x)
             (assign x 123))
           456) )
        123)

       (( ((fn (x)
             (assign x 123)
             x)
           456) )
        123)

       (( ((fn (a b)
             (assign a 11)
             (assign b 22)
             (list a b))
           1 2) )
        (arc-list 11 22))))

    (after '(ac-def ac-macro?)
      (arc-test
       (( (ac-macro? 5)    ) 'nil)
       (( (ac-macro? 'foo) ) 'nil)

       (( (assign foo 5)
          (ac-macro? 'foo) )
        'nil)

       (( (assign foo (annotate 'mac 123))
          (ac-macro? 'foo) )
        123)))

    (after '(extend ac-call (fn args env)
              (if (true? ((g ac-lex?) fn env))
                   'nil
                   ((g ac-macro?) fn)))
      (arc-test
       (( (assign foo (annotate 'mac (fn (x) x)))
          (foo 123) )
        123)))

    (after '(ac-def ac-fn-rest)
      (arc-test
       (( ((fn args (car args)) 1 2)             ) 1)
       (( (cdr ((fn args args) 1))               ) 'nil)
       (( ((fn (a b . rest) (car rest)) 1 2 3 4) ) 3)))

    (after '(ac-def bound)
      (arc-test
       (( (bound 'foo) )
        'nil)

       (( (assign foo nil)
          (bound 'foo) )
        't)

       (( (assign foo 123)
          (bound 'foo) )
        't)))

    (after '(ac-def racket-disp)
      (test-equal
       (let ((port (open-output-string))
             (globals* (new-ac (build-steps))))
         (hash-set! globals* 'port port)
         (arc-test-eval '( (racket-disp "a" port) ) globals*)
         (get-output-string port))
       "a")

      (test-equal
       (let ((globals* (new-ac (build-steps))))
         (tostringf (lambda ()
                      (arc-test-eval '( (racket-disp "abc") ) globals*))))
       "abc"))

    (after '(ac-def racket-write)
      (test-equal
       (let ((globals* (new-ac (build-steps))))
         (tostringf (lambda ()
                      (arc-test-eval '( (racket-write "a") ) globals*))))
       "\"a\""))

    (after '(extend ac-global (v) ((g ac-defined-var) v))
      (test-equal
       (let ((globals* (new-ac (build-steps))))
         (arc-test-eval '( (ac-defvar 'x (list (fn () 'foo))) ) globals*)
         (arc-test-eval '( x ) globals*))
       'foo))

    (after '(extend ac-global-assign (a b) ((g ac-defined-var) a))
      (test-equal
       (arc-test-eval '( (ac-defvar 'x (list nil (fn (x) (assign a (+ x 1)))))
                         (assign x 5)
                         a )
                      (new-ac (build-steps)))
       6)

      (test-expect-error
       (arc-test-eval '( (ac-defvar 'x (list nil))
                         (assign x 5) )
                      (new-ac (build-steps)))
       "x is not assignable"))

    (after '(ac-def table)
      (arc-test (( (table) ) (hash))))

    (after '(ac-def sref)
      (arc-test
       (( (assign a '(x y z))
          (sref a 'M 1)
          a )
        (arc-list 'x 'M 'z))

       (( (assign a (table))
          (sref a 55 'x)
          a)
        (hash 'x 55))

       (( (table (fn (h)
                   (sref h 55 'x)
                   (sref h 66 'y))) )
        (hash 'x 55 'y 66))

       (( (assign a "abcd")
          (sref a #\M 2)
          a )
        "abMd")))

    (after '(ac-def details)
      (arc-test
       (( (on-err details (fn () (/ 1 0))) )
        "/: division by zero")))

    (after '(ac-def parameter)
      (arc-test
       (( (type (parameter 3)) ) 'parameter)))

    ))

(run-ac-tests #t)