#lang scheme

(require mzlib/defmacro)

(require (only-in "ac.ss"
           racket-eval new-arc ac-build-steps get set g))

(define (arc-eval arc form)
  (racket-eval arc ((g ar-deep-fromarc) ((get arc 'ac) form 'nil))))

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
