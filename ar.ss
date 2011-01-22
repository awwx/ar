#lang scheme

(define run-repl         (make-parameter #t))
(define test-atend       (make-parameter #f))
(define test-inline      (make-parameter #f))
(define test-iteratively (make-parameter #f))
(define args #f)

(command-line
 #:program "arc"

 #:once-any

 (("--test-atend")
    "run tests once after all of Arc is loaded"
    (test-atend #t)
    (run-repl #f))

 (("--test-inline")
    "run tests as Arc is loaded"
    (test-inline #t)
    (run-repl #f))

 (("--test-iteratively")
    "run all tests defined so far at each step of building the compiler"
    (test-iteratively #t)
    (run-repl #f))

 (("--no-repl")
  "don't start the REPL"
  (run-repl #f))

 #:args remaining (set! args remaining)
 )

(require scheme/mpair)

(current-namespace (make-base-namespace))


(define (printwith-list port f xs)
  (cond ((eq? (mcdr xs) 'nil)
         (printwith port f (mcar xs))
         (display ")" port))
        ((mpair? (mcdr xs))
         (printwith port f (mcar xs))
         (display " " port)
         (printwith-list port f (mcdr xs)))
        (else
         (printwith port f (mcar xs))
         (display " . " port)
         (printwith port f (mcdr xs))
         (display ")" port))))

(define (printwith port f x)
  (cond ((mpair? x)
         (display "(" port)
         (printwith-list port f x))
        ; todo
        ((hash? x)
         (display "#table()" port))
        (else
         (f x port)))
  (flush-output port))

(define (print x)
  (printwith (current-output-port) write x))

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

(define-syntax test
  (syntax-rules ()
    ((test1 expr expected)
     (let ((r expr))
       (unless (equal? r expected)
         (display "bzzt! ")
         (write 'expr)
         (display " => ")
         (write r)
         (display " not ")
         (write expected)
         (newline))))))
  
(define (write-to-string x)
  (let ((port (open-output-string)))
    (write x port)
    (close-output-port port)
    (get-output-string port)))

(define (r/list-toarc x)
  (cond ((pair? x)
         (mcons (car x) (r/list-toarc (cdr x))))
        ((null? x)
         'nil)
        (else x)))

(test (r/list-toarc '())        'nil)
(test (r/list-toarc '(1 2 3))   (mcons 1 (mcons 2 (mcons 3 'nil))))
(test (r/list-toarc '(1 2 . 3)) (mcons 1 (mcons 2 3)))

(define (arc-list . rest)
  (r/list-toarc rest))

(test (arc-list 1 2 3) (mcons 1 (mcons 2 (mcons 3 'nil))))

(define (list-fromarc x)
  (cond ((mpair? x)
         (cons (mcar x) (list-fromarc (mcdr x))))
        ((eq? x 'nil)
         '())
        (else x)))

(test (list-fromarc 'nil)           '())
(test (list-fromarc (arc-list 1 2)) '(1 2))
(test (list-fromarc (mcons 1 2))    '(1 . 2))

(define (toarc x)
  (cond ((pair? x)
         (mcons (toarc (car x))
                (toarc (cdr x))))
        ((null? x)
         'nil)
        ((string? x)
         (string-copy x))
        (else x)))

(define-struct tunnel (x))

(define (deep-fromarc x)
  (cond ((tunnel? x)
         (tunnel-x x))

        ;; While I usually dislike being pedantic, this turned out to
        ;; be helpful in tracking down bugs.
        ;; Can have a more relaxed version for the runtime once the
        ;; compiler is working.
        ((or (pair? x) (null? x))
         (error "oops, a Racket list snuck in here!"))

        ;; nil in the car position isn't a list terminator, and so can
        ;; be left alone.
        ((mpair? x)
         (cons (let ((a (mcar x)))
                 (if (eq? a 'nil) 'nil (deep-fromarc a)))
               (let ((b (mcdr x)))
                 (if (eq? b 'nil) '() (deep-fromarc b)))))

        (else
         x)))

(define (no? x)
  (eq? x 'nil))

(define (true? x)
  (not (no? x)))

(define (arc-car x)
  (if (eq? x 'nil)
       'nil
       (mcar x)))

(test (equal? (arc-car 'nil)             'nil)
      (equal? (arc-car (arc-list 1 2 3)) 1))

(define (arc-cdr x)
  (if (eq? x 'nil)
       'nil
       (mcdr x)))

(test (arc-cdr 'nil)             'nil)
(test (arc-cdr (arc-list 1 2 3)) (arc-list 2 3))

(define (arc-cadr x)
  (arc-car (arc-cdr x)))

(test (arc-cadr (arc-list 1 2 3)) 2)

(define (arc-cddr x)
  (arc-cdr (arc-cdr x)))

(define (tnil x) (if x 't 'nil))

(define (arc-map1 f xs)
  (if (no? xs) 
      'nil
      (mcons (f (arc-car xs)) (arc-map1 f (arc-cdr xs)))))

(test (arc-map1 (lambda (x) (tnil (odd? x))) (arc-list 1 2 3 4)) (arc-list 't 'nil 't 'nil))

(define (arc-join . args)
  (r/list-toarc (apply append (map list-fromarc args))))

(test (arc-join) 'nil)
(test (arc-join (arc-list)) 'nil)
(test (arc-join 1) 1)
(test (arc-join (arc-list 1 2)) (arc-list 1 2))
(test (arc-join (arc-list) (arc-list)) 'nil)
(test (arc-join (arc-list 1 2) (arc-list)) (arc-list 1 2))
(test (arc-join (arc-list 1 2) 3) (mcons 1 (mcons 2 3)))
(test (arc-join (arc-list) (arc-list 1 2)) (arc-list 1 2))
(test (arc-join (arc-list 1 2) (arc-list 3 4)) (arc-list 1 2 3 4))
(test (arc-join (arc-list 1 2) 3) (mcons 1 (mcons 2 3)))
(test (arc-join (arc-list 1) (arc-list 2) (arc-list 3)) (arc-list 1 2 3))

(define err error)

(define-syntax ar-if
  (syntax-rules ()
    ((ar-if)
     'nil)
    ((ar-if x)
     x)
    ((ar-if t x)
     (if (eqv? t 'nil) 'nil x))
    ((ar-if t a . rest)
     (if (eqv? t 'nil) (ar-if . rest) a))
    ))

(test (ar-if)                    'nil)
(test (ar-if 'nil)               'nil)
(test (ar-if 44)                 44)
(test (ar-if 'nil 44)            'nil)
(test (ar-if 14 44)              44)
(test (ar-if 'nil 33 44)         44)
(test (ar-if 'foo 33 44)         33)
(test (ar-if 'nil 33 'yes 44 55) 44)
(test (ar-if 'nil 33 'nil 44 55) 55)
(test (ar-if 'nil 33 'nil 44)    'nil)

(define-syntax ar-or
  (syntax-rules ()
    ((ar-or)          'nil)
    ((ar-or x xs ...) (ar-if x x (ar-or xs ...)))))

(test (ar-or)              'nil)
(test (ar-or 'nil)         'nil)
(test (ar-or 33)           33)
(test (ar-or 'nil 33)      33)
(test (ar-or 11 33)        11)
(test (ar-or 'nil 'nil 33) 33)

(define-syntax ar-and
  (syntax-rules ()
    ((ar-and)         't)
    ((ar-and x)        x)
    ((ar-and x xs ...) (ar-if x (ar-and xs ...)))))

(define (ar-reclist f xs)
  (ar-and xs (ar-or (f xs) (ar-reclist f (mcdr xs)))))

(define (ar-is2 a b)
  (tnil (or (eqv? a b)
            (and (string? a) (string? b) (string=? a b)))))

(define (pairwise pred lst)
  (cond ((null? lst) 't)
        ((null? (cdr lst)) 't)
        ((not (eqv? (pred (car lst) (cadr lst)) 'nil))
         (pairwise pred (cdr lst)))
        (else 'nil)))

(define (arc-is . args)
  (pairwise ar-is2 args))

(test (arc-is)       't)
(test (arc-is 4)     't)
(test (arc-is 3 4)   'nil)
(test (arc-is 4 4)   't)
(test (arc-is 4 4 5) 'nil)
(test (arc-is 4 4 4) 't)

(define (ar-caris x val)
  (tnil (and (mpair? x)
             (true? (arc-is (arc-car x) val)))))

(test (ar-caris 4 'x)                'nil)
(test (ar-caris (arc-list 'y 'z) 'x) 'nil)
(test (ar-caris (arc-list 'x 'y) 'x) 't)

(define (tagged? x)
  (and (vector? x) (eq? (vector-ref x 0) 'tagged)))

(define (ar-rep x)
  (if (tagged? x)
      (vector-ref x 2)
      x))

(define (exint? x) (and (integer? x) (exact? x)))

(define (arc-type x)
  (cond ((tagged? x)        (vector-ref x 1))
        ((mpair? x)         'cons)
        ((symbol? x)        'sym)
        ((procedure? x)     'fn)
        ((char? x)          'char)
        ((string? x)        'string)
        ((exint? x)         'int)
        ((number? x)        'num)     ; unsure about this
        ((hash? x)          'table)
        ((output-port? x)   'output)
        ((input-port? x)    'input)
        ((tcp-listener? x)  'socket)
        ((exn? x)           'exception)
        ((thread? x)        'thread)
        ((thread-cell? x)   'thread-cell)
        ((semaphore? x)     'semaphore)
        (else               (err "Type: unknown type" x))))

(define (ar-tag type rep)
  (cond ((eqv? (arc-type rep) type) rep)
        (else (vector 'tagged type rep))))

(define (arc-isa x y)
  (arc-is (arc-type x) y))

(define (ar-testify x)
  (ar-if (arc-isa x 'fn) x (lambda (_) (arc-is _ x))))

(define (ar-mem test seq)
  (let ((f (ar-testify test)))
    (ar-reclist (lambda (_) (ar-if (f (mcar _)) _)) seq)))

(test (ar-mem 'x 'nil) 'nil)
(let* ((d 'nil) (c (mcons 3 d)) (b (mcons 2 c)) (a (mcons 1 b)))
  (test (ar-mem 1 a) a)
  (test (ar-mem 2 a) b)
  (test (ar-mem 3 a) c)
  (test (ar-mem 4 a) d)
  (test (ar-mem 5 a) 'nil))

(define (iround x) (inexact->exact (round x)))

(define (arc-coerce x type . args)
  (cond 
    ((tagged? x) (err "Can't coerce annotated object"))
    ((eqv? type (arc-type x)) x)
    ((char? x)      (case type
                      ((int)     (char->integer x))
                      ((string)  (string x))
                      ((sym)     (string->symbol (string x)))
                      (else      (err "Can't coerce" x type))))
    ((exint? x)     (case type
                      ((num)     x)
                      ((char)    (integer->char x))
                      ((string)  (apply number->string x args))
                      (else      (err "Can't coerce" x type))))
    ((number? x)    (case type
                      ((int)     (iround x))
                      ((char)    (integer->char (iround x)))
                      ((string)  (apply number->string x args))
                      (else      (err "Can't coerce" x type))))
    ((string? x)    (case type
                      ((sym)     (string->symbol x))
                      ((cons)    (r/list-toarc (string->list x)))
                      ((num)     (or (apply string->number x args)
                                     (err "Can't coerce" x type)))
                      ((int)     (let ((n (apply string->number x args)))
                                   (if n 
                                       (iround n)
                                       (err "Can't coerce" x type))))
                      (else      (err "Can't coerce" x type))))
    ((mpair? x)     (case type
                      ((string)  (apply string-append
                                        (list-fromarc
                                         (arc-map1 (lambda (y) (arc-coerce y 'string)) x))))
                      (else      (err "Can't coerce" x type))))
    ((eq? x 'nil)   (case type
                      ((string)  "")
                      (else      (err "Can't coerce" x type))))
    ((symbol? x)    (case type 
                      ((string)  (symbol->string x))
                      (else      (err "Can't coerce" x type))))
    (#t             x)))

(test (arc-coerce #\A                   'int)       65)
(test (arc-coerce #\A                   'string)    "A")
(test (arc-coerce #\A                   'sym)       'A)
(test (arc-coerce 123                   'num)       123)
(test (arc-coerce 65                    'char)      #\A)
(test (arc-coerce 123                   'string)    "123")
(test (arc-coerce 128                   'string 16) "80")
(test (arc-coerce 13.4                  'int)       13)
(test (arc-coerce 65.0                  'char)      #\A)
(test (arc-coerce 14.5                  'string)    "14.5")
(test (arc-coerce "foo"                 'sym)       'foo)
(test (arc-coerce "foo"                 'cons)      (arc-list #\f #\o #\o))
(test (arc-coerce "123.5"               'num)       123.5)
(test (arc-coerce "123"                 'int)       123)
(test (arc-coerce (arc-list "a" 'b #\c) 'string)    "abc")
(test (arc-coerce 'nil                  'string)    "")


(define (char-or-string? x) (or (string? x) (char? x)))

(define (arc-list? x) (or (no? x) (mpair? x)))

(define (ar-+ . args)
  (cond ((null? args)
         0)
        ((char-or-string? (car args))
         (apply string-append 
                (map (lambda (a) (arc-coerce a 'string)) args)))
        ((arc-list? (car args)) 
         (apply arc-join args))
        (else
         (apply + args))))

(test (ar-+) 0)
(test (ar-+ #\a "b" 'c 3) "abc3")
(test (ar-+ "a" 'b #\c) "abc")
(test (ar-+ 'nil (arc-list 1 2 3)) (arc-list 1 2 3))
(test (ar-+ (arc-list 1 2) (arc-list 3)) (arc-list 1 2 3))
(test (ar-+ 1 2 3) 6)


(define (arc->2 x y)
  (tnil (cond ((and (number? x) (number? y)) (> x y))
              ((and (string? x) (string? y)) (string>? x y))
              ((and (symbol? x) (symbol? y)) (string>? (symbol->string x)
                                                       (symbol->string y)))
              ((and (char? x) (char? y)) (char>? x y))
              (#t (> x y)))))

(define (arc-> . args) (pairwise arc->2 args))

(define (arc-<2 x y)
  (tnil (cond ((and (number? x) (number? y)) (< x y))
              ((and (string? x) (string? y)) (string<? x y))
              ((and (symbol? x) (symbol? y)) (string<? (symbol->string x)
                                                       (symbol->string y)))
              ((and (char? x) (char? y)) (char<? x y))
              (#t (< x y)))))

(define (arc-< . args) (pairwise arc-<2 args))


(define (arc-list-len x)
  (cond ((no? x)    0)
        ((mpair? x) (+ 1 (arc-list-len (mcdr x))))
        (else       (err "len expects a proper list"))))

(test (arc-list-len (arc-list))       0)
(test (arc-list-len (arc-list 1))     1)
(test (arc-list-len (arc-list 1 2))   2)
(test (arc-list-len (arc-list 1 2 3)) 3)


(define (arc-len x)
  (cond ((string? x) (string-length x))
        ((hash? x)   (hash-count x))
        (else        (arc-list-len x))))

(test (arc-len "abc")            3)
(test (arc-len (hash 'a 1 'b 2)) 2)
(test (arc-len (arc-list 1 2 3)) 3)


(define (ar-apply fn . r/args)
  (cond ((procedure? fn)
         (apply fn r/args))
        ((mpair? fn)
         (mlist-ref fn (car r/args)))
        ((string? fn)
         (string-ref fn (car r/args)))
        ((hash? fn)
         (hash-ref fn
                   (car r/args)
                   (let ((default (if (pair? (cdr r/args)) (car (cdr r/args)) 'nil)))
                     (lambda () default))))
        (else (error "Function call on inappropriate object" fn r/args))))

(test (ar-apply + 1 2 3) 6)
(test (ar-apply (arc-list 1 2 3) 1) 2)
(test (ar-apply "abcde" 2) #\c)
(test (ar-apply (hash 'a 1 'b 2) 'b) 2)
(test (ar-apply (hash 'a 1 'b 2) 'x) 'nil)
(test (ar-apply (hash 'a 1 'b 2) 'x 3) 3)


(define (combine as (accum '()))
  (cond ((null? (cdr as))
         (append accum (list-fromarc (car as))))
        (else
         (combine (cdr as) (append accum (list (car as)))))))

(test (combine (list (arc-list 'a 'b 'c)))    '(a b c))
(test (combine (list 'a (arc-list 'b 'c 'd))) '(a b c d))
(test (combine (list 'a 'b (arc-list 'c 'd))) '(a b c d))


(define (arc-apply fn . args)
  (apply ar-apply fn (combine args)))

(test (arc-apply ar-+ 'nil (toarc '((a b) (c d)))) (toarc '(a b c d)))


(define (ar-funcall0 fn)
  (if (procedure? fn)
      (fn)
      (ar-apply fn)))

(test (ar-funcall0 +) 0)

(define (ar-funcall1 fn arg1)
  (if (procedure? fn)
      (fn arg1)
      (ar-apply fn arg1)))

(test (ar-funcall1 + 3) 3)
(test (ar-funcall1 "abcd" 2) #\c)

(define (ar-funcall2 fn arg1 arg2)
  (if (procedure? fn)
      (fn arg1 arg2)
      (ar-apply fn arg1 arg2)))

(test (ar-funcall2 + 3 4) 7)
(test (ar-funcall2 (hash 'a 1 'b 2) 'x 3) 3)

(define (ar-funcall3 fn arg1 arg2 arg3)
  (if (procedure? fn)
      (fn arg1 arg2 arg3)
      (ar-apply fn arg1 arg2 arg3)))

(test (ar-funcall3 + 3 4 5) 12)

(define (ar-funcall4 fn arg1 arg2 arg3 arg4)
  (if (procedure? fn)
      (fn arg1 arg2 arg3 arg4)
      (ar-apply fn arg1 arg2 arg3 arg4)))

(test (ar-funcall4 + 3 4 5 6) 18)


(define (racket-module a/module)
  (let ((r/module (deep-fromarc a/module)))
    (lambda (sym)
      (dynamic-require r/module sym))))

(define (racket-parameterize parameter value body)
  (parameterize ((parameter value))
    (body)))

(define (arc-writec c (port (current-output-port)))
  (write-char c port))


(define (arc-readc (port (current-input-port)))
  (let ((c (read-char port)))
    (if (eof-object? c) 'nil c)))

(define (arc-peekc (port (current-input-port)))
  (let ((c (peek-char port)))
    (if (eof-object? c) 'nil c)))

(define ar-namespace*
  (hash '+                   ar-+
        '-                   -
        '/                   /
        '*                   *
        '<                   arc-<
        '>                   arc->
        'annotate            ar-tag
        'apply               arc-apply
        'car                 arc-car
        'caris               ar-caris
        'ccc                 call-with-current-continuation
        'cdr                 arc-cdr
        'coerce              arc-coerce
        'cons                mcons
        'err                 err
        'join                arc-join
        'inside              get-output-string
        'instring            open-input-string
        'is                  arc-is
        'len                 arc-len
        'list                arc-list
        'map1                arc-map1
        'mem                 ar-mem
        'outstring           open-output-string
        'peekc               arc-peekc
        'r/list-toarc        r/list-toarc
        'racket-stdin        current-input-port
        'racket-stdout       current-output-port
        'racket-stderr       current-error-port
        'racket-module       racket-module
        'racket-parameterize racket-parameterize
        'readc               arc-readc
        't                   't
        'type                arc-type
        'uniq                gensym
        'writec              arc-writec
        ))

(define (new-ar)
  (hash-copy ar-namespace*))


;; testing the compiler

; For a general purpose tracing facility I'd use a parameter, but this
; is good enough for here.

(define traces '())

(define (trace . args)
  (set! traces (append traces (list (cons 'racket args)))))

(define (trace/arc . args)
  (set! traces (append traces (list (cons 'arc args)))))

(define (display-trace)
  (map (lambda (trace)
         (let ((w (if (eq? (car trace) 'racket) write print)))
           (display (cadr trace))
           (display ": ")
           (map (lambda (arg)
                  (w arg)
                  (display " "))
                (cddr trace))
           (newline)))
       traces))
       
; Trace each step of reading, compiling, eval'ing an Arc program, with
; all the converting lists back and forth.

(define (trace-eval r/arc-program globals)
  (trace "program (racket)" r/arc-program)
  (let ((final 'nil))
    (for-each (lambda (r/source)
                (let ((a/source (toarc r/source)))
                  (trace "program (arc)" a/source)
                  (let ((a/compiled ((hash-ref globals 'ac) a/source 'nil)))
                    (trace/arc "compiled (arc)" a/compiled)
                    (let ((r/compiled (deep-fromarc a/compiled)))
                      (let ((result (eval r/compiled)))
                        (trace "result" result)
                        (set! final result))))))
              r/arc-program)
    final))

; If a test fails, display all the steps.

(define (test-eval arc-program globals*)
  (set! traces '())
  (with-handlers ((exn:fail? (lambda (c)
                               (display-trace)
                               (raise c))))
    (trace-eval arc-program globals*)))

; A test adds itself to the list of tests, and also (when running
; tests iteratively) runs all the tests again (including itself) from
; the beginning.  This allows us e.g. to add things like optimizations
; to the compiler, and all the previous tests will be run again to
; ensure that they still work.

(define arc-tests '())

(define (run-tests)
  (map (lambda (test)
         (test))
       arc-tests)
  (void))

(define test* #t)

(define (add-tests tests)
  (when test*
    (set! arc-tests (append arc-tests tests))
    (when (test-inline)
      (map (lambda (test) (test)) tests))
    (when (test-iteratively)
      (run-tests))))

(define (add-test test)
  (add-tests (list test)))

(define (test-t-impl thunk)
  (add-test
   (lambda ()
     (unless (true? (thunk))
       (error "not true")))))

(define (test-nil-impl thunk)
  (add-test
   (lambda ()
     (unless (no? (thunk))
       (error "not nil")))))

(define-syntax test-t 
  (syntax-rules ()
    ((test-t body ...)
     (test-t-impl (lambda () body ...)))))

(define-syntax test-nil
  (syntax-rules ()
    ((test-nil body ...)
     (test-nil-impl (lambda () body ...)))))

(define (test-equal-impl source thunk expected)
  (add-test
   (lambda ()
     (let ((result (thunk)))
       (unless (equal? result expected)
         (error (string-append
                 "bzzt! "
                 (write-to-string source)
                 " => "
                 (write-to-string result)
                 " not "
                 (write-to-string expected))))))))

(define-syntax test-equal
  (syntax-rules ()
    ((test-equal expr expected)
     (test-equal-impl 'expr (lambda () expr) expected))))

(define (test-expect-error-impl source thunk expected-error-message)
  (add-test
   (lambda ()
     (let ((actual-error
            (with-handlers ((exn:fail? (lambda (c) (exn-message c))))
              (thunk)
              'oops-no-error-after-all)))
       (unless (and (>= (string-length actual-error)
                        (string-length expected-error-message))
                    (equal? (substring actual-error 0 (string-length expected-error-message))
                            expected-error-message))
         (error actual-error))))))

(define-syntax test-expect-error
  (syntax-rules ()
    ((test-expect-error expr expected-error-message)
     (test-expect-error-impl 'expr (lambda () expr) expected-error-message))))

; Test that compiling and eval'ing an Arc program produces the
; expected Racket result.

(define (make-arc-test source expected)
  (lambda ()
    (let ((globals* (new-ac)))
      (let ((result (test-eval source globals*)))
        (if (equal? result expected)
             (begin (display "ok ")
                    (for-each (lambda (s)
                                (write s)
                                (display " "))
                              source)
                    (display "=> ")
                    (write result)
                    (newline))
             (begin (display "bzzt!\n")
                    (display-trace)
                    (display "result: ") (write result) (newline)
                    (display "not: ") (write expected) (newline)
                    (raise "failed")))))))

(define-syntax test-arc
  (syntax-rules ()
    ((test-arc (source expected) ...)
     (add-tests (list (make-arc-test 'source expected) ...)))))


;; Arc compiler steps

; The compiler is built up in steps, so that simple cases can be
; tested before more complex cases are implemented.

(define ac-build-steps '())

(define (add-ac-build-step step)
  (set! ac-build-steps (append ac-build-steps (list step))))

; Return a global variable namespace that includes the Arc runtime
; globals (car, +, etc.) and whatever Arc compiler globals that have
; been defined so far (ac, ac-literal?, etc.)  Note that a fresh copy
; of the compiler is created each time (new-ac) is called, including
; the compiler building steps defined so far.

(define (new-ac . args)
  (let ((globals* (new-ar)))
    (for-each (lambda (step)
                (step globals*))
              ac-build-steps)
    (add-to-hash globals* args)
    globals*))


;; sig

(add-ac-build-step
 (lambda (globals*)
   (hash-set! globals* 'sig (hash))))

(define-syntax g
  (lambda (stx)
    (syntax-case stx ()
      ((g v)
       (with-syntax ((globals* (datum->syntax #'v 'globals*)))
         #'(hash-ref globals* 'v))))))

(define (ac-def-fn globals* name signature fn)
  (hash-set! (hash-ref globals* 'sig) name (toarc signature))
  (hash-set! globals* name fn))

(define-syntax ac-def
  (lambda (stx)
    (syntax-case stx ()
      ((ac-def name args body ...)
       (with-syntax ((globals* (datum->syntax #'name 'globals*)))
         #'(add-ac-build-step
             (lambda (globals*)
               (ac-def-fn globals* 'name 'args (lambda args body ...)))))))))


;; The Arc compiler!

(ac-def ac (s env)
  ((g err) "Bad object in expression" s))

; ...which is extended to do more below :-)


(test-expect-error
 (let ((globals* (new-ac)))
   ((g ac) (lambda () 'foo) 'nil))
 "Bad object in expression")


; Extending the Arc compiler

(define (extend-impl name test body)
  (add-ac-build-step
   (lambda (globals*)
     (let ((previous (hash-ref globals* name)))
       (hash-set! globals* name
         (lambda args
           (let ((result (apply test globals* args)))
             (if (true? result)
                  (apply body globals* result args)
                  (apply previous args)))))))))

(define-syntax extend
  (lambda (stx)
    (syntax-case stx ()
      ((extend name args test body ...)
       (with-syntax ((globals* (datum->syntax #'args 'globals*))
                     (it       (datum->syntax #'args 'it)))
         #'(extend-impl 'name
            (lambda (globals* . args) test)
            (lambda (globals* it . args) body ...)))))))


;; literal

(ac-def ac-literal? (x)
  (tnil (or (char? x)
            (string? x)
            (number? x))))

(extend ac (s env)
  ((g ac-literal?) s)
  s)

(test-arc
 (( 123   ) 123)
 (( #\a   ) #\a)
 (( "abc" ) "abc"))

; it's alive!


;; nil

(define (ac-nil)
  (arc-list 'quote 'nil))

(extend ac (s env)
  (tnil (eq? s 'nil))
  (ac-nil))

(test-arc (( nil ) 'nil))


;; variables

(ac-def ac-lex? (v env)
  ((g mem) v env))

(test-t (let ((globals* (new-ac)))
          ((g ac-lex?)
           'y
           (arc-list 'x 'y 'z))))

(test-nil (let ((globals* (new-ac)))
            ((g ac-lex?)
             'w
             (arc-list 'x 'y 'z))))

(define (global-ref-err globals* v)
  (let ((message (string-append "undefined global variable: "
                                (symbol->string v))))
    (lambda ()
      ((g err) message))))

; An Arc global variable reference to "foo" such as in (+ foo 3) compiles into
; the Racket expression
; (#<procedure:hash-ref> #<hash:globals*> 'foo #<procedure:global-ref-err>)
; ...and thus performs no lookups in Racket's namespace (if it makes a difference).

(ac-def ac-global (v)
  (arc-list hash-ref
            globals*
            (arc-list 'quote v)
            (global-ref-err globals* v)))

(ac-def ac-var-ref (s env)
  (ar-if ((g ac-lex?) s env)
         s
         ((g ac-global) s)))

(extend ac (s env)
  (ar-and (tnil (not (no? s))) (tnil (symbol? s)))
  ((g ac-var-ref) s env))

(test-expect-error
 ; todo: should clear trace here
 (trace-eval '( foo ) (new-ac))
 "undefined global variable: foo")

(test-arc
 (( car ) arc-car))


;; call
; todo optimizations

(ac-def ac-call (fn args env)
  (mcons ar-apply
         (mcons ((g ac) fn env)
                ((g map1) (lambda (x)
                            ((g ac) x env)) args))))

(extend ac (s env)
  (tnil (mpair? s))
  ((g ac-call) (arc-car s) (arc-cdr s) env))

(test-arc
 (( (+)           ) 0)
 (( (+ 1 2)       ) 3)
 (( (+ 1 2 3)     ) 6)
 (( (+ 1 2 3 4)   ) 10)
 (( (+ 1 2 3 4 5) ) 15))


;; quote

(extend ac (s env)
  ((g caris) s 'quote)
  ((g list) 'quote (make-tunnel (arc-cadr s))))

(test-arc
 (( 'abc     ) 'abc)
 (( '()      ) 'nil)
 (( '(a)     ) (arc-list 'a))
 (( '(nil)   ) (arc-list 'nil))
 (( '(a . b) ) (mcons 'a 'b)))


;; apply

(test-arc
 (( (apply list 1 2 '(3 4)) ) (toarc '(1 2 3 4))))


;; fn
; rest args, optional args, arg list destructuring implemented later

(ac-def ac-body (body env)
  (arc-map1 (lambda (x) ((g ac) x env)) body))

(test-equal
 (let ((globals* (new-ac)))
   ((g ac-body)
    (arc-list 1 2 3)
    'nil))
 (arc-list 1 2 3))

(ac-def ac-body* (body env)
  (if (no? body)
      ((g list) (ac-nil))
      ((g ac-body) body env)))

(ac-def ac-body*x (args body env)
  ((g ac-body*) body (arc-join ((g ac-arglist) args) env)))

(ac-def ac-arglist (a)
  (cond ((no? a) 'nil)
        ((symbol? a) (arc-list a))
        ((and (symbol? (mcdr a)) (not (no? (mcdr a))))
         (arc-list (mcar a) (mcdr a)))
        (else (mcons (mcar a) ((g ac-arglist) (mcdr a))))))

(ac-def dotted-list? (x)
  (cond ((and (symbol? x) (not (eq? x 'nil)))
         't)
        ((mpair? x)
         ((g dotted-list?) (mcdr x)))
        (else
         'nil)))

(ac-def ac-fn (args body env)
  (if (true? ((g dotted-list?) args))
       ((g ac-fn-rest) args body env)
       (mcons 'lambda
              (mcons args
                     ((g ac-body*x) args body env)))))

(extend ac (s env)
  ((g caris) s 'fn)
  ((g ac-fn) (arc-cadr s) (arc-cddr s) env))

(test-arc
 (( ((fn ()))                  ) 'nil)
 (( ((fn () 3))                ) 3)
 (( ((fn (a) a) 3)             ) 3)
 (( ((fn (a b) b) 1 2)         ) 2)
 (( ((fn (a b) (+ a b 3)) 1 2) ) 6))


;; eval

(ac-def eval (form (namespace 'nil))
  (eval (deep-fromarc ((hash-ref (ar-or namespace globals*) 'ac) form 'nil))))

(test-arc
 (( (eval 3)        ) 3)
 (( (eval '(+ 1 2)) ) 3))


;; quasiquotation

; qq-expand takes an Arc list containing a quasiquotation expression
; (the x in `x), and returns an Arc list containing Arc code.  The Arc
; code, when evaled by Arc, will construct an Arc list, the
; expansion of the quasiquotation expression.

; This implementation is Alan Bawden's quasiquotation expansion
; algorithm from "Quasiquotation in Lisp"
; http://repository.readscheme.org/ftp/papers/pepm99/bawden.pdf

; You can redefine qq-expand in Arc if you want to implement a
; different expansion algorithm.

(ac-def qq-expand (x)
  (cond ((true? (ar-caris x 'unquote))
         (arc-cadr x))
        ((true? (ar-caris x 'unquote-splicing))
         (error "illegal use of ,@ in non-list quasiquote expansion"))
        ((true? (ar-caris x 'quasiquote))
         ((g qq-expand) ((g qq-expand) (arc-cadr x))))
        ((mpair? x)
         ((g qq-expand-pair) x))
        (else
         (arc-list 'quote x))))

(ac-def qq-expand-pair (x)
  (arc-list 'join
            ((g qq-expand-list) (mcar x))
            ((g qq-expand) (mcdr x))))

(ac-def qq-expand-list (x)
  (cond ((true? (ar-caris x 'unquote))
         (arc-list 'list (arc-cadr x)))
        ((true? (ar-caris x 'unquote-splicing))
         (arc-cadr x))
        ((true? (ar-caris x 'quasiquote))
         ((g qq-expand-list) ((g qq-expand) (arc-cadr x))))
        ((mpair? x)
         (arc-list 'list ((g qq-expand-pair) x)))
        (else
         (arc-list 'quote (list x)))))

(extend ac (s env)
  ((g caris) s 'quasiquote)
  (let ((expansion ((g qq-expand) (arc-cadr s))))
    ((g ac) expansion env)))

(test-arc
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
 (( (eval (eval ``(+ 1 ,,@(list 2 3) 4))) ) 10))


;; if

(ac-def ac-if (args env)
  (cond ((no? args)
         (ac-nil))
        ((no? ((g cdr) args))
         ((g ac) ((g car) args) env))
        (else
         (arc-list 'if
                   (arc-list true? ((g ac) ((g car) args) env))
                   ((g ac) (arc-cadr args) env)
                   ((g ac-if) (arc-cddr args) env)))))

(extend ac (s env)
  ((g caris) s 'if)
  ((g ac-if) ((g cdr) s) env))

(test-arc
 (( (if)           ) 'nil)
 (( (if nil)       ) 'nil)
 (( (if 9)         ) 9)
 (( (if nil 1 2)   ) 2)
 (( (if 9 1 2)     ) 1)
 (( (if 9 1 2 3)   ) 1)
 (( (if nil 1 2 3) ) 3))


;; assign

(ac-def ac-global-assign (a b)
  (arc-list hash-set! globals* (arc-list 'quote a) b))

(ac-def ac-assign1 (a b1 env)
  (unless (symbol? a)
    (err "First arg to assign must be a symbol" a))
  (let ((result (gensym)))
    (arc-list 'let
              (arc-list (arc-list result ((g ac) b1 env)))
              (if (true? ((g ac-lex?) a env))                  
                   (arc-list 'set! a result)
                   ((g ac-global-assign) a result))
              result)))

(ac-def ac-assignn (x env)
  (if (no? x)
      'nil
      ;; todo: Arc 3.1 calls ac-macex here
      (mcons ((g ac-assign1) (arc-car x) (arc-cadr x) env)
             ((g ac-assignn) (arc-cddr x) env))))

(ac-def ac-assign (x env)
  (mcons 'begin
         ((g ac-assignn) x env)))

(extend ac (s env)
  (ar-caris s 'assign)
  ((g ac-assign) (arc-cdr s) env))

(test-arc
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
  (arc-list 11 22)))


;; macro

(ac-def ac-macro? (fn)
  (if (symbol? fn)
      (let ((v (hash-ref globals* fn 'nil)))
        (if (and (tagged? v)
                 (eq? (arc-type v) 'mac))
            (ar-rep v)
            'nil))
      'nil))

(test-arc
 (( (ac-macro? 5)    ) 'nil)
 (( (ac-macro? 'foo) ) 'nil)

 (( (assign foo 5)
    (ac-macro? 'foo) )
  'nil)

 (( (assign foo (annotate 'mac 123))
    (ac-macro? 'foo) )
  123))

(ac-def ac-mac-call (m args env)
  (let ((x1 (arc-apply m args)))
    (let ((x2 ((g ac) x1 env)))
      x2)))

(extend ac-call (fn args env)
  ((g ac-macro?) fn)
  ((g ac-mac-call) it args env))

(test-arc
 (( (assign foo (annotate 'mac (fn (x) x)))
    (foo 123) )
  123))


;; Eval Arc code as a step in building ac ^_^

(define (test-form? form)
  (and (pair? form)
       (symbol? (car form))
       (let ((s (symbol->string (car form))))
         (and (>= (string-length s) 4)
              (equal? (substring s 0 4) "test")))))
  
(define (ac-eval-impl forms)
  (for-each (lambda (form)
              (if (test-form? form)
                   (add-test (lambda ()
                               (let ((globals* (new-ac)))
                                 (test-eval (list form) globals*))))
                   (add-ac-build-step
                    (lambda (globals*)
                      (trace-eval (list form) globals*)))))
            forms))

(define (readall)
  (let ((x (read)))
    (if (eof-object? x)
         '()
         (cons x (readall)))))

(define (ac-eval-file filename)
  (with-input-from-file filename
    (lambda ()
      (ac-eval-impl (readall)))))

(define-syntax ac-eval
  (syntax-rules ()
    ((ac-eval form ...)
     (ac-eval-impl '(form ...)))))


;; fn rest arg

(ac-def ac-rest-param (x)
  (cond ((and (symbol? x) (not (eq? x 'nil)))
         x)
        ((mpair? x)
         ((g ac-rest-param) (mcdr x)))
        (else
         (error "not a dotted list"))))

(ac-def ac-args-without-rest (x)
  (cond ((mpair? x)
         (arc-join (arc-list (arc-car x)) ((g ac-args-without-rest) (mcdr x))))
        (else
         'nil)))

; The implementation of "ac-fn-rest" turned out to be a lot easier to
; write in Arc.

(add-ac-build-step
 (lambda (globals*)
   (hash-set! globals* 'ac-fn-rest-impl
     (trace-eval
      '( (fn (args r/rest rest body env)
           `(lambda ,(join args r/rest)
              (let ((,rest (,r/list-toarc ,r/rest)))
                ,@(ac-body*x (join args (list rest)) body env)))) )
      globals*))))

(ac-def ac-fn-rest (args body env)
  ((g ac-fn-rest-impl)
     ((g ac-args-without-rest) args)
     (gensym)
     ((g ac-rest-param) args)
     body
     env))

(test-arc
 (( ((fn args (car args)) 1 2)             ) 1)
 (( (cdr ((fn args args) 1))               ) 'nil)
 (( ((fn (a b . rest) (car rest)) 1 2 3 4) ) 3))


;; do

(ac-eval
  (assign do (annotate 'mac
               (fn args `((fn () ,@args))))))

; dum de dum dum!

(test-arc
 (( (do (assign a 1)
        (assign b 2)
        (assign c 3)
        (list a b c)) )
  (arc-list 1 2 3)))


; bound

(ac-def bound (x)
  (tnil (hash-ref globals* x (lambda () #f))))

(test-arc
  (( (bound 'foo) )
   'nil)

  (( (assign foo nil)
     (bound 'foo) )
   't)

  (( (assign foo 123)
     (bound 'foo) )
   't))


;; disp

(define (tostringf f)
  (let ((port (open-output-string)))
    (parameterize ((current-output-port port))
      (f))
    (get-output-string port)))

(ac-def disp (x (port (current-output-port)))
  (printwith port display x))

(ac-def write (x (port (current-output-port)))
  (printwith port write x))

(test-equal
 (let ((port (open-output-string))
       (globals* (new-ac)))
   (hash-set! globals* 'port port)
   (test-eval '( (disp '("a" b 3) port) ) globals*)
   (get-output-string port))
 "(a b 3)")

(test-equal
 (let ((globals* (new-ac)))
   (tostringf (lambda ()
                (test-eval '( (disp '("a" b 3)) ) globals*))))
 "(a b 3)")

(test-equal
 (let ((globals* (new-ac)))
   (tostringf (lambda ()
                (test-eval '( (write '("a" b 3)) ) globals*))))
 "(\"a\" b 3)")


;; defvar impl

(add-ac-build-step
 (lambda (globals*)
   (hash-set! globals* 'ac-defined-vars* (hash))))

(ac-def ac-defvar (v x)
  (hash-set! (g ac-defined-vars*) v x)
  'nil)

(ac-def ac-defined-var (v)
  (hash-ref (g ac-defined-vars*) v (lambda () 'nil)))

(extend ac-global (v)
  ((g ac-defined-var) v)
  (arc-list ar-apply (mcar it)))

(test-equal
 (let ((globals* (new-ac)))
   (trace-eval '( (ac-defvar 'x (list (fn () 'foo))) ) globals*)
   (trace-eval '( x ) globals*))
 'foo)

(ac-def ac-not-assignable (v)
  (lambda (x)
    (err (string-append (symbol->string v) " is not assignable"))))

(extend ac-global-assign (a b)
  ((g ac-defined-var) a)
  (arc-list ar-apply
            (ar-or (arc-cadr it) ((g ac-not-assignable) a))
            b))

(test-equal
 (trace-eval '( (ac-defvar 'x (list nil (fn (x) (assign a (+ x 1)))))
                (assign x 5)
                a )
             (new-ac))
 6)

(test-expect-error
 (trace-eval '( (ac-defvar 'x (list nil))
                (assign x 5) )
              (new-ac))
 "x is not assignable")


;; safeset

(ac-eval
 (assign safeset (annotate 'mac
                   (fn (var val)
                     `(do (if (bound ',var)
                              (do (disp "*** redefining " (racket-stderr))
                                  (disp ',var (racket-stderr))
                                  (disp #\newline (racket-stderr))))
                          (assign ,var ,val))))))

(test-arc (( (safeset a 123) a ) 123))

(test-equal
 (let ((port (open-output-string)))
   (parameterize ((current-error-port port))
     (trace-eval '( (safeset a 123)
                    (safeset a 456) )
                 (new-ac)))
   (get-output-string port))
 "*** redefining a\n")


;; table

(ac-def table ((init #f))
  (let ((h (hash)))
    (when init (init h))
    h))

(test-arc (( (table) ) (hash)))


;; sref

(ac-def sref (com val ind)
  (cond ((hash? com)
         (if (eq? val 'nil)
             (hash-remove! com ind)
             (hash-set! com ind val)))
        ((string? com)
         (string-set! com ind val))
        ((mpair? com)
         (set-mcar! (mlist-tail com ind) val))
        (else
         (err "Can't set reference" com ind val)))
  val)

(test-arc
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
  "abMd"))


(ac-eval
 (assign assign-fn
   (annotate 'mac
     (fn (name signature func)
       `(do (sref sig ',signature ',name)
            (safeset ,name ,func))))))

(test-arc
 (( (assign-fn foo () (fn () 123))
    (foo))
  123))


;; def

(ac-eval
 (assign def
   (annotate 'mac
     (fn (name parms . body)
       `(assign-fn ,name ,parms (fn ,parms ,@body))))))

(test-arc
 (( (def a () 123) (a) ) 123))


;; caar .. map1

(ac-eval
 (def caar (xs) (car (car xs)))
 (def cadr (xs) (car (cdr xs)))
 (def cddr (xs) (cdr (cdr xs)))

 (def no (x) (is x nil))

 (def acons (x) (is (type x) 'cons))

 (def atom (x) (no (acons x)))

 (def copylist (xs)
   (if (no xs) 
       nil 
       (cons (car xs) (copylist (cdr xs)))))

 (def idfn (x) x)

 (def isa (x y) (is (type x) y)))

(test-arc
 (( (car nil)       ) 'nil)
 (( (car '(1 2 3))  ) 1)
 (( (cdr nil)       ) 'nil)
 (( (cdr '(1 2 3))  ) (arc-list 2 3))
 (( (caar '((1 2))) ) 1)
 (( (cadr '(1 2 3)) ) 2)
 (( (cddr '(1 2 3)) ) (arc-list 3))

 (( (acons 3)    ) 'nil)
 (( (acons '(3)) ) 't)

 (( (atom 3)    ) 't)
 (( (atom '(3)) ) 'nil)
 
 (( (copylist '(1 2 3)) ) (arc-list 1 2 3))

 (( (idfn 123) ) 123)

 (( (map1 acons '(1 (2) 3 (4))) ) (arc-list 'nil 't 'nil 't)))


;; pair
; don't have optional arguments yet

(ac-eval
 ; def pair (xs (o f list))
 (def pair args
   ((fn (xs f)
      (if (no xs)
           nil
          (no (cdr xs))
           (list (list (car xs)))
           (cons (f (car xs) (cadr xs))
                 (pair (cddr xs) f))))
    (car args)
    (if (cdr args) (cadr args) list))))


(test-arc
 (( (pair '(1 2 3 4 5)) ) (toarc '((1 2) (3 4) (5))))
 (( (pair '(1 2 3 4 5) cons) ) (toarc '((1 . 2) (3 . 4) (5)))))


;; mac .. or

(ac-eval
 (assign mac (annotate 'mac
               (fn (name parms . body)
                 `(do (sref sig ',parms ',name)
                      (safeset ,name (annotate 'mac (fn ,parms ,@body)))))))

 (mac and args
   (if args
       (if (cdr args)
           `(if ,(car args) (and ,@(cdr args)))
           (car args))
       't))

 (def assoc (key al)
   (if (atom al)
        nil
       (and (acons (car al)) (is (caar al) key))
        (car al)
       (assoc key (cdr al))))

 (def alref (al key) (cadr (assoc key al)))

 (mac with (parms . body)
  `((fn ,(map1 car (pair parms))
     ,@body)
    ,@(map1 cadr (pair parms))))

 (mac let (var val . body)
   `(with (,var ,val) ,@body))

 (mac withs (parms . body)
   (if (no parms) 
       `(do ,@body)
       `(let ,(car parms) ,(cadr parms) 
          (withs ,(cddr parms) ,@body))))

 (mac rfn (name parms . body)
   `(let ,name nil
      (assign ,name (fn ,parms ,@body))))

 (mac afn (parms . body)
  `(let self nil
     (assign self (fn ,parms ,@body))))

 (mac compose args
   (let g (uniq)
     `(fn ,g
        ,((afn (fs)
            (if (cdr fs)
                (list (car fs) (self (cdr fs)))
                `(apply ,(if (car fs) (car fs) 'idfn) ,g)))
          args))))

 (mac complement (f)
   (let g (uniq)
     `(fn ,g (no (apply ,f ,g)))))

 (def rev (xs) 
   ((afn (xs acc)
      (if (no xs)
          acc
          (self (cdr xs) (cons (car xs) acc))))
    xs nil))

 (def isnt (x y) (no (is x y)))

 (mac w/uniq (names . body)
   (if (acons names)
       `(with ,(apply + nil (map1 (fn (n) (list n '(uniq)))
                              names))
          ,@body)
       `(let ,names (uniq) ,@body)))

 (mac or args
   (and args
        (w/uniq g
          `(let ,g ,(car args)
             (if ,g ,g (or ,@(cdr args))))))))

(test-arc
 (( (and)       ) 't)
 (( (and 3)     ) 3)
 (( (and 3 4)   ) 4)
 (( (and nil 4) ) 'nil)
 (( (and 3 4 5) ) 5)

 (( (alref '((a 1) (b 2)) 'b) ) 2)

 (( (with (a 1 b 2)
      (list a b)) )
  (arc-list 1 2))

 (( (let a 1 a) ) 1)

 (( (withs (a 1 b (+ a 2)) (list a b)) ) (arc-list 1 3))

 (( ((rfn foo (x) (if (no x) 0 (+ 1 (foo (cdr x))))) '(a b c)) ) 3)

 (( ((afn (x) (if (no x) 0 (+ 1 (self (cdr x))))) '(a b c)) ) 3)

 (( ((compose + car) '(3 4)) ) 3)

 (( ((complement acons) 3) ) 't)

 (( (rev '(1 2 3 4 5)) ) (arc-list 5 4 3 2 1))

 (( (isnt 5 5) ) 'nil)
 (( (isnt 4 6) ) 't)

 (( (w/uniq (a b)) ) 'nil)

 (( (or)           ) 'nil)
 (( (or 3)         ) 3)
 (( (or 3 4)       ) 3)
 (( (or nil 4)     ) 4)
 (( (or nil nil 5) ) 5))


;; alist

(ac-eval
 (def alist (x) (or (no x) (is (type x) 'cons))))

(test-arc
 (( (alist nil)      ) 't)
 (( (alist '(1 2 3)) ) 't)
 (( (alist 3)        ) 'nil))


;; in

(ac-eval
 (mac in (x . choices)
   (w/uniq g
     `(let ,g ,x
        (or ,@(map1 (fn (c) `(is ,g ,c)) choices))))))

(test-arc
 (( (in 'c 'a 'b 'c) ) 't)
 (( (in 'x 'a 'b 'c) ) 'nil))


;; iso

(ac-eval
 (def iso (x y)
   (or (is x y)
       (and (acons x) 
            (acons y) 
            (iso (car x) (car y)) 
            (iso (cdr x) (cdr y))))))

(test-arc
 (( (iso '(1 2 3) (list 1 2 3)) ) 't)
 (( (iso 'x 5)                  ) 'nil))


;; when

(ac-eval
 (mac when (test . body)
   `(if ,test (do ,@body))))

(test-arc
 (( (when 'yes 1 2 3 (+ 4 5)) ) 9))


;; unless

(ac-eval
 (mac unless (test . body)
   `(if (no ,test) (do ,@body))))

(test-arc
 (( (unless 'yes 1 2 3 (+ 4 5)) ) 'nil))


;; while

(ac-eval
 (mac while (test . body)
   (w/uniq (gf gp)
     `((rfn ,gf (,gp)
         (when ,gp ,@body (,gf ,test)))
       ,test))))

(test-arc
 (( (assign x 5)
    (while (> x 0)
      (assign x (- x 1)))
    x)
  0))


;; empty

(ac-eval
 (def empty (seq) 
   (or (no seq) 
       (and (or (is (type seq) 'string) (is (type seq) 'table))
            (is (len seq) 0)))))

(test-arc
 (( (empty '())      ) 't)
 (( (empty '(a b))   ) 'nil)
 (( (empty "")       ) 't)
 (( (empty "ab")     ) 'nil)
 (( (empty (table))  ) 't)
 (( (empty (let h (table)
             (sref h 4 'x)
             h)) )
  'nil)
 )


;; reclist

(ac-eval
 (def reclist (f xs)
   (and xs (or (f xs) (reclist f (cdr xs))))))


;; recstring

(ac-eval
 (def caddr (x) (car (cddr x)))

 ; def recstring (test s (o start 0))
 (def recstring args
   (with (test (car args)
          s    (cadr args)
          start (if (cddr args) (caddr args) 0))
     ((afn (i)
        (and (< i (len s))
             (or (test i)
                 (self (+ i 1)))))
      start))))

;; testify
; cheating, we don't have [] yet

(ac-eval
 (def testify (x)
   (if (isa x 'fn) x (fn (_) (is _ x)))))


;; some
; nor do we have ssyntax

(ac-eval
 (def some (test seq)
   (let f (testify test)
     (if (alist seq)
         (reclist   (compose f car) seq)
         (recstring (compose f seq) seq)))))

(test-arc
 (( (some 'x '(a b c)) ) 'nil)
 (( (some 'x '(a x c)) ) 't)

 (( (some atom '((a) (b) (c))) ) 'nil)
 (( (some atom '((a) b (c)))   ) 't)

 (( (some #\x "abc")   ) 'nil)
 (( (some #\x "abx")   ) 't)

 (( (some (fn (_) (> (coerce _ 'int) 99)) "abc")  ) 'nil)
 (( (some (fn (_) (> (coerce _ 'int) 99)) "abcd") ) 't))


;; all

(ac-eval
 (def all (test seq) 
   ((complement some) (complement (testify test)) seq)))
       
(test-arc
 (( (all 'x '(a x x)) ) 'nil)
 (( (all 'x '(x x x)) ) 't))


;; defvar

(ac-eval
 ; mac defvar (name get (o set))
 (mac defvar args
   (with (name (car args)
          get  (cadr args)
          set  (caddr args))
   `(ac-defvar ',name (list ,get ,set)))))

(test-arc
 (( (defvar x (fn () 3))
    x )
  3))


;; sym

(ac-eval
 (def sym (x) (coerce x 'sym)))

(test-arc
 (( (sym "foo") ) 'foo))


;; int

(ac-eval
 ; def int (x (o b 10))
 (def int args
   (with (x (car args)
          b (if (cdr args) (cadr args) 10))
     (coerce x 'int b))))

(test-arc
 (( (int "123") ) 123))


;; implicit

(ac-eval
 ; mac implicit (name (o param (make-parameter...)))
 (mac implicit args
   (with (name (car args)
          param (if (cdr args)
                     (cadr args)
                     (((racket-module 'scheme) 'make-parameter) nil)))
     (withs (get    (fn () (param))
             set    (fn (val) (param val))
             w/name (sym (+ "w/" name)))
       `(do (defvar ,name ',get ',set)
            (mac ,w/name (val . body)
              `(racket-parameterize ',',param ,val (fn () ,@body))))))))

;(test-arc
; (( (implicit foo 3)
;    foo )
;  3))

(test-arc
 (( (implicit foo)
    (assign foo 4)
    foo )
  4))

(test-arc
 (( (implicit foo)
    (w/foo 4 foo) )
  4))


; std ports

(ac-eval
 (eval `(do (implicit stdin  ,racket-stdin)
            (implicit stdout ,racket-stdout)
            (implicit stderr ,racket-stderr))))


;; on-err, details

(ac-def on-err (errf f)
  (with-handlers ((exn:fail? errf))
    (f)))

(ac-def details (c)
  (exn-message c))

(test-arc
  (( (on-err details (fn () (/ 1 0))) )
   "/: division by zero"))


;; pr

(ac-eval
 (def pr args
   (map1 disp args)
   (car args)))


;; do1

(ac-eval
 (mac do1 args
   (w/uniq g
     `(let ,g ,(car args)
        ,@(cdr args)
        ,g))))

(test-arc
 (( (do1 3 4) ) 3))


;; tostring

(ac-eval
 (mac w/outstring (var . body)
   `(let ,var (outstring) ,@body))

 (mac tostring body
   (w/uniq gv
    `(w/outstring ,gv
       (w/stdout ,gv ,@body)
       (inside ,gv)))))

(test-arc
 (( (tostring (pr 1 2 3)) ) "123"))


;; prn

(ac-eval
 (def prn args
   (do1 (apply pr args)
        (writec #\newline))))

(test-arc
 (( (tostring (prn "hi")) ) "hi\n"))


;; unit tests written in Arc

(ac-eval-file "equal-wrt-testing.arc")

(test-arc
  (( (equal-wrt-testing (on-err idfn (fn () (/ 1 0)))
                        (on-err idfn (fn () (/ 1 0)))) )
   't))

(ac-eval-file "test.arc")

(test-equal
 (let ((globals* (new-ac)))
   (tostringf
    (lambda ()
      (test-eval '( (catcherr (testis 4 5)) ) globals*))))
 "FAIL 4 => 4, not the expected result 5\n")


;; arc-test

(define-syntax arc-test
  (syntax-rules ()
    ((arc-test form ...)
     (add-test
      (lambda ()
        (let ((globals* (new-ac)))
          (test-eval '(form ...) globals*)))))))


;; erp

(ac-eval
 (mac erp (x)
   (w/uniq (gx)
     `(let ,gx ,x
        (w/stdout stderr
          (write ',x)
          (disp ": ")
          (write ,gx)
          (disp #\newline))
        ,gx))))


;; optional arguments, argument destructuring

; (fn (a (o b 3)) ...)
;   ->
; (fn args
;   (withs (a (car args)
;           b (if (cdr args) (cadr args)))
;     ...))    

(ac-eval
 (def ac-complex-getargs (a) (map1 car a))

 (def ac-complex-opt (var expr ra)
   (list (list var `(if (acons ,ra) (car ,ra) ,expr)))))

; todo more of this could be written in Arc now

(ac-def ac-complex-args (args ra)
  (cond ((no? args)
         'nil)
        ((true? (arc-isa args 'sym))
         (arc-list (arc-list args ra)))
        ((mpair? args)
         (let* ((a (arc-car args))
                (r (arc-cdr args))
                (x (if (and (mpair? a) (eq? (arc-car a) 'o))
                        ((g ac-complex-opt) (arc-cadr a) (arc-car (arc-cddr a)) ra)
                        ((g ac-complex-args) a (arc-list 'car ra)))))
           (arc-join x ((g ac-complex-args) (arc-cdr args) (arc-list 'cdr ra)))))
        (else
         (err "Can't understand fn arg list" args))))

(test-arc
 (( (ac-complex-args nil 'ra) ) 'nil)
 (( (ac-complex-args 'a  'ra) ) (toarc '((a ra)))))

(ac-eval
 (def ac-complex-fn (args body)
   (let ra 'ra
     `(fn ,ra
        (withs ,(apply join (ac-complex-args args ra))
          ,@body)))))

(test-arc
  (( ((eval (ac-complex-fn '(a (o b 3)) '((+ a b)))) 5) ) 8))

(ac-def ac-complex-args? (args)
  (cond ((no? args) 'nil)
        ((symbol? args) 'nil)
        ((and (mpair? args) (symbol? (mcar args)))
         ((g ac-complex-args?) (mcdr args)))
        (else 't)))

(test-arc
 (( (ac-complex-args? '(a b c))      ) 'nil)
 (( (ac-complex-args? '(a b . rest)) ) 'nil)
 (( (ac-complex-args? '(a (o b)))    ) 't))

(extend ac-fn (args body env)
  ((g ac-complex-args?) args)
  ((g ac) ((g ac-complex-fn) args body) env))

(test-arc
  (( ((fn (a (o b 3)) (+ a b)) 5)              ) 8)
  (( ((fn ((o a 3) . rest) (list a rest)))     ) (arc-list 3 'nil))
  (( ((fn ((o a 3) . rest) (list a rest)) 1)   ) (arc-list 1 'nil))
  (( ((fn ((o a 3) . rest) (list a rest)) 1 2) ) (arc-list 1 (arc-list 2)))
  (( ((fn (a (o b a)) b) 3)                    ) 3)
  (( ((fn ((a b c)) (+ a (* b c))) (list 1 2 3)) ) 7))


;; point

(ac-eval
 (mac point (name . body)
   (w/uniq (g p)
     `(ccc (fn (,g)
             (let ,name (fn ((o ,p)) (,g ,p))
               ,@body))))))

(arc-test
 (testis (point foo (foo 5) 6) 5))


;; catch

(ac-eval
 (mac catch body
   `(point throw ,@body)))

(arc-test
 (testis (catch 1 2 (throw 3) 4 5) 3))


;; <=, >=

(ac-eval
 (def <= args
   (or (no args)
       (no (cdr args))
       (and (no (> (car args) (cadr args)))
            (apply <= (cdr args)))))

 (def >= args
   (or (no args)
       (no (cdr args))
       (and (no (< (car args) (cadr args)))
            (apply >= (cdr args))))))

(arc-test
 (testis (<= 1 2 3) t))


;; whitec .. punc

(ac-eval
 (def whitec (c)
   (in c #\space #\newline #\tab #\return))

 (def nonwhite (c) (no (whitec c)))

 (def letter (c) (or (<= #\a c #\z) (<= #\A c #\Z)))

 (def digit (c) (<= #\0 c #\9))

 (def alphadig (c) (or (letter c) (digit c)))

 (def punc (c)
   (in c #\. #\, #\; #\: #\! #\?)))

(arc-test
 (testis (alphadig #\7) t)
 (testis (alphadig #\:) nil))


;; ret

(ac-eval
 (mac ret (var val . body)
   (w/uniq gval
     `(withs (,gval ,val ,var ,gval)
        ,@body
        ,gval))))

(arc-test
 (testis (ret a '(1 2 3)
         (sref a 'X 0)
         4)
       '(X 2 3)))


;; after

(ac-def protect (during after)
  (dynamic-wind (lambda () #t) during after))

(ac-eval
 (mac after (x . ys)
   `(protect (fn () ,x) (fn () ,@ys))))

(arc-test
 (testis (let a 1
         (catch (after (throw nil)
                       (assign a 2)))
         a)
       2))

;; headmatch

(ac-eval
 (def headmatch (pat seq (o start 0))
   (let p (len pat) 
     ((afn (i)      
        (or (is i p) 
            (and (is (pat i) (seq (+ i start)))
                 (self (+ i 1)))))
      0))))

(arc-test
 (testis (headmatch "abc" "abcdef")  t)
 (testis (headmatch "abc" "xxabc" 2) t)
 (testis (headmatch "abc" "xxabc")   nil))


;; len>

(ac-eval
 (def len> (x n) (> (len x) n)))

(arc-test
 (testis (len> "abc" 3) nil)
 (testis (len> "abc" 2) t))


;; begins

(ac-eval
 (def begins (seq pat (o start 0))
   (unless (len> pat (- (len seq) start))
     (headmatch pat seq start))))

(arc-test
 (testis (begins "abcdef" "ab") t)
 (testis (begins "abcdef" "cd") nil)
 (testis (begins "abcdef" "cd" 2) t)
 (testis (begins '(#\a #\b #\c #\d) '(#\b #\c) 1) t))

;; nthcdr

(ac-eval
 (def nthcdr (n xs)
   (if (no n)  xs
       (> n 0) (nthcdr (- n 1) (cdr xs))
               xs)))

(arc-test
 (testis (nthcdr 4 '(a b c d e)) '(e)))


;; xloop

(ac-eval
 (mac xloop (withses . body)
   (let w (pair withses)
     `((rfn next ,(map1 car w) ,@body) ,@(map1 cadr w)))))

(arc-test
 (tostring (xloop (x 0)
             (pr x)
             (if (< x 10)
                  (next (+ x 1)))))
 "012345678910")


;; accum
; don't have push yet

(ac-eval
 (mac accum (accfn . body)
   (w/uniq gacc
     `(withs (,gacc nil ,accfn (fn (_)
                                 (assign ,gacc (cons _ ,gacc))))
        ,@body
        (rev ,gacc)))))

(arc-test
 (testis (accum a (a 1) (a 2) (a 3))
       '(1 2 3)))

;; loop

(ac-eval
 (mac loop (start test update . body)
   (w/uniq (gfn gparm)
     `(do ,start
          ((rfn ,gfn (,gparm) 
             (if ,gparm
                 (do ,@body ,update (,gfn ,test))))
           ,test)))))

(arc-test
 (testis (accum a (let x nil (loop (assign x 0) (< x 5) (assign x (+ x 1))
                             (a x))))
       '(0 1 2 3 4)))


;; for

(ac-eval
 (mac for (v init max . body)
   (w/uniq (gi gm)
     `(with (,v nil ,gi ,init ,gm (+ ,max 1))
        (loop (assign ,v ,gi) (< ,v ,gm) (assign ,v (+ ,v 1))
          ,@body)))))

(arc-test
 (testis (accum a (for x 0 5 (a x))) '(0 1 2 3 4 5)))


;; repeat

(ac-eval
 (mac repeat (n . body)
   `(for ,(uniq) 1 ,n ,@body)))

(arc-test
 (testis (accum a (repeat 5 (a 7))) '(7 7 7 7 7)))


;; n-of
; don't have push yet

(ac-eval
 (mac n-of (n expr)
   (w/uniq ga
     `(let ,ga nil     
        (repeat ,n (assign ,ga (cons ,expr ,ga)))
        (rev ,ga)))))

(arc-test
 (testis (n-of 5 7) '(7 7 7 7 7)))


;; match

(ac-eval-file "match.arc")


;; case

(ac-eval
 (mac caselet (var expr . args)
   (let ex (afn (args)
             (if (no (cdr args)) 
                 (car args)
                 `(if (is ,var ',(car args))
                      ,(cadr args)
                      ,(self (cddr args)))))
     `(let ,var ,expr ,(ex args))))

 (mac case (expr . args)
   `(caselet ,(uniq) ,expr ,@args)))

(arc-test
 (testis (case 2 1 7 2 8 3 9) 8))


;; firstn

(ac-eval
 (def firstn (n xs)
   (if (no n)            xs
       (and (> n 0) xs)  (cons (car xs) (firstn (- n 1) (cdr xs)))
                         nil)))

(arc-test
 (testis (firstn 3 '(1 2 3 4 5)) '(1 2 3)))


;; tuples

(ac-eval
 (def tuples (xs (o n 2))
   (if (no xs)
       nil
       (cons (firstn n xs)
             (tuples (nthcdr n xs) n)))))

(arc-test
 (testis (tuples '(1 2 3 4 5 6 7) 3) '((1 2 3) (4 5 6) (7))))


;;; Arc reader

(ac-def racket-read-from-string (str)
  (toarc (read (open-input-string str))))

(test-arc
 (( (racket-read-from-string "123")   ) 123)
 (( (racket-read-from-string "(1 2)") ) (arc-list 1 2)))


; Test whether our reader parses something the same as Racket's
; reader.  (We may not always want to be the same, but it's good to at
; least know when we're different).

(ac-eval
 (mac test-parses-like-racket1 (parser input)
   `(test-match ,parser ,input ',(racket-read-from-string input)))

 (mac test-parses-like-racket (parser . inputs)
   `(do ,@(map1 (fn (example)
                  `(test-parses-like-racket1 ,parser ,example))
                inputs))))

(test-equal
 (let ((globals* (new-ac)))
   (tostringf
    (lambda ()
      (test-eval '( (test-parses-like-racket (do (one #\1) 1) "1") )
                 globals*))))
 "ok \"1\" (do (one #\\1) 1) => 1\n")


;; char

(ac-eval-file "hexdigit.arc")

(ac-eval
 (def match-digit (base)
   (case base
     2  (oneof #\0 #\1)
     8  (oneof #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
     10 (one digit)
     16 (one hexdigit)
        (err "unknown base" base))))

(arc-test
 (test-match (match-digit 2) "2" '<<parse-fail>>)
 (test-match (match-digit 10)
   "9" #\9
   "A" '<<parse-fail>>)
 (test-match (match-digit 16) "A" '#\A))

(ac-eval
 (def aschar (x)
   (coerce x 'char))

 (def intchar (b s)
   (aschar (int (coerce s 'string) b)))

 (def backslash-octal ()
   (intchar 8 (n-of 3 (match-digit 8)))))

(arc-test
 (test-match (backslash-octal)
   "0009"   #\nul
   "041abc" #\!
   "101"    #\A))

(ac-eval
 (def backslash-hex2 ()
   (one #\x)
   (must "a \\x must be followed by one or two hex digits"
         (intchar 16 (from1upto 2 (match-digit 16))))))

(arc-test
 (test-match (backslash-hex2) "x41" #\A))

(ac-eval
 (def backslash-hex4 ()
   (one #\u)
   (must "a \\u must be followed by one to four hex digits"
         (intchar 16 (from1upto 4 (match-digit 16))))))

(arc-test
 (test-match (backslash-hex4) "u0041" #\A))

(ac-eval
 (def backslash-hex8 ()
   (one #\U)
   (must "a \\U must be followed by one to eight hex digits"
         (intchar 16 (from1upto 8 (match-digit 16))))))

(arc-test
 (test-match (backslash-hex8) "U00000041" #\A))

(ac-eval
 (def named-char ()
   (mliteral
    "null"      #\nul
    "nul"       #\nul
    "backspace" #\backspace
    "tab"       #\tab
    "newline"   #\newline
    "linefeed"  #\newline
    "vtab"      #\vtab
    "page"      #\page
    "return"    #\return
    "space"     #\space
    "rubout"    #\rubout)))

(arc-test
 (test-match (named-char) "tab" #\tab))

(ac-eval
 (def char-constant ()
   (one #\#)
   (one #\\)
   (must "invalid character constant"
         (alt (do1 (named-char) (not (one letter)))
              (backslash-octal)
              (backslash-hex2)
              (backslash-hex4)
              (backslash-hex8)
              (do1 (next) (not (one letter)))))))

(arc-test
 (test-parses-like-racket (char-constant)
   "#\\null"
   "#\\space(a b c)"
   "#\\1"
   "#\\167"
   ;; "#\\x41" not implemented in PLT 4.2.1
   "#\\u0041"
   "#\\U00000041"
   "#\\λ")

 (test-match (char-constant)
   "#\\nulx" (makeerr "invalid character constant")))

(ac-eval
 (defalt parse-value-here (char-constant)))

(arc-test
 (test-match (parse-value-here) "#\\a" #\a))

(ac-eval
 (def match-line-comment ()
   (one #\;)
   (manyisnt #\newline)
   (optional (one #\newline))))

(arc-test
 (test-matchpos ";blah blah\nfoo" (match-line-comment) "foo"))

(ac-eval
 (def match-block-comment ()
   (mliteral "#|")
   (many (alt (match-block-comment)
              (do (not (mliteral "|#"))
                  (next))))
   (must "no closing |#" (mliteral "|#"))))

(arc-test
 (test-matchpos "#| a |#foo"           (match-block-comment) "foo")
 (test-matchpos "#| a #| b |# c |#foo" (match-block-comment) "foo")

 (test-match (match-block-comment)
   "#| abc"                 (makeerr "no closing |#")
   "#|#|#|#|abc|#|#|#xyzzy" (makeerr "no closing |#")))

(ac-eval
 (def match-expression-comment ()
   (mliteral "#;")
   (manyis whitec)
   (parse-value-here)))

(arc-test
 (test-matchpos "#;#\\a 2"   (match-expression-comment) " 2")
 (test-matchpos "#;  #\\a 2" (match-expression-comment) " 2"))

(ac-eval
 (def match-comment ()
   (alt (match-line-comment)
        (match-block-comment)
        (match-expression-comment)))

 (def skip-comments-and-whitespace ()
   (many (alt (one whitec)
              (match-comment)))))

(arc-test
 (test-matchpos "  ; foo\n  #|blah|# \n abc"
                (skip-comments-and-whitespace)
                "abc"))
  
(ac-eval
 (def parse-value ()
   (skip-comments-and-whitespace)
   (parse-value-here)))

(arc-test
 (test-match (parse-value) "#;#\\a  #\\b" #\b))

;; string

(ac-eval
 (def match-string-backslash-char ()
   (case (next)
     #\a #\u0007
     #\b #\backspace
     #\t #\tab
     #\n #\newline
     #\v #\vtab
     #\f #\u000C
     #\r #\return
     #\e #\u001B
     #\" #\"
     #\' #\'
     #\\ #\\
         (fail))))

(arc-test
 (test-match (match-string-backslash-char) "n" #\newline))

(ac-eval
 (def match-string-backslash-newline ()
   (optional (one #\return))
   (one #\newline)))

(arc-test
 (test-matchpos "\nfoo" (match-string-backslash-newline) "foo"))

(ac-eval
 (def match-string-backslash-sequence ()
   (one #\\)
   (when (at-end)
     (err "a backslash in a string must be followed by a character"))
   (must "invalid backslash sequence in string"
     (alt (match-string-backslash-char)
          (backslash-octal)
          (backslash-hex2)
          (backslash-hex4)
          (backslash-hex8)))))

(arc-test
 (test-match (match-string-backslash-sequence) "\\u0041" #\A))

(ac-eval
 (def parse-string ()
   (coerce
    (accum a
      (one #\")
      (many (alt (a (match-string-backslash-sequence))
                 (match-string-backslash-newline)
                 (a (onenot #\"))))
      (must "missing closing quote in string"
            (one #\")))
    'string)))

(arc-test
 (test-parses-like-racket (parse-string)
   "\"\""
   "\"abc\""
   "\"\\n\""
   "\"a\\u41!\""
   "\"abc\\ndef\""))

(ac-eval
 (defalt parse-value-here (parse-string)))

(arc-test
 (test-match (parse-value)
   "#\\a"    #\a
   "\"abc\"" "abc"))

;; quoted symbols like |abc| and \f\o\o

(ac-eval
 (def delimiter (c)
   (in c #\( #\) #\[ #\] #\{ #\} #\" #\, #\' #\` #\;))

 (def terminator (c)
   (or (delimiter c) (whitec c)))

 (def match-backslash-sym-char ()
   (one #\\)
   (must "backslash must be followed by a character"
         (next))))

(arc-test
 (test-match (match-backslash-sym-char) "\\A" #\A))


(ac-eval
 (mac do2 (a b . cs)
   `(do ,a (do1 ,b ,@cs)))

 (def match-bar-quote ()
   (do2 (one #\|)
        (manyisnt #\|)
        (must "missing closing |"
              (one #\|))))

 (def begins-quoted-sym (acc)
   (alt (acc (match-backslash-sym-char))
        (map1 acc (match-bar-quote))))

 (def in-quoted-sym (acc)
   (alt (acc (match-backslash-sym-char))
        (map1 acc (match-bar-quote))
        (acc (onenot terminator))))

 (def charssym (cs)
   (sym (coerce cs 'string)))

 (def parse-quoted-sym ()
   (charssym (accum a (begins-quoted-sym a)
                      (many (in-quoted-sym a)))))

 (defalt parse-value-here (parse-quoted-sym)))

(arc-test
 (test-match (parse-value)
   "|a|"    'a
   "\\a\\b" 'ab))

; let Racket do the hard work of figuring out whether the input
; can be parsed as a number

(ac-def racket-string->number (s)
  (let ((v (string->number s)))
    (if v v 'nil)))

(test-arc
 (( (racket-string->number "123") ) 123)
 (( (racket-string->number "abc") ) 'nil))

(ac-eval
 (def upto-terminator ()
   (coerce (many1is (complement terminator)) 'string))

 (def parse-unquoted-sym-or-number ()
   ; not sure what's the best way to handle defalt when
   ; order matters
   (not (at (parse-quoted-sym)))

   (alt (do (at (one #\#)
                (oneof #\b #\B #\o #\O #\x #\X))
            (let this (upto-terminator)
              (or (racket-string->number this)
                  (err "invalid number" this))))

        (do (not (at (oneof #\# #\\)))
            (let this (upto-terminator)
              (when (is this ".") (fail))
              (or (racket-string->number this)
                  (coerce this 'sym)))))))

(arc-test
 (test-match (parse-unquoted-sym-or-number)
   "123"   123
   "1+"    '1+
   "#x100" 256
   "#foo"  '<<parse-fail>>
   "."     '<<parse-fail>>))

(ac-eval
 (defalt parse-value-here (parse-unquoted-sym-or-number)))

(arc-test
 (test-parses-like-racket (parse-value)
   "123"
   "123("
   "123abc"
   "123abc("
   "\\3"
   "#x3"))


;; lists

(ac-eval

 ; )

 (def match-list-end ()
   (skip-comments-and-whitespace)
   (one #\)))

 ; . b

 (def match-dotted-end ()
   (skip-comments-and-whitespace)
   (one #\.)
   (at (one terminator))
   (must "a dotted list period must be followed by a single value and then the closing parenthesis"
         (do1 (parse-value)
              (match-list-end)))))

(arc-test
 (test-match (match-dotted-end)
   ". x )"     'x
   ".;foo\nx)" 'x))

(ac-eval

 ; can have (a b c d . e) or (a . b), but not (. a)

 ; note this needs the join that can produce dotted lists

 (def match-list-values ()
   (let xs (many1 (parse-value))
     (alt (join xs (match-dotted-end))
          (must "missing closing parenthesis"
                (match-list-end)
                xs))))

 (def parse-list ()
   (one #\()
   (skip-comments-and-whitespace)
   (alt (do (one #\)) nil)
        (match-list-values))))

(arc-test
 (test-match (parse-list)
   "()"        '()
   "(a b c)"   '(a b c)
   "(a b . c)" '(a b . c)))

(ac-eval
 (defalt parse-value-here (parse-list)))

(arc-test
 (test-match (parse-value) "(a (b c (d)) e)" '(a (b c (d)) e)))

(ac-eval
 (assign quote-abbreviations* (pair '(
  "'"  quote
  "`"  quasiquote
  ",@" unquote-splicing  ; need to match ,@ before ,
  ","  unquote)))

 (def parse-quote-abbreviation ((name expansion))
   (mliteral name)
   (skip-comments-and-whitespace)
        (must (string "a " name " must be followed by a value")
              `(,expansion ,(parse-value))))

 (def parse-quote-abbreviations ((o abbrevs quote-abbreviations*))
   (if (no abbrevs)
        (fail)
        (alt (parse-quote-abbreviation (car abbrevs))
             (parse-quote-abbreviations (cdr abbrevs))))))

(arc-test
 (test-match (parse-quote-abbreviations) "'a" ''a))

(ac-eval
 (defalt parse-value-here (parse-quote-abbreviations)))

(arc-test
 (test-parses-like-racket (parse-value) ",@foo"))

(ac-eval
 (assign brackets* (tuples '(
  "[" "]" square-bracket
  "{" "}" curly-bracket
 ) 3))
 
 (def match-bracket ((open close expansion))
   (mliteral open)
   (let l (many (parse-value))
     (must (string open " without closing " close)
           (mliteral close)
           `(,expansion ,@l))))

 (def match-brackets ((o brackets brackets*))
   (if (no brackets)
        (fail)
        (alt (match-bracket (car brackets))
             (match-brackets (cdr brackets))))))

(arc-test
 (test-match (match-brackets) "[a b c]" '(square-bracket a b c))
 (test-match (match-brackets) "{a b c}" '(curly-bracket a b c)))

(ac-eval
 (defalt parse-value-here (match-brackets)))


;; read1

; these are working on strings only

(ac-eval
 (def read1 (in)
   (match in (parse-value))))

(arc-test
 (testis (read1 "123") 123))


;; readall

(ac-eval
 (def readall (in)
   (match in
     (do1 (many (parse-value))
          (skip-comments-and-whitespace)
          (unless (at-end) (err "unable to parse"
                                (coerce (firstn 30 pos*) 'string)))))))

(arc-test
 (testis (readall "1 2 3") '(1 2 3)))


(ac-eval
 (def read-eval (in)
   (map1 eval (readall in))))

(arc-test
 (testis (read-eval "1 (+ 2 3) 4") '(1 5 4)))


(ac-def nil->racket-false (x)
  (if (no? x) #f x))

(ac-def scdr (x val)
  (set-mcdr! x val)
  val)


(define (readchars-list)
  (let ((c (read-char)))
    (if (eof-object? c)
         '()
         (cons c (readchars-list)))))

(define (filechars-list filename)
  (with-input-from-file filename
    (lambda ()
      (readchars-list))))

(ac-def aload (filename)
  ((g read-eval) (toarc (filechars-list filename))))

(ac-eval (aload "arc.arc"))

(when (test-atend)
  (run-tests))

(let ((globals* (new-ac)))
  (for-each (lambda (arg)
              ((g eval) (arc-list 'aload arg)))
            args)
  (when (run-repl)
    ((g eval) (arc-list 'toy-repl)))
  (void))
