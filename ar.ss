#lang scheme

; Set to run-tests* to 'none, 'atend, or 'iteratively.
;
; 'iteratively runs all the tests at each step of building the
; compiler.  This is slow, but good at finding out which step
; broke an earlier test.

(define run-tests* 'atend)


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
         (f x port))))

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


; todo: (apply list 1 2 '(3 4))

(define (arc-apply fn args)
  (apply ar-apply fn (list-fromarc args)))


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

(define (iround x) (inexact->exact (round x)))

(define (ar-coerce x type . args)
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
                                         (arc-map1 (lambda (y) (ar-coerce y 'string)) x))))
                      (else      (err "Can't coerce" x type))))
    ((eq? x 'nil)   (case type
                      ((string)  "")
                      (else      (err "Can't coerce" x type))))
    ((symbol? x)    (case type 
                      ((string)  (symbol->string x))
                      (else      (err "Can't coerce" x type))))
    (#t             x)))

(test (ar-coerce #\A                   'int)       65)
(test (ar-coerce #\A                   'string)    "A")
(test (ar-coerce #\A                   'sym)       'A)
(test (ar-coerce 123                   'num)       123)
(test (ar-coerce 65                    'char)      #\A)
(test (ar-coerce 123                   'string)    "123")
(test (ar-coerce 128                   'string 16) "80")
(test (ar-coerce 13.4                  'int)       13)
(test (ar-coerce 65.0                  'char)      #\A)
(test (ar-coerce 14.5                  'string)    "14.5")
(test (ar-coerce "foo"                 'sym)       'foo)
(test (ar-coerce "foo"                 'cons)      (arc-list #\f #\o #\o))
(test (ar-coerce "123.5"               'num)       123.5)
(test (ar-coerce "123"                 'int)       123)
(test (ar-coerce (arc-list "a" 'b #\c) 'string)    "abc")
(test (ar-coerce 'nil                  'string)    "")


(define (char-or-string? x) (or (string? x) (char? x)))

(define (arc-list? x) (or (no? x) (mpair? x)))

(define (ar-+ . args)
  (cond ((null? args)
         0)
        ((char-or-string? (car args))
         (apply string-append 
                (map (lambda (a) (ar-coerce a 'string)) args)))
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


(define ar-namespace*
  (hash '+            ar-+
        'annotate     ar-tag
        'apply        arc-apply
        'car          arc-car
        'cdr          arc-cdr
        'caris        ar-caris
        'cons         mcons
        'err          err
        'join         arc-join
        'is           arc-is
        'list         arc-list
        'map1         arc-map1
        'mem          ar-mem
        'r/list-toarc r/list-toarc
        'type         arc-type
        'uniq         gensym
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

(define (add-tests tests)
  (set! arc-tests (append arc-tests tests))
  (when (eq? run-tests* 'iteratively)
    (run-tests)))

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
      (set! traces '())
      (let ((result (test-eval source globals*)))
        (unless (equal? result expected)
          (display "bzzt!\n")
          (display-trace)
          (display "result: ") (write result) (newline)
          (display "not: ") (write expected) (newline)
          (raise "failed"))))))

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
; of the compiler is created each time (new-ac) is called, with
; whatever compiler building steps have been defined so far.

(define (new-ac . args)
  (let ((globals* (new-ar)))
    (for-each (lambda (step)
                (step globals*))
              ac-build-steps)
    (add-to-hash globals* args)
    globals*))

(define-syntax ac-def
  (lambda (stx)
    (syntax-case stx ()
      ((ac-def name args body ...)
       (with-syntax ((globals* (datum->syntax #'name 'globals*)))
         #'(add-ac-build-step
            (lambda (globals*)
              (hash-set! globals* 'name
                (let ((name (lambda args body ...)))
                  name)))))))))

(define-syntax g
  (lambda (stx)
    (syntax-case stx ()
      ((g v)
       (with-syntax ((globals* (datum->syntax #'v 'globals*)))
         #'(hash-ref globals* 'v))))))


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
  (let ((message (string-append "reference to global variable \""
                                (symbol->string v)
                                "\" which hasn't been set")))
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
 "reference to global variable \"foo\" which hasn't been set")

(test-arc
 (( car ) arc-car))


;; call

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

; todo: an optional second argument specifying the global namespace to
; eval in.

(ac-def eval (x)
  (eval (deep-fromarc ((g ac) x 'nil))))

(test-arc
 (( (eval 3)        ) 3)
 (( (eval '(+ 1 2)) ) 3))


;; quasiquotation

; qq-expand takes an Arc list containing a quasiquotation expression
; (the x in `x), and returns an Arc list containing Arc code.  The Arc
; code, when evaled by Arc, will construct an Arc list, the
; expansion of the quasiquotation expression.

; Alan Bawden's quasiquotation expansion algorithm from
; "Quasiquotation in Lisp"
; http://repository.readscheme.org/ftp/papers/pepm99/bawden.pdf

(ac-def qq-expand-pair (x)
  (arc-list 'join
            ((g qq-expand-list) (mcar x))
            ((g qq-expand) (mcdr x))))

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

(ac-def ac-global-assign (a b env)
  (arc-list hash-set! globals* (arc-list 'quote a) ((g ac) b env)))

(ac-def ac-assign1 (a b1 env)
  (unless (symbol? a)
    (err "First arg to assign must be a symbol" a))
  (if (true? ((g ac-lex?) a env))
      (arc-list 'set! a ((g ac) b1 env))
      ((g ac-global-assign) a b1 env)))

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
 (( ((fn ()
       (assign x 123)
       x)) )
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

(define (ac-eval-impl forms)
  (add-ac-build-step
   (lambda (globals*)
     (trace-eval forms globals*))))

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

; todo a better name than ac-fn-foo
(add-ac-build-step
 (lambda (globals*)
   (hash-set! globals* 'ac-fn-foo
     (trace-eval
      '( (fn (args r/rest rest body env)
           `(lambda ,(join args r/rest)
              (let ((,rest (,r/list-toarc ,r/rest)))
                ,@(ac-body*x (join args (list rest)) body env)))) )
      globals*))))

(ac-def ac-fn-rest (args body env)
  ((g ac-fn-foo) ((g ac-args-without-rest) args)
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

(ac-def disp (x (port (current-output-port)))
  (printwith port display x))

(test-equal
 (let ((port (open-output-string))
       (globals* (new-ac)))
   (hash-set! globals* 'port port)
   (test-eval '( (disp '("a" b 3) port) ) globals*)
   (get-output-string port))
 "(a b 3)")

(test-equal
 (let ((port (open-output-string))
       (globals* (new-ac)))
   (parameterize ((current-output-port port))
     (test-eval '( (disp '("a" b 3)) ) globals*)
     (get-output-string port)))
 "(a b 3)")


;; defvar impl

(add-ac-build-step
 (lambda (globals*)
   (hash-set! globals* 'ac-defined-vars* (hash))))

(ac-def ac-defvar (v x)
  (hash-set! (g ac-defined-vars*) v x))

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

(extend ac-global-assign (a b env)
  ((g ac-defined-var) a)
  (arc-list ar-apply
            (ar-or (arc-cadr it) ((g ac-not-assignable) a))
            ((g ac) b env)))

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


;; stdin, stdout, stderr

(add-ac-build-step
  (lambda (globals*)
    ((g ac-defvar) 'stdin  (arc-list (lambda () (current-input-port))))
    ((g ac-defvar) 'stdout (arc-list (lambda () (current-output-port))))
    ((g ac-defvar) 'stderr (arc-list (lambda () (current-error-port))))))

(test-arc (( stdin ) (current-input-port)))


;; safeset

(ac-eval
 (assign safeset (annotate 'mac
                   (fn (var val)
                     `(do (if (bound ',var)
                              (do (disp "*** redefining " stderr)
                                  (disp ',var stderr)
                                  (disp #\newline stderr)))
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
         (err "Can't set reference" com ind val))))

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


;; def

(ac-eval
 (assign sig (table))

 (assign def (annotate 'mac
                (fn (name parms . body)
                  `(do (sref sig ',parms ',name)
                       (safeset ,name (fn ,parms ,@body)))))))

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

; next up is (def pair (xs (o f list))... but wait, we need optional arguments!


;; optional arguments

; (fn (a (o b 3)) x)
;   ->
; (fn args
;   ((fn (a b) x)
;    (car args)
;    (if (cdr args) (cadr args) 3))
    
(test-arc
 (( ((fn args
       ((fn (a b) (list a b))
        (car args)
        (if (cdr args) (car (cdr args)) 3)))
     1 2) )
  (arc-list 1 2))

 (( ((fn args
       ((fn (a b) (list a b))
        (car args)
        (if (cdr args) (car (cdr args)) 3)))
     1) )
  (arc-list 1 3)))

(ac-eval
 (def ac-complex-getargs (a) (map1 car a))

 (def ac-complex-opt (var expr ra)
   (list (list var `(if (acons ,ra) (car ,ra) ,expr)))))

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
   ((fn (ra)
      ((fn (expansion)
         `(fn ,ra
            ((fn ,(ac-complex-getargs expansion) ,@body)
             ,@(map1 cadr expansion))))
       (ac-complex-args args 'ra)))
    'ra)))

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
 (( (ac-complex-args? '(a (o b)))    ) 't)
 )

(extend ac-fn (args body env)
  ((g ac-complex-args?) args)
  ((g ac) ((g ac-complex-fn) args body) env))

(test-arc
  (( ((fn (a (o b 3)) (+ a b)) 5) ) 8))


;; pair

(ac-eval
 (def pair (xs (o f list))
   (if (no xs)
        nil
       (no (cdr xs))
        (list (list (car xs)))
       (cons (f (car xs) (cadr xs))
             (pair (cddr xs) f)))))

(test-arc
 (( (pair '(1 2 3 4 5)) ) (toarc '((1 2) (3 4) (5)))))


;;

(when (eq? run-tests* 'atend)
  (run-tests))
