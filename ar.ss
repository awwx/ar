(require scheme/mpair)

; Arc runtime functions

; While we don't care much if the Arc compiler is particularly fast or
; not, we do want the runtime functions to be as fast as possible.  So
; todo: can try putting them into a module to see if the promise that
; they won't be redefined does lead to a speed improvement, and/or
; other optiminzations.

; todo: some of these are utility functions only used by the compiler,
; and so could be separated out from the runtime functions.

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
         (display "bzzt ")
         (write 'expr)
         (display " => ")
         (write r)
         (display " not ")
         (write expected)
         (newline))))))
  
(define (read-from-string str)
  (let ((port (open-input-string str)))
    (let ((val (read port)))
      (close-input-port port)
      val)))

(define (write-to-string x)
  (let ((port (open-output-string)))
    (write x port)
    (close-output-port port)
    (get-output-string port)))

(define (list-toarc x)
  (cond ((pair? x)
         (mcons (car x) (list-toarc (cdr x))))
        ((null? x)
         'nil)
        (else x)))

(test (list-toarc '())        'nil)
(test (list-toarc '(1 2 3))   (mcons 1 (mcons 2 (mcons 3 'nil))))
(test (list-toarc '(1 2 . 3)) (mcons 1 (mcons 2 3)))

(define (ar-list . rest)
  (list-toarc rest))

(test (ar-list 1 2 3) (mcons 1 (mcons 2 (mcons 3 'nil))))

(define (list-fromarc x)
  (cond ((mpair? x)
         (cons (mcar x) (list-fromarc (mcdr x))))
        ((eq? x 'nil)
         '())
        (else x)))

(test (list-fromarc 'nil)          '())
(test (list-fromarc (ar-list 1 2)) '(1 2))
(test (list-fromarc (mcons 1 2))   '(1 . 2))

(define (deep-toarc x)
  (cond ((pair? x)
         (mcons (deep-toarc (car x))
                (deep-toarc (cdr x))))
        ((null? x)
         'nil)
        ((string? x)
         (string-copy x))
        (else x)))

(define-struct tunnel (x))

(define (deep-fromarc x)
  (cond ((tunnel? x)
         (tunnel-x x))
        ((eq? x 'nil)
         '())
        ;; While I usually dislike being pedantic, this turned out to
        ;; be helpful in tracking down bugs.
        ;; Can have a more relaxed version for the runtime once the
        ;; compiler is working.
        ((or (pair? x) (null? x))
         (error "oops, a Racket list snuck in here!"))
        ((mpair? x)
         (cons (deep-fromarc (mcar x))
               (deep-fromarc (mcdr x))))
        (else
         x)))

(define (no? x)
  (eq? x 'nil))

(define (true? x)
  (not (no? x)))

(define (ar-car x)
  (if (eq? x 'nil)
       'nil
       (mcar x)))

(test (equal? (ar-car 'nil)            'nil)
      (equal? (ar-car (ar-list 1 2 3)) 1))

(define (ar-cdr x)
  (if (eq? x 'nil)
       'nil
       (mcdr x)))

(test (ar-cdr 'nil)            'nil)
(test (ar-cdr (ar-list 1 2 3)) (ar-list 2 3))

(define (ar-cadr x)
  (ar-car (ar-cdr x)))

(test (ar-cadr (ar-list 1 2 3)) 2)

(define (ar-cddr x)
  (ar-cdr (ar-cdr x)))

(define (tnil x) (if x 't 'nil))

(define (ar-map1 f xs)
  (if (no? xs) 
      'nil
      (mcons (f (ar-car xs)) (ar-map1 f (ar-cdr xs)))))

(test (ar-map1 (lambda (x) (tnil (odd? x))) (ar-list 1 2 3 4)) (ar-list 't 'nil 't 'nil))

(define (ar-join . args)
  (list-toarc (apply append (map list-fromarc args))))

(test (ar-join) 'nil)
(test (ar-join (ar-list)) 'nil)
(test (ar-join (ar-list 1 2)) (ar-list 1 2))
(test (ar-join (ar-list) (ar-list)) 'nil)
(test (ar-join (ar-list 1 2) (ar-list)) (ar-list 1 2))
(test (ar-join (ar-list) (ar-list 1 2)) (ar-list 1 2))
(test (ar-join (ar-list 1 2) (ar-list 3 4)) (ar-list 1 2 3 4))
(test (ar-join (ar-list 1 2) 3) (mcons 1 (mcons 2 3)))
(test (ar-join (ar-list 1) (ar-list 2) (ar-list 3)) (ar-list 1 2 3))

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

(define (ar-is . args)
  (pairwise ar-is2 args))

(test (ar-is)       't)
(test (ar-is 4)     't)
(test (ar-is 3 4)   'nil)
(test (ar-is 4 4)   't)
(test (ar-is 4 4 5) 'nil)
(test (ar-is 4 4 4) 't)

(define (ar-caris x val)
  (tnil (and (mpair? x)
             (true? (ar-is (ar-car x) val)))))

(test (ar-caris 4 'x) 'nil)
(test (ar-caris (ar-list 'y 'z) 'x) 'nil)
(test (ar-caris (ar-list 'x 'y) 'x) 't)

(define (tag type rep)
  (cond ((eqv? (ar-type rep) type) rep)
        (else (vector 'tagged type rep))))

(define (tagged? x)
  (and (vector? x) (eq? (vector-ref x 0) 'tagged)))

(define (exint? x) (and (integer? x) (exact? x)))

(define (ar-type x)
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

(define (ar-isa x y)
  (ar-is (ar-type x) y))

(define (ar-testify x)
  (ar-if (ar-isa x 'fn) x (lambda (_) (ar-is _ x))))

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
(test (ar-apply (ar-list 1 2 3) 1) 2)
(test (ar-apply "abcde" 2) #\c)
(test (ar-apply (hash 'a 1 'b 2) 'b) 2)
(test (ar-apply (hash 'a 1 'b 2) 'x) 'nil)
(test (ar-apply (hash 'a 1 'b 2) 'x 3) 3)

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


(define ar-namespace*
  (hash 'cadr  ar-cadr
        'cddr  ar-cddr
        'caris ar-caris
        'cons  mcons
        'err   err
        'join  ar-join
        'list  ar-list
        'map1  ar-map1
        'mem   ar-mem
        ))

(define (new-ar)
  (hash-copy ar-namespace*))


;; testing the compiler

; For a general purpose tracing facility I'd use a parameter, but this
; is good enough for here.

(define traces '())

(define (trace . args)
  (set! traces (append traces (list args))))

; When looking at trace output, it's helpful to know that Racket displays
; an Arc list like (1 2 3) as {1 2 3 . nil}

(define (display-trace)
  (map (lambda (trace)
         (display (car trace))
         (display ": ")
         (map (lambda (arg)
                (write arg)
                (display " "))
              (cdr trace))
         (newline))
         traces))
       
; Trace each step of reading, compiling, eval'ing an Arc program, with
; all the converting lists back and forth.

(define (trace-eval arc-program-string globals)
  (trace "program (string)"  arc-program-string)
  (let ((r/source (read-from-string arc-program-string)))
    (trace "program (racket)" r/source)
    (let ((a/source (deep-toarc r/source)))
      (trace "program (arc)" a/source)
      (let ((a/compiled ((hash-ref globals 'ac) a/source 'nil)))
        (trace "compiled (arc)" a/compiled)
        (let ((r/compiled (deep-fromarc a/compiled)))
          (trace "compiled (racket)" r/compiled)
          (let ((result (eval r/compiled)))
            (trace "result" result)
            result))))))

; If a test fails, display all the steps.

(define (test-eval arc-program-string globals*)
  (set! traces '())
  (with-handlers ((exn:fail? (lambda (c)
                               (display-trace)
                               (raise c))))
    (trace-eval arc-program-string globals*)))

; Test that compiling an eval'ing an Arc program produces the
; expected Racket result.

(define-syntax make-arc-test
  (syntax-rules ()
    ((make-arc-test arc-program-source globals* expected)
     (lambda ()
       (set! traces '())
       (let ((result (test-eval arc-program-source globals*)))
         (unless (equal? result expected)
           (display "bzzt!\n")
           (display-trace)
           (display "result: ") (write result) (newline)
           (display "not: ") (write expected) (newline)
           (raise "failed")))))))

; A test adds itself to the list of tests, and also runs all the tests
; again (including itself) from the beginning.  This allows us e.g. to
; add things like optimizations to the compiler, and all the previous
; tests will be run again to ensure that they still work.

(define arc-tests '())

(define (run-tests)
  (map (lambda (test)
         (test))
       arc-tests))

(define (add-tests tests)
  (set! arc-tests (append arc-tests tests))
  (run-tests))

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

(define (test-expect-error-impl source thunk expected-error-message)
  (let ((actual-error
         (with-handlers ((exn:fail? (lambda (c) (exn-message c))))
           (thunk)
           'oops-no-error-after-all)))
    (unless (equal? (substring actual-error 0 (string-length expected-error-message))
                    expected-error-message)
      (error actual-error))))

(define-syntax test-expect-error
  (syntax-rules ()
    ((test-expect-error expr expected-error-message)
     (test-expect-error-impl 'expr (lambda () expr) expected-error-message))))

(define-syntax test-arc
  (syntax-rules ()
    ((test-arc (arc-program-source globals* expected) ...)
     (add-tests (list (make-arc-test arc-program-source globals* expected)
                       ...)))))

  
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
                (lambda args body ...)))))))))

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

(define (ac-extend-impl test body)
  (add-ac-build-step
   (lambda (globals*)
     (let ((previous-ac (g ac)))
       (hash-set! globals* 'ac
         (lambda (s env)
           (if (true? (test globals* s env))
                (body globals* s env)
                (previous-ac s env))))))))

(define-syntax ac-extend
  (lambda (stx)
    (syntax-case stx ()
      ((ac-extend (s env) test body ...)
       (with-syntax ((globals* (datum->syntax #'s 'globals*)))
         #'(ac-extend-impl
            (lambda (globals* s env) test)
            (lambda (globals* s env) body ...)))))))


;; literal

(ac-def ac-literal? (x)
  (tnil (or (char? x)
            (string? x)
            (number? x))))

(ac-extend (s env)
  ((g ac-literal?) s)
  s)

(test-arc ("123"     (new-ac) 123)
          ("#\\a"    (new-ac) #\a)
          ("\"abc\"" (new-ac) "abc"))
; it's alive!


;; nil

; Need to tunnel nil through deep-fromarc, since otherwise it will be
; converted to a Racket ().

(define (ac-nil globals*)
  ((g list) 'quote (make-tunnel 'nil)))

(ac-extend (s env)
  (tnil (eq? s 'nil))
  (ac-nil globals*))

(test-arc ("nil" (new-ac) 'nil))


;; variables

(ac-def ac-lex? (v env)
  ((g mem) v env))

(test-t (let ((globals* (new-ac)))
          ((g ac-lex?)
           'y
           (ar-list 'x 'y 'z))))

(test-nil (let ((globals* (new-ac)))
            ((g ac-lex?)
             'w
             (ar-list 'x 'y 'z))))

(define (global-ref-err globals* v)
  (let ((message (string-append "reference to global variable \""
                                (symbol->string v)
                                "\" which hasn't been set yet")))
    (lambda ()
      ((g err) message))))

; An Arc global variable reference to "foo" such as in (+ foo 3) compiles into
; the Racket expression
; (#<procedure:hash-ref> #<hash:globals*> 'foo #<procedure:global-ref-err>)

(ac-def ac-global (v)
  ((g list)
   hash-ref
   globals*
   ((g list) 'quote v)
   (global-ref-err globals* v)))

(ac-def ac-var-ref (s env)
  (ar-if ((g ac-lex?) s env)
         s
         ((g ac-global) s)))

(ac-extend (s env)
  (ar-and (tnil (not (no? s))) (tnil (symbol? s)))
  ((g ac-var-ref) s env))

(test-expect-error
 ; todo: should clear trace here
 (trace-eval "foo" (new-ac))
 "reference to global variable \"foo\" which hasn't been set yet")

(test-arc
 ("foo" (new-ac 'foo 123) 123))


;; ac-call

(ac-def ac-call (fn args env)
  (mcons ar-apply
         (mcons ((g ac) fn env)
                ((g map1) (lambda (x)
                            ((g ac) x env)) args))))

(ac-extend (s env)
  (tnil (mpair? s))
  ((g ac-call) (ar-car s) (ar-cdr s) env))

(test-arc
 ("(+)"           (new-ac '+ +) 0)
 ("(+ 1 2)"       (new-ac '+ +) 3)
 ("(+ 1 2 3)"     (new-ac '+ +) 6)
 ("(+ 1 2 3 4)"   (new-ac '+ +) 10)
 ("(+ 1 2 3 4 5)" (new-ac '+ +) 15))


;; quote

(ac-extend (s env)
  ((g caris) s 'quote)
  ((g list) 'quote (make-tunnel ((g cadr) s))))

(test-arc ("'abc"     (new-ac) 'abc)
          ("'()"      (new-ac) 'nil)
          ("'(a)"     (new-ac) (ar-list 'a))
          ("'(nil)"   (new-ac) (ar-list 'nil))
          ("'(a . b)" (new-ac) (mcons 'a 'b)))


;; ac-fn

(ac-def ac-body (body env)
  ((g map1) (lambda (x) ((g ac) x env)) body))

(test-equal
 (let ((globals* (new-ac)))
  ((g ac-body)
   (ar-list 1 2 3)
   'nil))
 (ar-list 1 2 3))

(ac-def ac-body* (body env)
  (if (no? body)
      ((g list) (ac-nil globals*))
      ((g ac-body) body env)))

(ac-def ac-arglist (a)
  (cond ((no? a) 'nil)
        ((symbol? a) (ar-list a))
        ((and (symbol? (mcdr a)) (not (no? (mcdr a))))
         (ar-list (mcar a) (mcdr a)))
        (else (mcons (mcar a) ((g ac-arglist) (mcdr a))))))

(ac-def ac-fn (args body env)
  ((g cons)
   'lambda
   ((g cons)
    args
    ((g ac-body*)
     body
     ((g join)
      ((g ac-arglist) args)
      env)))))

(ac-extend (s env)
  ((g caris) s 'fn)
  ((g ac-fn) ((g cadr) s) ((g cddr) s) env))

(test-arc
 ("((fn ()))"                  (new-ac)      'nil)
 ("((fn () 3))"                (new-ac)      3)
 ("((fn (a) a) 3)"             (new-ac)      3)
 ("((fn (a b) b) 1 2)"         (new-ac)      2)
 ("((fn (a b) (+ a b 3)) 1 2)" (new-ac '+ +) 6))
