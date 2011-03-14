#lang scheme

(require scheme/mpair)

(provide ar-apply ar-caris ar-funcall0 ar-funcall1 ar-funcall2
         ar-funcall3 ar-funcall4 ar-if ar-or ar-rep arc-apply arc-cadr
         arc-car arc-cddr arc-cdr arc-isa arc-join arc-list arc-map1
         arc-type deep-fromarc err hash new-ar no? noprint
         run-ar-tests tagged? tnil toarc true? write-to-string)

(define ar-tests* '())

; Wrap a module top level expression in noprint to keep Racket from
; printing out the value when the module is loaded.

(define-syntax noprint
  (syntax-rules ()
    ((noprint body ...)
     (((lambda ()
         body ...
         void))))))

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
    ((test expr expected)
     (set! ar-tests*
           (cons (lambda ()
                   (let ((r expr))
                     (if (equal? r expected)
                          (begin
                            (display "ok ")
                            (write 'expr)
                            (display " => ")
                            (write r)
                            (newline))
                          (begin
                            (display "bzzt! ")
                            (write 'expr)
                            (display " => ")
                            (write r)
                            (display " not ")
                            (write expected)
                            (newline)
                            (error "test failed")))))
                 ar-tests*)))))

(define (run-ar-tests)
  (display "run ar tests\n")
  (for-each (lambda (test) (test)) (reverse ar-tests*))
  (void))

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

(define (deep-fromarc x)
  (cond ;; nil in the car position isn't a list terminator, and so can
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
        ((parameter? x)     'parameter)
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
        (else               'unknown)))

(define (ar-tag type rep)
  (cond ((eqv? (arc-type rep) type) rep)
        (else (vector 'tagged type rep))))

(define (arc-isa x y)
  (arc-is (arc-type x) y))

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


(define (arc-readc (port (current-input-port)) (eof 'nil))
  (let ((c (read-char port)))
    (if (eof-object? c) eof c)))

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
        'ar-toarc            toarc
        'type                arc-type
        'uniq                gensym
        'writec              arc-writec
        ))

(define (new-ar)
  (hash-copy ar-namespace*))
