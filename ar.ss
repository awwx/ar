#lang scheme

(require scheme/mpair)

(provide arc-list
         hash list-fromarc new-ar
         noprint r/list-toarc run-ar-tests
         write-to-string)

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


(define ar-namespace*
  (hash '-                   -
        '/                   /
        '*                   *
        'cons                mcons
        'inside              get-output-string
        'instring            open-input-string
        'list                arc-list
        'nil                 'nil
        'outstring           open-output-string
        'r/list-toarc        r/list-toarc
        'racket-stdin        current-input-port
        'racket-stdout       current-output-port
        'racket-stderr       current-error-port
        't                   't
        'uniq                gensym
        ))

(define (new-ar)
  (hash-copy ar-namespace*))
