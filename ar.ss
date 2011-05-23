#lang scheme

(require scheme/mpair)

(provide hash new-ar
         noprint run-ar-tests)

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


(define ar-namespace*
  (hash '-                   -
        '/                   /
        '*                   *
        'cons                mcons
        'inside              get-output-string
        'instring            open-input-string
        'nil                 'nil
        'outstring           open-output-string
        'racket-stdin        current-input-port
        'racket-stdout       current-output-port
        'racket-stderr       current-error-port
        't                   't
        'uniq                gensym
        ))

(define (new-ar)
  (hash-copy ar-namespace*))
