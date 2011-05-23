#lang scheme

(require scheme/mpair)

(provide hash new-ar noprint)

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
