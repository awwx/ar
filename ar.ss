#lang scheme

(require scheme/mpair)

(provide pairwise ar-caris
         arc-cadr
         arc-car arc-cddr arc-cdr arc-join arc-list arc-map1
         deep-fromarc err hash list-fromarc new-ar
         no? noprint run-ar-tests tnil toarc true? write-to-string)

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
  (cond ((and (mpair? x) (eq? (mcar x) 'racket-list))
         (strict-deep-fromarc (mcar (mcdr x))))

        ;; nil in the car position isn't a list terminator, and so can
        ;; be left alone.
        ((mpair? x)
         (cons (let ((a (mcar x)))
                 (if (eq? a 'nil) 'nil (deep-fromarc a)))
               (let ((b (mcdr x)))
                 (if (eq? b 'nil) '() (deep-fromarc b)))))

        (else
         x)))

(define (strict-deep-fromarc x)
  (cond ((eq? x 'nil)
         '())
        ((mpair? x)
         (cons (strict-deep-fromarc (mcar x))
               (strict-deep-fromarc (mcdr x))))
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


(define ar-namespace*
  (hash '-                   -
        '/                   /
        '*                   *
        'car                 arc-car
        'caris               ar-caris
        'cdr                 arc-cdr
        'cons                mcons
        'err                 err
        'join                arc-join
        'inside              get-output-string
        'instring            open-input-string
        'is                  arc-is
        'list                arc-list
        'map1                arc-map1
        'nil                 'nil
        'outstring           open-output-string
        'r/list-toarc        r/list-toarc
        'racket-stdin        current-input-port
        'racket-stdout       current-output-port
        'racket-stderr       current-error-port
        't                   't
        'ar-toarc            toarc
        'uniq                gensym
        ))

(define (new-ar)
  (hash-copy ar-namespace*))
