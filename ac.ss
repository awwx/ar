#lang scheme

(provide (all-defined-out))

;; note these are slow

(define (get runtime varname)
  (namespace-variable-value
   varname
   #t
   #f
   runtime))

(define (get-default runtime varname default)
  (namespace-variable-value
    varname
    #t
    default
    runtime))

(define (set runtime varname value)
  (namespace-set-variable-value! varname value
    #t
    runtime))

(define (new-runtime)
  (let ((runtime (make-base-empty-namespace)))
    (parameterize ((current-namespace runtime))
      (namespace-require '(only racket/base #%app #%datum #%top))
      (namespace-require '(prefix racket- racket/base))
      (namespace-require '(prefix racket- racket/mpair))
      (namespace-require '(prefix racket- racket/tcp)))
    (set runtime 'arc* runtime)
    (set runtime 'ar-racket-eval racket-eval)
    (set runtime 'ar-ail-load ail-load)
    (set runtime 'ar-var
         (case-lambda
          ((name)
           (get runtime name))
          ((name default)
           (get-default runtime name (lambda () default)))))
    (set runtime 'ar-assign
         (lambda (name value)
           (set runtime name value)))
    runtime))

(define (new-arc arcdir)
  (let ((arc (new-runtime)))
    (set arc 'arcdir* arcdir)
    (ail-load arc (string-append (get arc 'arcdir*) "/ar.ail"))
    ((get arc 'ar-load)  (string-append (get arc 'arcdir*) "/ac.arc"))
    arc))


;; racket-eval

(define (racket-eval arc form)
  (parameterize ((compile-allow-set!-undefined #t))
    (eval form arc)))


;; ail-load

(define (ail-load arc filename)
  (call-with-input-file filename
    (lambda (in)
      (let loop ()
        (let ((form (read in)))
          (unless (eof-object? form)
            (racket-eval arc form)
            (loop))))))
  'nil)
