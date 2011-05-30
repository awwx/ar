#lang scheme

(provide (all-defined-out))

(define runtime-get
  (case-lambda
   ((runtime varname)
    (namespace-variable-value varname #t #f runtime))

   ((runtime varname default)
    (namespace-variable-value varname #t (lambda () default) runtime))))

(define (runtime-set runtime varname value)
  (namespace-set-variable-value! varname value #t runtime))

(define (racket-eval arc form)
  (parameterize ((compile-allow-set!-undefined #t))
    (eval form arc)))

(define (new-runtime)
  (let ((runtime (make-base-empty-namespace)))
    (parameterize ((current-namespace runtime))
      (namespace-require '(only racket/base #%app #%datum #%top))
      (namespace-require '(prefix racket- racket/base))
      (namespace-require '(prefix racket- racket/mpair))
      (namespace-require '(prefix racket- racket/tcp)))
    (runtime-set runtime 'arc* runtime)
    (runtime-set runtime 'ar-racket-eval racket-eval)
    (runtime-set runtime 'ar-ail-load ail-load)
    (runtime-set runtime 'ar-var
         (case-lambda
          ((name)
           (runtime-get runtime name))
          ((name default)
           (runtime-get runtime name default))))
    (runtime-set runtime 'ar-assign
         (lambda (name value)
           (runtime-set runtime name value)))
    runtime))

(define (ail-load arc filename)
  (call-with-input-file filename
    (lambda (in)
      (let loop ()
        (let ((form (read in)))
          (unless (eof-object? form)
            (racket-eval arc form)
            (loop))))))
  'nil)

(define (new-arc arcdir)
  (let ((arc (new-runtime)))
    (runtime-set arc 'arcdir* arcdir)
    (ail-load arc (string-append (runtime-get arc 'arcdir*) "/ar.ail"))
    ((runtime-get arc 'ar-load)  (string-append (runtime-get arc 'arcdir*) "/ac.arc"))
    arc))
