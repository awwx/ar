#lang scheme

(require scheme/mpair)
(require mzlib/defmacro)

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


(define ac-build-steps '())

(define (add-ac-build-step step (source #f))
  (set! ac-build-steps (append ac-build-steps (list (list step source)))))

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

(define (new-arc arcdir (options (make-hash)))
  (let ((arc (new-runtime)))
    (set arc 'arcdir* arcdir)

    (for-each (lambda (pair)
                (let ((step (car pair)))
                  (step arc)))
              (hash-ref options 'build-steps ac-build-steps))
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


;; ac.ail

(add-ac-build-step
 (lambda (arc)
   (ail-load arc (string-append (get arc 'arcdir*) "/ar.ail"))
   ((get arc 'ar-load)  (string-append (get arc 'arcdir*) "/ac.arc"))))
