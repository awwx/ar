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

(define (racket-eval runtime form)
  (parameterize ((compile-allow-set!-undefined #t))
    (eval form runtime)))

(define (ail-code-eval runtime form)
  (if (and (pair? form) (eq? (car form) 'ail-code))
       (for-each (lambda (expr)
                   (racket-eval runtime expr))
                 (cdr form))
       (error "sorry, this primitive eval only knows how to do ail-code" form)))

(define (ail-load runtime filename)
  (call-with-input-file filename
    (lambda (in)
      (let loop ()
        (let ((form (read in)))
          (unless (eof-object? form)
            (ail-code-eval runtime form)
            (loop))))))
  'nil)

(define (load runtime basedir filename)
  (let ((path (path->string
               (path->complete-path filename
                                    (or basedir (current-directory))))))
    ((or (runtime-get runtime 'load #f)
         (runtime-get runtime 'ar-load #f)
         (let ((ail-load (runtime-get runtime 'ar-ail-load #f)))
           (and ail-load
                (lambda (path)
                  (ail-load runtime path))))
         (error "unable to find a loader in the runtime" filename))
     path)))

(define (new-runtime)
  (let ((runtime (make-base-empty-namespace)))
    (parameterize ((current-namespace runtime))
      (namespace-require '(only scheme/base #%app #%datum #%top))
      (namespace-require '(prefix racket- scheme/base))
      (namespace-require '(prefix racket- scheme/mpair)))
    (runtime-set runtime 'runtime* runtime)
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

(define (new-arc arcdir)
  (let ((arc (new-runtime)))
    (runtime-set arc 'arcdir* arcdir)
    (load arc arcdir "ar.arc")
    (load arc arcdir "ac.arc")
    arc))
