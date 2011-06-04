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

(define (loadin runtime in)
  (let ((form (read in)))
    (if (eof-object? form)
         'nil
         (begin (ail-code-eval runtime form)
                ((runtime-get runtime 'loadin) in)))))

(define (ail-load runtime filename)
  (call-with-input-file filename
    (lambda (in)
      (loadin runtime in))))

(define (full-path basedir filename)
  (path->string
   (path->complete-path
    filename
    (if (or (not basedir) (eqv? basedir 'nil))
         (current-directory)
         basedir))))

(define (load runtime basedir filename)
  ((runtime-get runtime 'load)
   (full-path (or basedir (current-directory)) filename)))

(define (use-load runtime basedir item)
  (let ((item (cond ((symbol? item) item)
                    ((string? item) (string->symbol item))
                    (else           (error "can't load-use" item))))
        (loaded* (runtime-get runtime 'loaded*)))
    (unless (hash-ref loaded* item #f)
      (load runtime basedir (string-append (symbol->string item) ".arc"))
      (hash-set! loaded* item 't))))
                     
(define (new-runtime libdir)
  (let ((runtime (make-base-empty-namespace)))
    (parameterize ((current-namespace runtime))
      (namespace-require '(only scheme/base #%app #%datum #%top))
      (namespace-require '(prefix racket- scheme/base))
      (namespace-require '(prefix racket- scheme/mpair)))
    (runtime-set runtime 'runtime* runtime)
    (runtime-set runtime 'loaded* (make-hash))
    (runtime-set runtime 'ar-racket-eval racket-eval)
    (runtime-set runtime 'ar-var
         (case-lambda
          ((name)
           (runtime-get runtime name))
          ((name default)
           (runtime-get runtime name default))))
    (runtime-set runtime 'ar-assign
         (lambda (name value)
           (runtime-set runtime name value)))
    (runtime-set runtime 'load (lambda (filename) (ail-load runtime filename)))
    (runtime-set runtime 'use-load (lambda (item)
                                     (use-load runtime libdir item)))
    (runtime-set runtime 'loadin (lambda (in)
                                   (loadin runtime in)))
    runtime))

(define (new-arc arcdir)
  (let ((arc (new-runtime arcdir)))
    (runtime-set arc 'arcdir* arcdir)
    (load arc arcdir "ac.arc")
    arc))
