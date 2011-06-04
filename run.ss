#lang scheme

(provide (all-defined-out))
(require scheme/path)

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
  (cond ((and (pair? form) (eq? (car form) 'ail-code))
         (for-each (lambda (expr)
                     (racket-eval runtime expr))
                   (cdr form)))
        ((and (pair? form) (eq? (car form) 'use))
         (for-each (lambda (item)
                     (use-load runtime item))
                   (cdr form)))
        (else
         (error "sorry, this primitive eval only knows how to do ail-code and use!" form))))

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

(define (asstring x)
  (cond ((string? x) x)
        ((symbol? x) (symbol->string x))
        (else        (error "can't convert to string" x))))

(define (assymbol x)
  (cond ((string? x) (string->symbol x))
        ((symbol? x) x)
        (else        (error "can't convert to symbol" x))))

(define (dirpart path)
  (let-values (((dir file must) (split-path path)))
    (path->string dir)))

(define (filepart path)
  (let ((r (file-name-from-path path)))
    (if r (path->string r) #f)))

(define (extension path)
  (let ((r (regexp-match #px"\\.([^\\.]+)$" (filepart path))))
    (if r (cadr r) #f)))

(define (default-arc path)
  (if (extension path)
       path
       (string-append path ".arc")))

(define (find file dirs)
  (cond ((absolute-path? file)
         (cond ((file-exists? file)
                file)
               ((and (not (extension file))
                     (file-exists? (string-append file ".arc")))
                (string-append file ".arc"))
               (else
                (error "not found" file))))
        ((eq? dirs 'nil)
         (error "not found" file))
        (else
         (let ((try (string-append (mcar dirs) "/" (default-arc file))))
           (if (file-exists? (string->path try))
                try
                (find file (mcdr dirs)))))))

(define (use-load runtime item)
  (let ((loaded* (runtime-get runtime 'loaded*))
        (usepath* (runtime-get runtime 'usepath*)))
    (unless (hash-ref loaded* (assymbol item) #f)
      (let ((path (find (asstring item) (usepath*))))
        (parameterize ((usepath* (mcons (dirpart path) (usepath*))))
          (load runtime #f path))
        (hash-set! loaded* (assymbol item) 't)))))
                     
(define (new-runtime libdir)
  (let ((runtime (make-base-empty-namespace)))
    (parameterize ((current-namespace runtime))
      (namespace-require '(only scheme/base #%app #%datum #%top))
      (namespace-require '(prefix racket- scheme/base))
      (namespace-require '(prefix racket- scheme/mpair)))
    (runtime-set runtime 'runtime* runtime)
    (runtime-set runtime 'usepath* (make-parameter (mcons libdir 'nil)))
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
                                     (use-load runtime item)))
    (runtime-set runtime 'loadin (lambda (in)
                                   (loadin runtime in)))
    (runtime-set runtime 'add-usepath
      (lambda (path)
        ((runtime-get runtime 'usepath*)
         (mcons (path->string (normalize-path path))
                ((runtime-get runtime 'usepath*))))))
    runtime))

(define (new-arc arcdir)
  (let ((arc (new-runtime arcdir)))
    (runtime-set arc 'arcdir* arcdir)
    arc))
