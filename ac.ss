#lang scheme

(require scheme/mpair)
(require mzlib/defmacro)

(provide (all-defined-out))

;; note these are slow

(define (get arc varname)
  (namespace-variable-value
   varname
   #t
   #f
   (hash-ref arc 'racket-namespace*)))

(define (get-default arc varname default)
  (namespace-variable-value
    varname
    #t
    default
    (hash-ref arc 'racket-namespace*)))

(define (set arc varname value)
  (namespace-set-variable-value! varname value
    #t
    (hash-ref arc 'racket-namespace*)))


;; Arc compiler steps

; The compiler is built up in steps, so that simple cases can be
; tested before more complex cases are implemented.
;
; This isn't particularly important when the compiler is working (then
; we only really care that all the unit tests pass), but is useful
; when changing something fundamental early on in the compiler.

(define ac-build-steps '())

(define (add-ac-build-step step (source #f))
  (set! ac-build-steps (append ac-build-steps (list (list step source)))))

; Return a global variable namespace that includes the Arc runtime
; globals (car, +, etc.) and whatever Arc compiler globals that have
; been defined so far (ac, ac-literal?, etc.)  Note that a fresh copy
; of the compiler is created each time (new-arc) is called, including
; the compiler building steps defined so far.

(define (make-arc-racket-namespace)
  (let ((ns (make-base-empty-namespace)))
    (parameterize ((current-namespace ns))
      (namespace-require '(only racket/base #%app #%datum #%top))
      (namespace-require '(prefix racket- racket/base))
      (namespace-require '(prefix racket- racket/mpair))
      (namespace-require '(prefix racket- racket/tcp)))
    ns))

(define (new-arc arcdir (options (make-hash)))
  (let ((arc (make-hash)))
    (hash-set! arc 'racket-namespace* (make-arc-racket-namespace))
    (set arc 'arc* arc)
    (set arc 'arcdir* arcdir)
    (set arc 'ar-racket-eval racket-eval)
    (set arc 'ar-ail-load ail-load)
    (set arc 'ar-var
         (case-lambda
          ((name)
           (get arc name))
          ((name default)
           (get-default arc name (lambda () default)))))
    (set arc 'ar-assign
         (lambda (name value)
           (set arc name value)))
    (for-each (lambda (pair)
                (let ((step (car pair)))
                  (step arc)))
              (hash-ref options 'build-steps ac-build-steps))
    arc))

(define (new-arc2 arcdir)
  (new-arc arcdir
           (make-hash `((build-steps . ())))))


;; toarc

(define (toarc x)
  (cond ((pair? x)
         (mcons (toarc (car x))
                (toarc (cdr x))))
        ((null? x)
         'nil)
        ((string? x)
         (string-copy x))
        (else x)))


;; racket-eval

(define (racket-eval arc form)
  (parameterize ((compile-allow-set!-undefined #t))
    (eval form (hash-ref arc 'racket-namespace*))))


(define-syntax g
  (lambda (stx)
    (syntax-case stx ()
      ((g v)
       (with-syntax ((arc (datum->syntax #'v 'arc)))
         #'(get arc 'v))))))

(define (ac-def-fn arc name signature fn)
  (hash-set! (get arc 'sig) name (toarc signature))
  (set arc name fn))

(define-syntax ac-def
  (lambda (stx)
    (syntax-case stx ()
      ((ac-def name args body ...)
       (with-syntax ((arc (datum->syntax #'name 'arc)))
         #'(add-ac-build-step
             (lambda (arc)
               (ac-def-fn arc 'name 'args (lambda args body ...)))
             `(ac-def ,'name ,'args)))))))

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
   ((g ar-load)  (string-append (get arc 'arcdir*) "/ac.arc"))))


;; sref

(ac-def sref (com val ind)
  (cond ((hash? com)
         (if (eq? val 'nil)
             (hash-remove! com ind)
             (hash-set! com ind val)))
        ((string? com)
         (string-set! com ind val))
        ((mpair? com)
         (set-mcar! (mlist-tail com ind) val))
        (else
         ((g err) "Can't set reference" com ind val)))
  val)


;; on-err, details

(ac-def on-err (errf f)
  (with-handlers ((exn:fail? errf))
    (f)))

(ac-def details (c)
  (exn-message c))


;; parameters

(ac-def parameter (init)
  (make-parameter init))


(define (read-square-brackets ch port src line col pos)
  `(square-bracket ,@(read/recursive port #\[ #f)))

(define (bracket-readtable readtable)
  (make-readtable readtable #\[ 'terminating-macro read-square-brackets))

(add-ac-build-step
  (lambda (arc)
    (set arc 'arc-readtable* (bracket-readtable #f))))

(add-ac-build-step
 (lambda (arc)
   (ac-def-fn arc 'this-namespace '() (lambda () arc))))
