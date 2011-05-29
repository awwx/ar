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
         (lambda (name)
           (get arc name)))
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

(define-syntax ac-def-sig
  (lambda (stx)
    (syntax-case stx ()
      ((ac-def-sig name racket-args arc-signature body ...)
       (with-syntax ((arc (datum->syntax #'name 'arc)))
         #'(add-ac-build-step
            (lambda (arc)
              (ac-def-fn arc 'name 'arc-signature
                (lambda racket-args body ...)))
            `(ac-def-sig ,'name ,'racket-args)))))))


;; ar-def

(defmacro ar-def (name signature . body)
  `(add-ac-build-step
    (lambda (arc)
      (hash-set! (get arc 'sig) ',name (toarc ',signature))
      ,@(map (lambda (form)
               `(racket-eval arc ',form))
             body))
    '(ar-def ,name)))


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


;; r/list-toarc

(define (r/list-toarc x)
  (cond ((pair? x)
         (mcons (car x) (r/list-toarc (cdr x))))
        ((null? x)
         'nil)
        (else x)))


;; list

(define (arc-list . rest)
  (r/list-toarc rest))


;; ar-deep-fromarc

(define (ar-deep-fromarc x)
  (cond ;; nil in the car position isn't a list terminator, and so can
        ;; be left alone.
        ((mpair? x)
         (cons (let ((a (mcar x)))
                 (if (eq? a 'nil) 'nil (ar-deep-fromarc a)))
               (let ((b (mcdr x)))
                 (if (eq? b 'nil) '() (ar-deep-fromarc b)))))

        (else
         x)))


; Extending the Arc compiler

(defmacro extend (name args test . body)
  `(add-ac-build-step
     (lambda (arc)
       ((g ar-extend-impl) ',name
        (lambda (arc . ,args) ,test)
        (lambda (arc . ,args) ,@body)))
     '(extend ,name ,args ,test)))


;; fn

; Rest args, optional args, and arg list destructuring are implemented
; later.

(ar-def ac-fn (args body env)
  (racket-define (ac-fn args body env)
    (racket-if (ar-true (ac-dotted-list? args))
      (ac-fn-rest args body env)
      (racket-mcons (racket-quote racket-lambda)
                    (racket-mcons (ar-tunnel (ar-list-fromarc args))
                                  (ac-body*x args body env))))))

(extend ac (s env)
  ((g caris) s 'fn)
  ((g ac-fn) ((g cadr) s) ((g cddr) s) env))


;; arc-eval

(define (arc-eval arc form)
  (racket-eval arc ((g ar-deep-fromarc) ((get arc 'ac) form 'nil))))


;; quasiquotation

; qq-expand takes an Arc list containing a quasiquotation expression
; (the x in `x), and returns an Arc list containing Arc code.  The Arc
; code, when evaled by Arc, will construct an Arc list, the
; expansion of the quasiquotation expression.

; This implementation is Alan Bawden's quasiquotation expansion
; algorithm from "Quasiquotation in Lisp"
; http://repository.readscheme.org/ftp/papers/pepm99/bawden.pdf

; You can redefine qq-expand in Arc if you want to implement a
; different expansion algorithm.

(ac-def qq-expand (x)
  (cond (((g ar-true) ((g caris) x 'unquote))
         ((g cadr) x))
        (((g ar-true) ((g caris) x 'unquote-splicing))
         (error "illegal use of ,@ in non-list quasiquote expansion"))
        (((g ar-true) ((g caris) x 'quasiquote))
         ((g qq-expand) ((g qq-expand) ((g cadr) x))))
        ((mpair? x)
         ((g qq-expand-pair) x))
        (else
         (arc-list 'quote x))))

(ac-def qq-expand-pair (x)
  (arc-list 'join
            ((g qq-expand-list) (mcar x))
            ((g qq-expand) (mcdr x))))

(ac-def qq-expand-list (x)
  (cond (((g ar-true) ((g caris) x 'unquote))
         (arc-list 'list ((g cadr) x)))
        (((g ar-true) ((g caris) x 'unquote-splicing))
         ((g cadr) x))
        (((g ar-true) ((g caris) x 'quasiquote))
         ((g qq-expand-list) ((g qq-expand) ((g cadr) x))))
        ((mpair? x)
         (arc-list 'list ((g qq-expand-pair) x)))
        (else
         (arc-list 'quote (list x)))))

(extend ac (s env)
  ((g caris) s 'quasiquote)
  (let ((expansion ((g qq-expand) ((g cadr) s))))
    ((g ac) expansion env)))


;; if

(ac-def ac-if (args env)
  (cond (((g ar-no) args)
         '(racket-quote nil))
        (((g ar-no) ((g cdr) args))
         ((g ac) ((g car) args) env))
        (else
         (arc-list 'racket-if
                   (arc-list (g ar-true) ((g ac) ((g car) args) env))
                   ((g ac) ((g cadr) args) env)
                   ((g ac-if) ((g cddr) args) env)))))

(extend ac (s env)
  ((g caris) s 'if)
  ((g ac-if) ((g cdr) s) env))


;; assign

(ac-def ac-global-assign (a b)
  (arc-list 'racket-set! a b))

(ac-def ac-assign1 (a b1 env)
  (unless (symbol? a)
    ((g err) "First arg to assign must be a symbol" a))
  (let ((result (gensym)))
    (arc-list 'racket-let
              (arc-list (arc-list result ((g ac) b1 env)))
              (if ((g ar-true) ((g ac-lex?) a env))
                   (arc-list 'racket-set! a result)
                   ((g ac-global-assign) a result))
              result)))

(ac-def ac-assignn (x env)
  (if ((g ar-no) x)
      'nil
      ;; todo: Arc 3.1 calls ac-macex here
      (mcons ((g ac-assign1) ((g car) x) ((g cadr) x) env)
             ((g ac-assignn) ((g cddr) x) env))))

(ac-def ac-assign (x env)
  (mcons 'racket-begin
         ((g ac-assignn) x env)))

(extend ac (s env)
  ((g caris) s 'assign)
  ((g ac-assign) ((g cdr) s) env))


;; macro

(ac-def ac-macro? (fn)
  (cond ((eq? ((g type) fn) 'mac)
         ((g rep) fn))
        ((symbol? fn)
         (let ((v (get-default arc fn (lambda () 'nil))))
           (if (eq? ((g type) v) 'mac)
                ((g rep) v)
                'nil)))
        (else
         'nil)))

(ac-def ac-mac-call (m args env)
  (let ((x1 ((g apply) m args)))
    (let ((x2 ((g ac) x1 env)))
      x2)))

(extend ac-call (fn args env)
  (if ((g ar-true) ((g ac-lex?) fn env))
       'nil
       ((g ac-macro?) fn))
  ((g ac-mac-call) ((g ac-macro?) fn) args env))


;; fn rest arg

(ac-def ac-rest-param (x)
  (cond ((and (symbol? x) (not (eq? x 'nil)))
         x)
        ((mpair? x)
         ((g ac-rest-param) (mcdr x)))
        (else
         ((g err) "not a dotted list"))))

(ac-def ac-args-without-rest (x)
  (cond ((mpair? x)
         ((g join) (arc-list ((g car) x)) ((g ac-args-without-rest) (mcdr x))))
        (else
         'nil)))

; The implementation of "ac-fn-rest" turned out to be a lot easier to
; write in Arc.

(add-ac-build-step
 (lambda (arc)
   (set arc 'ac-fn-rest-impl
     (arc-eval arc
      ((g ar-toarc)
       '(fn (args r/rest rest body env)
          `(racket-lambda ,(join args r/rest)
             (racket-let ((,rest (,ar-r/list-toarc ,r/rest)))
               ,@(ac-body*x (join args (list rest)) body env)))) )))))

(ac-def ac-fn-rest (args body env)
  ((g ac-fn-rest-impl)
     ((g ac-args-without-rest) args)
     (gensym)
     ((g ac-rest-param) args)
     body
     env))


;; bound

(ac-def bound (name)
  (let ((undef (list 'undef)))
    ((g ar-tnil)
     (not (eq? (get-default arc name (lambda () undef)) undef)))))


;; disp, write

(define (tostringf f)
  (let ((port (open-output-string)))
    (parameterize ((current-output-port port))
      (f))
    (get-output-string port)))

(ac-def racket-disp (x (port (current-output-port)))
  (display x port)
  (flush-output port)
  x)

(ac-def racket-write (x (port (current-output-port)))
  (write x port)
  (flush-output port)
  x)


;; table

(ac-def table ((init #f))
  (let ((h (make-hash)))
    (when init (init h))
    h))


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
