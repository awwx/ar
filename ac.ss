#lang scheme

(require scheme/mpair)
(require "ar.ss")

(provide (all-from-out "ar.ss") (all-defined-out))


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
; of the compiler is created each time (new-ac) is called, including
; the compiler building steps defined so far.

(define (new-ac (build-steps ac-build-steps))
  (let ((globals* (new-ar)))
    (hash-set! globals* 'racket-namespace* (make-base-namespace))
    (for-each (lambda (pair)
                (let ((step (car pair)))
                  (step globals*)))
              build-steps)
    globals*))


;; sig

(add-ac-build-step
 (lambda (globals*)
   (hash-set! globals* 'sig (hash))))

(define-syntax g
  (lambda (stx)
    (syntax-case stx ()
      ((g v)
       (with-syntax ((globals* (datum->syntax #'v 'globals*)))
         #'(hash-ref globals* 'v))))))

(define (ac-def-fn globals* name signature fn)
  (hash-set! (hash-ref globals* 'sig) name (toarc signature))
  (hash-set! globals* name fn))

(define-syntax ac-def
  (lambda (stx)
    (syntax-case stx ()
      ((ac-def name args body ...)
       (with-syntax ((globals* (datum->syntax #'name 'globals*)))
         #'(add-ac-build-step
             (lambda (globals*)
               (ac-def-fn globals* 'name 'args (lambda args body ...)))
             `(ac-def ,'name ,'args)))))))


;; The Arc compiler!

(ac-def ac (s env)
  ((g err) "Bad object in expression" s))

; ...which is extended to do more below :-)


; Extending the Arc compiler

(define (extend-impl name test body source)
  (add-ac-build-step
   (lambda (globals*)
     (let ((previous (hash-ref globals* name)))
       (hash-set! globals* name
         (lambda args
           (let ((result (apply test globals* args)))
             (if (true? result)
                  (apply body globals* result args)
                  (apply previous args)))))))
   source))

(define-syntax extend
  (lambda (stx)
    (syntax-case stx ()
      ((extend name args test body ...)
       (with-syntax ((globals* (datum->syntax #'args 'globals*))
                     (it       (datum->syntax #'args 'it)))
         #'(extend-impl 'name
            (lambda (globals* . args) test)
            (lambda (globals* it . args) body ...)
            `(extend ,'name ,'args ,'test)))))))

;; literal

(ac-def ac-literal? (x)
  (tnil (or (char? x)
            (string? x)
            (number? x))))

(extend ac (s env)
  ((g ac-literal?) s)
  s)

; it's alive!


;; nil

(define (ac-nil)
  (arc-list 'quote 'nil))

(extend ac (s env)
  (tnil (eq? s 'nil))
  (ac-nil))


;; variables

(ac-def ac-lex? (v env)
  ((g mem) v env))

(define (global-ref-err globals* v)
  (let ((message (string-append "undefined global variable: "
                                (symbol->string v))))
    (lambda ()
      ((g err) message))))

; An Arc global variable reference to "foo" such as in (+ foo 3) compiles into
; the Racket expression
; (#<procedure:hash-ref> #<hash:globals*> 'foo #<procedure:global-ref-err>)
; ...and thus performs no lookups in Racket's namespace (don't know
; yet if it makes a difference).

(ac-def ac-global (v)
  (arc-list hash-ref
            globals*
            (arc-list 'quote v)
            (global-ref-err globals* v)))

(ac-def ac-var-ref (s env)
  (ar-if ((g ac-lex?) s env)
         s
         ((g ac-global) s)))

(extend ac (s env)
  (ar-and (tnil (not (no? s))) (tnil (symbol? s)))
  ((g ac-var-ref) s env))


;; call

; todo ac-dbname!
(ac-def ac-args (names exprs env)
  ((g map1) (lambda (expr) ((g ac) expr env)) exprs))

(ac-def ac-call (f args env)
  (cond
   ;; if we're about to call a literal fn such as ((fn (a b) ...) 1 2)
   ;; then we know we can just call it in Racket and we don't
   ;; have to use ar-apply
   ((and (mpair? f) (eq? (mcar f) 'fn))
    (mcons ((g ac) f env)
           ((g ac-args) (arc-cadr f) args env)))

   (else
    (mcons (case ((g len) args)
             ((0) ar-funcall0)
             ((1) ar-funcall1)
             ((2) ar-funcall2)
             ((3) ar-funcall3)
             ((4) ar-funcall4)
             (else ar-apply))
           (mcons ((g ac) f env)
                  ((g map1) (lambda (arg) ((g ac) arg env)) args))))))

(extend ac (s env)
  (tnil (mpair? s))
  ((g ac-call) (arc-car s) (arc-cdr s) env))


;; quote

; The goal here is to get the quoted value tunneled through Racket's
; compiler unscathed.  This trick uses rocketnia's method: Racket
; doesn't copy function values.

(extend ac (s env) ((g caris) s 'quote)
  (let ((v (arc-cadr s)))
    ((g list) ((g list) 'quote (lambda () v)))))


;; fn

; Rest args, optional args, and arg list destructuring are implemented
; later.

(ac-def ac-body (body env)
  (arc-map1 (lambda (x) ((g ac) x env)) body))

(ac-def ac-body* (body env)
  (if (no? body)
      ((g list) (ac-nil))
      ((g ac-body) body env)))

(ac-def ac-body*x (args body env)
  ((g ac-body*) body (arc-join ((g ac-arglist) args) env)))

(ac-def ac-arglist (a)
  (cond ((no? a) 'nil)
        ((symbol? a) (arc-list a))
        ((and (symbol? (mcdr a)) (not (no? (mcdr a))))
         (arc-list (mcar a) (mcdr a)))
        (else (mcons (mcar a) ((g ac-arglist) (mcdr a))))))

(ac-def dotted-list? (x)
  (cond ((and (symbol? x) (not (eq? x 'nil)))
         't)
        ((mpair? x)
         ((g dotted-list?) (mcdr x)))
        (else
         'nil)))

(ac-def ac-fn (args body env)
  (if (true? ((g dotted-list?) args))
       ((g ac-fn-rest) args body env)
       (mcons 'lambda
              (mcons args
                     ((g ac-body*x) args body env)))))

(extend ac (s env)
  ((g caris) s 'fn)
  ((g ac-fn) (arc-cadr s) (arc-cddr s) env))


;; eval

(define (arc-eval arc form)
  (parameterize ((current-readtable (hash-ref arc 'arc-readtable* #f)))
    (eval (deep-fromarc ((hash-ref arc 'ac) form 'nil))
          (hash-ref arc 'racket-namespace*))))

(ac-def eval (form (arc 'nil))
  (arc-eval (ar-or arc globals*) form))


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
  (cond ((true? (ar-caris x 'unquote))
         (arc-cadr x))
        ((true? (ar-caris x 'unquote-splicing))
         (error "illegal use of ,@ in non-list quasiquote expansion"))
        ((true? (ar-caris x 'quasiquote))
         ((g qq-expand) ((g qq-expand) (arc-cadr x))))
        ((mpair? x)
         ((g qq-expand-pair) x))
        (else
         (arc-list 'quote x))))

(ac-def qq-expand-pair (x)
  (arc-list 'join
            ((g qq-expand-list) (mcar x))
            ((g qq-expand) (mcdr x))))

(ac-def qq-expand-list (x)
  (cond ((true? (ar-caris x 'unquote))
         (arc-list 'list (arc-cadr x)))
        ((true? (ar-caris x 'unquote-splicing))
         (arc-cadr x))
        ((true? (ar-caris x 'quasiquote))
         ((g qq-expand-list) ((g qq-expand) (arc-cadr x))))
        ((mpair? x)
         (arc-list 'list ((g qq-expand-pair) x)))
        (else
         (arc-list 'quote (list x)))))

(extend ac (s env)
  ((g caris) s 'quasiquote)
  (let ((expansion ((g qq-expand) (arc-cadr s))))
    ((g ac) expansion env)))


;; if

(ac-def ac-if (args env)
  (cond ((no? args)
         (ac-nil))
        ((no? ((g cdr) args))
         ((g ac) ((g car) args) env))
        (else
         (arc-list 'if
                   (arc-list true? ((g ac) ((g car) args) env))
                   ((g ac) (arc-cadr args) env)
                   ((g ac-if) (arc-cddr args) env)))))

(extend ac (s env)
  ((g caris) s 'if)
  ((g ac-if) ((g cdr) s) env))


;; assign

(ac-def ac-global-assign (a b)
  (arc-list hash-set! globals* (arc-list 'quote a) b))

(ac-def ac-assign1 (a b1 env)
  (unless (symbol? a)
    (err "First arg to assign must be a symbol" a))
  (let ((result (gensym)))
    (arc-list 'let
              (arc-list (arc-list result ((g ac) b1 env)))
              (if (true? ((g ac-lex?) a env))                  
                   (arc-list 'set! a result)
                   ((g ac-global-assign) a result))
              result)))

(ac-def ac-assignn (x env)
  (if (no? x)
      'nil
      ;; todo: Arc 3.1 calls ac-macex here
      (mcons ((g ac-assign1) (arc-car x) (arc-cadr x) env)
             ((g ac-assignn) (arc-cddr x) env))))

(ac-def ac-assign (x env)
  (mcons 'begin
         ((g ac-assignn) x env)))

(extend ac (s env)
  (ar-caris s 'assign)
  ((g ac-assign) (arc-cdr s) env))


;; macro

(ac-def ac-macro? (fn)
  (if (symbol? fn)
      (let ((v (hash-ref globals* fn 'nil)))
        (if (and (tagged? v)
                 (eq? (arc-type v) 'mac))
            (ar-rep v)
            'nil))
      'nil))

(ac-def ac-mac-call (m args env)
  (let ((x1 (arc-apply m args)))
    (let ((x2 ((g ac) x1 env)))
      x2)))

(extend ac-call (fn args env)
  (if (true? ((g ac-lex?) fn env))
       'nil
       ((g ac-macro?) fn))
  ((g ac-mac-call) it args env))


;; fn rest arg

(ac-def ac-rest-param (x)
  (cond ((and (symbol? x) (not (eq? x 'nil)))
         x)
        ((mpair? x)
         ((g ac-rest-param) (mcdr x)))
        (else
         (error "not a dotted list"))))

(ac-def ac-args-without-rest (x)
  (cond ((mpair? x)
         (arc-join (arc-list (arc-car x)) ((g ac-args-without-rest) (mcdr x))))
        (else
         'nil)))

; The implementation of "ac-fn-rest" turned out to be a lot easier to
; write in Arc.

(add-ac-build-step
 (lambda (globals*)
   (hash-set! globals* 'ac-fn-rest-impl
     (arc-eval globals*
      (toarc '(fn (args r/rest rest body env)
           `(lambda ,(join args r/rest)
              (let ((,rest (,r/list-toarc ,r/rest)))
                ,@(ac-body*x (join args (list rest)) body env)))) )))))

(ac-def ac-fn-rest (args body env)
  ((g ac-fn-rest-impl)
     ((g ac-args-without-rest) args)
     (gensym)
     ((g ac-rest-param) args)
     body
     env))


;; bound

(ac-def bound (x)
  (tnil (hash-ref globals* x (lambda () #f))))


;; disp, write

(define (tostringf f)
  (let ((port (open-output-string)))
    (parameterize ((current-output-port port))
      (f))
    (get-output-string port)))

(ac-def primitive-disp (x (port (current-output-port)))
  (display x port)
  (flush-output port)
  x)

(ac-def primitive-write (x (port (current-output-port)))
  (write x port)
  (flush-output port)
  x)


;; defvar impl

(add-ac-build-step
 (lambda (globals*)
   (hash-set! globals* 'ac-defined-vars* (hash))))

(ac-def ac-defvar (v x)
  (hash-set! (g ac-defined-vars*) v x)
  'nil)

(ac-def ac-defined-var (v)
  (hash-ref (g ac-defined-vars*) v (lambda () 'nil)))

(extend ac-global (v)
  ((g ac-defined-var) v)
  (arc-list ar-apply (mcar it)))

(ac-def ac-not-assignable (v)
  (lambda (x)
    (err (string-append (symbol->string v) " is not assignable"))))

(extend ac-global-assign (a b)
  ((g ac-defined-var) a)
  (arc-list ar-apply
            (ar-or (arc-cadr it) ((g ac-not-assignable) a))
            b))


;; table

(ac-def table ((init #f))
  (let ((h (hash)))
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
         (err "Can't set reference" com ind val)))
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
  (lambda (globals*)
    (hash-set! globals* 'racket-readtable* #f)
    (hash-set! globals* 'arc-readtable* (bracket-readtable #f))))

(ac-def racket-read-from-string (str)
  (parameterize ((current-readtable (g racket-readtable*)))
    (read (open-input-string str))))

(define (arc-read arc input)
  (let ((globals* arc))
    (parameterize ((current-readtable (g arc-readtable*)))
      (read input))))

(define (aload1 arc p)
  (let ((x (arc-read arc p)))
    (if (eof-object? x)
         'nil
         (begin (arc-eval arc (toarc x))
                (aload1 arc p)))))

(define (aload arc . filenames)
  (for-each (lambda (filename)
              (call-with-input-file filename (lambda (p) (aload1 arc p))))
            filenames))
