#lang scheme

(require scheme/mpair)
(require "ar.ss")

(provide (all-from-out "ar.ss") (all-defined-out))

(define default-globals-implementation 'namespace)

(define (globals-implementation arc)
  (hash-ref arc 'globals-implementation* default-globals-implementation))

(define (ac-global-name s)
  (string->symbol (string-append "_" (symbol->string s))))

;; note that these don't need to be particularly fast

(define (get arc varname)
  (case (globals-implementation arc)
    ((table) (hash-ref arc varname))
    ((namespace) (namespace-variable-value (ac-global-name varname) #t #f
                   (hash-ref arc 'racket-namespace*)))))

(define (get-default arc varname default)
  (case (globals-implementation arc)
    ((table) (hash-ref arc varname default))
    ((namespace) (namespace-variable-value
                  (ac-global-name varname)
                  #t
                  default
                  (hash-ref arc 'racket-namespace*)))))

(define (set arc varname value)
  (case (globals-implementation arc)
    ((table) (hash-set! arc varname value))
    ((namespace)
     (namespace-set-variable-value! (ac-global-name varname) value
                   #t
                   (hash-ref arc 'racket-namespace*)))))


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

(define (new-arc (options (hash)))
  (let ((arc (hash)))
    (hash-set! arc 'racket-namespace* (make-base-namespace))
    (hash-for-each (new-ar)
      (lambda (k v)
        (set arc k v)))
    (for-each (lambda (pair)
                (let ((step (car pair)))
                  (step arc)))
              (hash-ref options 'build-steps ac-build-steps))
    arc))


;; sig

(add-ac-build-step
 (lambda (arc)
   (set arc 'sig (hash))))

(define-syntax g
  (lambda (stx)
    (syntax-case stx ()
      ((g v)
       (with-syntax ((arc (datum->syntax #'v 'arc)))
         #'(get arc 'v))))))

(define (ac-def-fn arc name signature fn)
  (hash-set! (get arc 'sig) name (toarc signature))
  (set arc name fn))

(add-ac-build-step
 (lambda (arc)
   (ac-def-fn arc 'new-arc '((o options)) new-arc)
   (ac-def-fn arc 'namespace-get '(namespace varname) get)
   (ac-def-fn arc 'namespace-set '(namespace varname value)
     (lambda (namespace varname value)
       (set namespace varname value)
       'nil))))

(define-syntax ac-def
  (lambda (stx)
    (syntax-case stx ()
      ((ac-def name args body ...)
       (with-syntax ((arc (datum->syntax #'name 'arc)))
         #'(add-ac-build-step
             (lambda (arc)
               (ac-def-fn arc 'name 'args (lambda args body ...)))
             `(ac-def ,'name ,'args)))))))


;; The Arc compiler!

(ac-def ac (s env)
  ((g err) "Bad object in expression" s))

; ...which is extended to do more below :-)


; Extending the Arc compiler

(define (extend-impl name test body source)
  (add-ac-build-step
   (lambda (arc)
     (let ((previous (get arc name)))
       (set arc name
         (lambda args
           (let ((result (apply test arc args)))
             (if (true? result)
                  (apply body arc result args)
                  (apply previous args)))))))
   source))

(define-syntax extend
  (lambda (stx)
    (syntax-case stx ()
      ((extend name args test body ...)
       (with-syntax ((arc (datum->syntax #'args 'arc))
                     (it       (datum->syntax #'args 'it)))
         #'(extend-impl 'name
            (lambda (arc . args) test)
            (lambda (arc it . args) body ...)
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


;; variables

(define (mem v lst)
  (tnil (and (mpair? lst) 
             (or (eqv? v (mcar lst))
                 (true? (mem v (mcdr lst)))))))

(ac-def ac-lex? (v env)
  (mem v env))

(define (global-ref-err arc v)
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
  (case (globals-implementation arc)
    ((table) (arc-list hash-ref
                       arc
                       (arc-list 'quote v)
                       (global-ref-err arc v)))
    ((namespace) (ac-global-name v))))
     
(ac-def ac-var-ref (s env)
  (if (true? ((g ac-lex?) s env))
       s
       ((g ac-global) s)))

(extend ac (s env)
  (tnil (symbol? s))
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
      ((g list) ''nil)
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
  (parameterize ((current-readtable (get-default arc 'arc-readtable* (lambda () #f)))
                 (compile-allow-set!-undefined #t))
    (eval (deep-fromarc ((get arc 'ac) form 'nil))
          (hash-ref arc 'racket-namespace*))))

(ac-def eval (form (other-arc 'nil))
  (arc-eval (if (true? other-arc) other-arc arc) form))


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
         ''nil)
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
  (case (globals-implementation arc)
    ((table) (arc-list hash-set! arc (arc-list 'quote a) b))
    ((namespace) (arc-list 'set! (ac-global-name a) b))))

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

(define (mac? x)
  (and (tagged? x)
       (eq? (arc-type x) 'mac)))

(ac-def ac-macro? (fn)
  (cond ((mac? fn)
         (ar-rep fn))
        ((symbol? fn)
         (let ((v (get-default arc fn (lambda () 'nil))))
           (if (mac? v)
               (ar-rep v)
               'nil)))
        (else
         'nil)))

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
 (lambda (arc)
   (set arc 'ac-fn-rest-impl
     (arc-eval arc
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

(ac-def bound (name)
  (let ((undef (list 'undef)))
    (tnil (not (eq? (get-default arc name (lambda () undef)) undef)))))


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
  (lambda (arc)
    (set arc 'racket-readtable* #f)
    (set arc 'arc-readtable* (bracket-readtable #f))))

(ac-def racket-read-from-string (str)
  (parameterize ((current-readtable (g racket-readtable*)))
    (read (open-input-string str))))

(define (arc-read arc input)
  (parameterize ((current-readtable (g arc-readtable*)))
    (read input)))

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

(add-ac-build-step
 (lambda (arc)
   (ac-def-fn arc 'aload '(namespace . filenames) aload)
   (ac-def-fn arc 'this-namespace '() (lambda () arc))))
