#lang scheme

(require scheme/mpair)
(require "ar.ss")

(provide (all-from-out "ar.ss") (all-defined-out))

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
      (namespace-require '(prefix racket- racket/base)))
    ns))

(define (new-arc (options (hash)))
  (let ((arc (hash)))
    (hash-set! arc 'racket-namespace* (make-arc-racket-namespace))
    (hash-for-each (new-ar)
      (lambda (k v)
        (set arc k v)))
    (for-each (lambda (pair)
                (let ((step (car pair)))
                  (step arc)))
              (hash-ref options 'build-steps ac-build-steps))
    arc))


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
  (parameterize ((current-readtable (get-default arc 'arc-readtable* (lambda () #f)))
                 (compile-allow-set!-undefined #t))
    (eval form (hash-ref arc 'racket-namespace*))))


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


;; ar-toarc

(ac-def ar-toarc (x)
  (toarc x))


;; ar-deep-fromarc

; todo I think we just need some better way of representing a Racket
; null in the Arc compiler.

(define (strict-deep-fromarc x)
  (cond ((eq? x 'nil)
         '())
        ((mpair? x)
         (cons (strict-deep-fromarc (mcar x))
               (strict-deep-fromarc (mcdr x))))
        (else
         x)))

(define (ar-deep-fromarc x)
  (cond ((and (mpair? x) (eq? (mcar x) 'racket-list))
         (strict-deep-fromarc (mcar (mcdr x))))

        ;; nil in the car position isn't a list terminator, and so can
        ;; be left alone.
        ((mpair? x)
         (cons (let ((a (mcar x)))
                 (if (eq? a 'nil) 'nil (ar-deep-fromarc a)))
               (let ((b (mcdr x)))
                 (if (eq? b 'nil) '() (ar-deep-fromarc b)))))

        (else
         x)))

(ac-def ar-deep-fromarc (x)
  (ar-deep-fromarc x))


;; err

(ac-def err args
  (apply error args))


;; car, cdr

(ac-def car (x)
  (if (eq? x 'nil)
       'nil
       (mcar x)))

(ac-def cdr (x)
  (if (eq? x 'nil)
       'nil
       (mcdr x)))


;; cadr, cddr

(add-ac-build-step
  (lambda (arc)
    (racket-eval arc
                 '(racket-define (cadr x)
                    (car (cdr x))))
   (hash-set! (get arc 'sig) 'cadr (toarc '(x))))
  '(ac-def cadr))

(add-ac-build-step
 (lambda (arc)
   (racket-eval arc
                '(racket-define (cddr x)
                   (cdr (cdr x))))
   (hash-set! (get arc 'sig) 'cddr (toarc '(x))))
 '(ac-def cddr))


;; type

(define (exint? x) (and (integer? x) (exact? x)))

(define (tagged? x)
  (and (vector? x) (eq? (vector-ref x 0) 'tagged)))

(ac-def rep (x)
  (if (tagged? x)
       (vector-ref x 2)
       x))

(ac-def type (x)
  (cond ((tagged? x)        (vector-ref x 1))
        ((mpair? x)         'cons)
        ((symbol? x)        'sym)
        ((parameter? x)     'parameter)
        ((procedure? x)     'fn)
        ((char? x)          'char)
        ((string? x)        'string)
        ((exint? x)         'int)
        ((number? x)        'num)
        ((hash? x)          'table)
        ((output-port? x)   'output)
        ((input-port? x)    'input)
        ((tcp-listener? x)  'socket)
        ((exn? x)           'exception)
        ((thread? x)        'thread)
        ((thread-cell? x)   'thread-cell)
        ((semaphore? x)     'semaphore)
        (else               'unknown)))


;; ar-tnil

(ac-def ar-tnil (x)
  (if x 't 'nil))


;; ar-no

(ac-def ar-no (x)
  (eq? x 'nil))


;; ar-true

(add-ac-build-step
 (lambda (arc)
   (racket-eval arc
                '(racket-define (ar-true x)
                   (racket-not (ar-no x))))
   (hash-set! (get arc 'sig) 'ar-true (toarc '(x))))
 '(ac-def ar-true))


;; map1

(add-ac-build-step
 (lambda (arc)
   (racket-eval arc
                '(racket-define (map1 f xs)
                   (racket-if (ar-no xs)
                               (racket-quote nil)
                               (cons (f (car xs)) (map1 f (cdr xs))))))
   (hash-set! (get arc 'sig) 'map1 (toarc '(f xs))))
 '(ac-def map1))


;; coerce

(define (iround x) (inexact->exact (round x)))

(ac-def coerce (x type . args)
  (cond
    ((tagged? x) ((g err) "Can't coerce annotated object"))
    ((eqv? type ((g type) x)) x)
    ((char? x)      (case type
                      ((int)     (char->integer x))
                      ((string)  (string x))
                      ((sym)     (string->symbol (string x)))
                      (else      ((g err) "Can't coerce" x type))))
    ((exint? x)     (case type
                      ((num)     x)
                      ((char)    (integer->char x))
                      ((string)  (apply number->string x args))
                      (else      ((g err) "Can't coerce" x type))))
    ((number? x)    (case type
                      ((int)     (iround x))
                      ((char)    (integer->char (iround x)))
                      ((string)  (apply number->string x args))
                      (else      ((g err) "Can't coerce" x type))))
    ((string? x)    (case type
                      ((sym)     (string->symbol x))
                      ((cons)    ((g r/list-toarc) (string->list x)))
                      ((num)     (or (apply string->number x args)
                                     ((g err) "Can't coerce" x type)))
                      ((int)     (let ((n (apply string->number x args)))
                                   (if n
                                       (iround n)
                                       ((g err) "Can't coerce" x type))))
                      (else      ((g err) "Can't coerce" x type))))
    ((mpair? x)     (case type
                      ((string)  (apply string-append
                                        (list-fromarc
                                         ((g map1) (lambda (y) ((g coerce) y 'string)) x))))
                      (else      ((g err) "Can't coerce" x type))))
    ((eq? x 'nil)   (case type
                      ((string)  "")
                      ((cons)    'nil)
                      (else      ((g err) "Can't coerce" x type))))
    ((symbol? x)    (case type
                      ((string)  (symbol->string x))
                      (else      ((g err) "Can't coerce" x type))))
    (#t             x)))


;; annotate

(ac-def annotate (type rep)
  (cond ((eqv? ((g type) rep) type) rep)
        (else (vector 'tagged type rep))))


;; is

(add-ac-build-step
 (lambda (arc)
   (racket-eval
    arc
    '(racket-define (ar-pairwise pred lst)
       (racket-cond
        ((racket-null? lst) (racket-quote t))
        ((racket-null? (racket-cdr lst)) (racket-quote t))
        ((racket-not (racket-eqv? (pred (racket-car lst) (racket-cadr lst))
                                  (racket-quote nil)))
         (ar-pairwise pred (racket-cdr lst)))
        (racket-else (racket-quote nil)))))
   (hash-set! (get arc 'sig) 'ar-pairwise (toarc '(pred lst)))))

(define (pairwise pred lst)
  (cond ((null? lst) 't)
        ((null? (cdr lst)) 't)
        ((not (eqv? (pred (car lst) (cadr lst)) 'nil))
         (pairwise pred (cdr lst)))
        (else 'nil)))

(add-ac-build-step
 (lambda (arc)
   (racket-eval
    arc
    '(racket-define (is2 a b)
       (ar-tnil
        (racket-or (racket-eqv? a b)
                   (racket-and (racket-string? a)
                               (racket-string? b)
                               (racket-string=? a b))))))
   (hash-set! (get arc 'sig) 'is2 (toarc '(a b))))
 '(ac-def is2))
 
(add-ac-build-step
 (lambda (arc)
   (racket-eval
    arc
    '(racket-define (is . args)
       (ar-pairwise is2 (ar-list-fromarc args))))
   (hash-set! (get arc 'sig) 'is 'args))
 '(ac-def is))


;; caris

(ac-def caris (x val)
  ((g ar-tnil)
   (and (mpair? x)
        ((g ar-true) ((g is) ((g car) x) val)))))


;; <

(ac-def <2 (x y)
  ((g ar-tnil)
   (cond ((and (number? x) (number? y)) (< x y))
         ((and (string? x) (string? y)) (string<? x y))
         ((and (symbol? x) (symbol? y)) (string<? (symbol->string x)
                                                  (symbol->string y)))
         ((and (char? x) (char? y)) (char<? x y))
         ((g err) "Can't <" x y))))

(ac-def < args
  (pairwise (g <2) (list-fromarc args)))


(ac-def >2 (x y)
  ((g ar-tnil)
   (cond ((and (number? x) (number? y)) (> x y))
         ((and (string? x) (string? y)) (string>? x y))
         ((and (symbol? x) (symbol? y)) (string>? (symbol->string x)
                                                  (symbol->string y)))
         ((and (char? x) (char? y)) (char>? x y))
         ((g err) "Can't >" x y))))

(ac-def > args
  (pairwise (g >2) (list-fromarc args)))


;; len

(ac-def list-len (x)
  (cond (((g ar-no) x) 0)
        ((mpair? x)    (+ 1 ((g list-len) (mcdr x))))
        (else          ((g err) "len expects a proper list"))))

(ac-def len (x)
  (cond ((string? x) (string-length x))
        ((hash? x)   (hash-count x))
        (else        ((g list-len) x))))


;; join

(ac-def join args
  (r/list-toarc (apply append (map list-fromarc (list-fromarc args)))))


;; +

(ac-def ar-alist (x)
  (or ((g ar-no) x) (mpair? x)))

(ac-def + args
  (cond ((null? args)
         0)
        ((or (char? (car args)) (string? (car args)))
         (apply string-append
                (map (lambda (a) ((g coerce) a 'string)) args)))
        (((g ar-alist) (car args))
         (apply (g join) args))
        (else
         (apply + args))))


;; peekc

(ac-def-sig peekc ((port (current-input-port))) ((o port stdin))
  (let ((c (peek-char port)))
    (if (eof-object? c) 'nil c)))


;; readc

(ac-def-sig readc ((port (current-input-port)) (eof 'nil))
                  ((o port stdin) (o eof nil))
  (let ((c (read-char port)))
    (if (eof-object? c) eof c)))


;; writec

(ac-def-sig writec (c (port (current-output-port)))
                   (c (o port stdout))
  (write-char c port))


;; racket-parameterize

(ac-def racket-parameterize (parameter value body)
  (parameterize ((parameter value))
    (body)))


;; racket-module-ref

(ac-def racket-module-ref (a/module)
  (let ((r/module ((g ar-deep-fromarc) a/module)))
    (lambda (sym)
      (dynamic-require r/module sym))))


;; ar-apply

(ac-def ar-apply (fn . racket-arg-list)
  (cond ((procedure? fn)
         (apply fn racket-arg-list))
        ((mpair? fn)
         (mlist-ref fn (car racket-arg-list)))
        ((string? fn)
         (string-ref fn (car racket-arg-list)))
        ((hash? fn)
         (hash-ref fn
                   (car racket-arg-list)
                   (let ((default (if (pair? (cdr racket-arg-list))
                                       (car (cdr racket-arg-list))
                                       'nil)))
                     (lambda () default))))
        (else ((g err) "Function call on inappropriate object" fn racket-arg-list))))


;; ar-funcall

(ac-def ar-funcall0 (fn)
  (if (procedure? fn)
      (fn)
      ((g ar-apply) fn)))

(ac-def ar-funcall1 (fn arg1)
  (if (procedure? fn)
      (fn arg1)
      ((g ar-apply) fn arg1)))

(ac-def ar-funcall2 (fn arg1 arg2)
  (if (procedure? fn)
      (fn arg1 arg2)
      ((g ar-apply) fn arg1 arg2)))

(ac-def ar-funcall3 (fn arg1 arg2 arg3)
  (if (procedure? fn)
      (fn arg1 arg2 arg3)
      ((g ar-apply) fn arg1 arg2 arg3)))

(ac-def ar-funcall4 (fn arg1 arg2 arg3 arg4)
  (if (procedure? fn)
      (fn arg1 arg2 arg3 arg4)
      ((g ar-apply) fn arg1 arg2 arg3 arg4)))


;; apply

(ac-def ar-combine-args (as)
  (let next ((as as) (accum '()))
    (cond ((null? as)
         accum)
        ((null? (cdr as))
         (append accum (list-fromarc (car as))))
        (else
         (next (cdr as) (append accum (list (car as))))))))

(add-ac-build-step
 (lambda (arc)
   (racket-eval arc
                '(racket-define (apply fn . args)
                   (racket-apply ar-apply fn (ar-combine-args args))))
   (hash-set! (get arc 'sig) 'apply (toarc '(fn . args))))
 '(ac-def apply))


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
             (if ((g ar-true) result)
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
  ((g ar-tnil)
   (or (char? x)
       (string? x)
       (number? x)
       (procedure? x))))

(extend ac (s env)
  ((g ac-literal?) s)
  s)

; it's alive!


;; variables

(ac-def ar-mem (v lst)
  ((g ar-tnil)
   (and (mpair? lst)
        (or (eqv? v (mcar lst))
            ((g ar-true) ((g ar-mem) v (mcdr lst)))))))

(ac-def ac-lex? (v env)
  ((g ar-mem) v env))

(define (global-ref-err arc v)
  (let ((message (string-append "undefined global variable: "
                                (symbol->string v))))
    (lambda ()
      ((g err) message))))

(ac-def ac-global (v)
  v)

(ac-def ac-var-ref (s env)
  (if ((g ar-true) ((g ac-lex?) s env))
       s
       ((g ac-global) s)))

(extend ac (s env)
  ((g ar-tnil) (symbol? s))
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
           ((g ac-args) ((g cadr) f) args env)))

   (else
    (mcons (case ((g len) args)
             ((0) (g ar-funcall0))
             ((1) (g ar-funcall1))
             ((2) (g ar-funcall2))
             ((3) (g ar-funcall3))
             ((4) (g ar-funcall4))
             (else (g ar-apply)))
           (mcons ((g ac) f env)
                  ((g map1) (lambda (arg) ((g ac) arg env)) args))))))

(extend ac (s env)
  ((g ar-tnil) (mpair? s))
  ((g ac-call) ((g car) s) ((g cdr) s) env))


;; quote

; The goal here is to get the quoted value tunneled through Racket's
; compiler unscathed.  This trick uses rocketnia's method: Racket
; doesn't copy function values.

(extend ac (s env) ((g caris) s 'quote)
  (let ((v ((g cadr) s)))
    ((g list) ((g list) 'racket-quote (lambda () v)))))


;; fn

; Rest args, optional args, and arg list destructuring are implemented
; later.

(ac-def ac-body (body env)
  ((g map1) (lambda (x) ((g ac) x env)) body))

(ac-def ac-body* (body env)
  (if ((g ar-no) body)
       ((g list) '(racket-quote nil))
       ((g ac-body) body env)))

(ac-def ac-body*x (args body env)
  ((g ac-body*) body ((g join) ((g ac-arglist) args) env)))

(ac-def ac-arglist (a)
  (cond (((g ar-no) a) 'nil)
        ((symbol? a) (arc-list a))
        ((and (symbol? (mcdr a)) (not ((g ar-no) (mcdr a))))
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
  (if ((g ar-true) ((g dotted-list?) args))
       ((g ac-fn-rest) args body env)
       (mcons 'racket-lambda
              ;; TODO I think it would be better to have an explicit
              ;; representation for nil instead
              (mcons (arc-list 'racket-list args)
                     ((g ac-body*x) args body env)))))

(extend ac (s env)
  ((g caris) s 'fn)
  ((g ac-fn) ((g cadr) s) ((g cddr) s) env))


;; eval

(define (arc-eval arc form)
  (racket-eval arc ((g ar-deep-fromarc) ((get arc 'ac) form 'nil))))
          

(ac-def eval (form (other-arc 'nil))
  (arc-eval (if ((g ar-true) other-arc) other-arc arc) form))


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
  ((g ac-mac-call) it args env))


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
             (racket-let ((,rest (,r/list-toarc ,r/rest)))
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
         (begin (arc-eval arc ((g ar-toarc) x))
                (aload1 arc p)))))

(define (aload arc . filenames)
  (for-each (lambda (filename)
              (call-with-input-file filename (lambda (p) (aload1 arc p))))
            filenames))

(add-ac-build-step
 (lambda (arc)
   (ac-def-fn arc 'aload '(namespace . filenames) aload)
   (ac-def-fn arc 'this-namespace '() (lambda () arc))))
