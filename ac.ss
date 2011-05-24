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

(define (new-arc (options (make-hash)))
  (let ((arc (make-hash)))
    (hash-set! arc 'racket-namespace* (make-arc-racket-namespace))
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
   (set arc 'sig (make-hash))))

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


;; ar-def

(defmacro ar-def (name signature . body)
  `(add-ac-build-step
    (lambda (arc)
      (hash-set! (get arc 'sig) ',name (toarc ',signature))
      ,@(map (lambda (form)
               `(racket-eval arc ',form))
             body))
    '(ar-def ,name)))


;; primitives

(add-ac-build-step
  (lambda (arc)
    (hash-set! (g sig) '-         (toarc 'args))
    (hash-set! (g sig) '/         (toarc 'args))
    (hash-set! (g sig) '*         (toarc 'args))
    (hash-set! (g sig) 'cons      (toarc '(a b)))
    (hash-set! (g sig) 'inside    (toarc '(s)))
    (hash-set! (g sig) 'instring  (toarc '(str)))
    (hash-set! (g sig) 'outstring (toarc '()))
    (hash-set! (g sig) 'uniq      (toarc '()))
    (racket-eval arc
      `(racket-begin
        (racket-define -         racket--)
        (racket-define /         racket-/)
        (racket-define *         racket-*)
        (racket-define cons      racket-mcons)
        (racket-define inside    racket-get-output-string)
        (racket-define instring  racket-open-input-string)
        (racket-define nil       (racket-quote nil))
        (racket-define outstring racket-open-output-string)
        (racket-define t         (racket-quote t))
        (racket-define uniq      racket-gensym)))))


;; r/list-toarc

(define (r/list-toarc x)
  (cond ((pair? x)
         (mcons (car x) (r/list-toarc (cdr x))))
        ((null? x)
         'nil)
        (else x)))

(ar-def ar-r/list-toarc (x)
  (racket-define (ar-r/list-toarc x)
    (racket-cond
     ((racket-pair? x)
      (racket-mcons (racket-car x) (ar-r/list-toarc (racket-cdr x))))
     ((racket-null? x)
      (racket-quote nil))
     (racket-else x))))


;; list

(define (arc-list . rest)
  (r/list-toarc rest))

(ar-def list args
  (racket-define (list . args)
    (ar-r/list-toarc args)))


;; ar-list-fromarc

(ar-def ar-list-fromarc (x)
  (racket-define (ar-list-fromarc x)
    (racket-cond
     ((racket-mpair? x)
      (racket-cons (racket-mcar x) (ar-list-fromarc (racket-mcdr x))))
     ((racket-eq? x (racket-quote nil))
      (racket-quote ()))
     (racket-else x))))


;; ar-toarc

(add-ac-build-step
 (lambda (arc)
   (ac-def-fn arc 'ar-toarc '(x) toarc))
 '(ar-toarc))


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

(add-ac-build-step
 (lambda (arc)
   (ac-def-fn arc 'ar-deep-fromarc '(x) ar-deep-fromarc))
 '(ar-deep-fromarc))


;; err

(ar-def err args
  (racket-define err racket-error))


;; car, cdr

(ar-def car (x)
  (racket-define (car x)
    (racket-if (racket-eq? x (racket-quote nil))
                (racket-quote nil)
                (racket-mcar x))))

(ar-def cdr (x)
  (racket-define (cdr x)
    (racket-if (racket-eq? x (racket-quote nil))
                (racket-quote nil)
                (racket-mcdr x))))


;; cadr, cddr

(ar-def cadr (x)
  (racket-define (cadr x)
    (car (cdr x))))

(ar-def cddr (x)
  (racket-define (cddr x)
    (cdr (cdr x))))


;; type

(ar-def ar-exint (x)
  (racket-define (ar-exint x)
    (racket-and (racket-integer? x) (racket-exact? x))))

(ar-def ar-tagged (x)
  (racket-define (ar-tagged x)
    (racket-and (racket-vector? x)
                (racket-eq? (racket-vector-ref x 0) (racket-quote tagged)))))

(ar-def rep (x)
  (racket-define (rep x)
    (racket-if (ar-tagged x)
                (racket-vector-ref x 2)
                x)))

(ar-def type (x)
  (racket-define (type x)
    (racket-cond
     ((ar-tagged x)             (racket-vector-ref x 1))
     ((racket-mpair? x)         (racket-quote cons))
     ((racket-symbol? x)        (racket-quote sym))
     ((racket-parameter? x)     (racket-quote parameter))
     ((racket-procedure? x)     (racket-quote fn))
     ((racket-char? x)          (racket-quote char))
     ((racket-string? x)        (racket-quote string))
     ((ar-exint x)              (racket-quote int))
     ((racket-number? x)        (racket-quote num))
     ((racket-hash? x)          (racket-quote table))
     ((racket-output-port? x)   (racket-quote output))
     ((racket-input-port? x)    (racket-quote input))
     ((racket-tcp-listener? x)  (racket-quote socket))
     ((racket-exn? x)           (racket-quote exception))
     ((racket-thread? x)        (racket-quote thread))
     ((racket-thread-cell? x)   (racket-quote thread-cell))
     ((racket-semaphore? x)     (racket-quote semaphore))
     (racket-else               (racket-quote unknown)))))


;; ar-tnil

(ar-def ar-tnil (x)
  (racket-define (ar-tnil x)
    (racket-if x (racket-quote t) (racket-quote nil))))


;; ar-no

(ar-def ar-no (x)
  (racket-define (ar-no x)
    (racket-eq? x (racket-quote nil))))


;; ar-true

(ar-def ar-true (x)
  (racket-define (ar-true x)
    (racket-not (ar-no x))))


;; map1

(ar-def map1 (f xs)
  (racket-define (map1 f xs)
    (racket-if (ar-no xs)
                (racket-quote nil)
                (cons (f (car xs)) (map1 f (cdr xs))))))


;; coerce

(ar-def ar-iround (x)
  (racket-define (ar-iround x)
    (racket-inexact->exact (racket-round x))))

(ar-def coerce (x totype . args)
  (racket-define (coerce x totype . args)
    (racket-cond
      ((ar-tagged x)
       (err "Can't coerce annotated object"))
      ((racket-eqv? totype (type x))
       x)
      ((racket-char? x)
       (racket-case totype
         ((int)       (racket-char->integer x))
         ((string)    (racket-string x))
         ((sym)       (racket-string->symbol (racket-string x)))
         (racket-else (err "Can't coerce" x type))))
      ((ar-exint x)
       (racket-case totype
         ((num)       x)
         ((char)      (racket-integer->char x))
         ((string)    (racket-apply racket-number->string x args))
         (racket-else (err "Can't coerce" x type))))
      ((racket-number? x)
       (racket-case totype
         ((int)       (ar-iround x))
         ((char)      (racket-integer->char (ar-iround x)))
         ((string)    (racket-apply racket-number->string x args))
         (racket-else (err "Can't coerce" x type))))
      ((racket-string? x)
       (racket-case totype
         ((sym)       (racket-string->symbol x))
         ((cons)      (ar-r/list-toarc (racket-string->list x)))
         ((num)       (racket-or (racket-apply racket-string->number x args)
                                 (err "Can't coerce" x totype)))
         ((int)       (racket-let ((n (racket-apply racket-string->number x args)))
                        (racket-if n
                                    (ar-iround n)
                                    (err "Can't coerce" x totype))))
         (racket-else (err "Can't coerce" x totype))))
      ((racket-mpair? x)
       (racket-case totype
         ((string)    (racket-apply racket-string-append
                                    (ar-list-fromarc
                                     (map1 (racket-lambda (y)
                                             (coerce y (racket-quote string)))
                                           x))))
         (racket-else (err "Can't coerce" x totype))))
      ((racket-eq? x (racket-quote nil))
       (racket-case totype
         ((string)    "")
         ((cons)      (racket-quote nil))
         (racket-else (err "Can't coerce" x type))))
      ((racket-symbol? x)
       (racket-case totype
         ((string)    (racket-symbol->string x))
         (racket-else (err "Can't coerce" x type))))
      (racket-else x))))


;; annotate

(ar-def annotate (totype rep)
  (racket-define (annotate totype rep)
    (racket-cond
     ((racket-eqv? (type rep) totype) rep)
     (racket-else (racket-vector (racket-quote tagged) totype rep)))))


;; is

(ar-def ar-pairwise (pred lst)
  (racket-define (ar-pairwise pred lst)
    (racket-cond
      ((racket-null? lst) (racket-quote t))
      ((racket-null? (racket-cdr lst)) (racket-quote t))
      ((racket-not (racket-eqv? (pred (racket-car lst) (racket-cadr lst))
                                (racket-quote nil)))
       (ar-pairwise pred (racket-cdr lst)))
      (racket-else (racket-quote nil)))))
  
(ar-def is2 (a b)
  (racket-define (is2 a b)
    (ar-tnil
     (racket-or (racket-eqv? a b)
                (racket-and (racket-string? a)
                            (racket-string? b)
                            (racket-string=? a b))))))

(ar-def is args
  (racket-define (is . args)
    (ar-pairwise is2 (ar-list-fromarc args))))


;; caris

(ar-def caris (x val)
  (racket-define (caris x val)
    (ar-tnil
     (racket-and (racket-mpair? x)
                 (ar-true (is (car x) val))))))


;; <

(ar-def <2 (x y)
  (racket-define (<2 x y)
    (ar-tnil
     (racket-cond
      ((racket-and (racket-number? x) (racket-number? y)) (racket-< x y))
      ((racket-and (racket-string? x) (racket-string? y)) (racket-string<? x y))
      ((racket-and (racket-symbol? x) (racket-symbol? y))
       (racket-string<? (racket-symbol->string x)
                        (racket-symbol->string y)))
      ((racket-and (racket-char? x) (racket-char? y)) (racket-char<? x y))
      (racket-else (err "Can't <" x y))))))

(ar-def < args
  (racket-define (< . args)
    (ar-pairwise <2 (ar-list-fromarc args))))

(ar-def >2 (x y)
  (racket-define (>2 x y)
    (ar-tnil
     (racket-cond
      ((racket-and (racket-number? x) (racket-number? y)) (racket-> x y))
      ((racket-and (racket-string? x) (racket-string? y)) (racket-string>? x y))
      ((racket-and (racket-symbol? x) (racket-symbol? y))
       (racket-string>? (racket-symbol->string x) (racket-symbol->string y)))
      ((racket-and (racket-char? x) (racket-char? y)) (racket-char>? x y))
      (racket-else (err "Can't >" x y))))))

(ar-def > args
  (racket-define (> . args)
    (ar-pairwise >2 (ar-list-fromarc args))))


;; len

(ar-def list-len (x)
  (racket-define (list-len x)
    (racket-cond
     ((ar-no x)         0)
     ((racket-mpair? x) (racket-+ 1 (list-len (racket-mcdr x))))
     (racket-else       (err "len expects a proper list")))))

(ar-def len (x)
  (racket-define (len x)
    (racket-cond
     ((racket-string? x) (racket-string-length x))
     ((racket-hash? x)   (racket-hash-count x))
     (racket-else        (list-len x)))))


;; join

(ar-def join args
  (racket-define (join . args)
    (ar-r/list-toarc
     (racket-apply racket-append
                   (racket-map ar-list-fromarc (ar-list-fromarc args))))))


;; +

(ar-def ar-alist (x)
  (racket-define (ar-alist x)
    (racket-or (ar-no x) (racket-mpair? x))))

(ar-def + args
  (racket-define (+ . args)
    (racket-cond
     ((racket-null? args)
      0)
     ((racket-or (racket-char? (racket-car args)) (racket-string? (racket-car args)))
      (racket-apply racket-string-append
                    (racket-map (racket-lambda (a)
                                  (coerce a (racket-quote string)))
                                args)))
     ((ar-alist (racket-car args))
      (racket-apply join args))
     (racket-else
      (racket-apply racket-+ args)))))


;; peekc

(ar-def peekc ((o port stdin))
  (racket-define (peekc (port (racket-current-input-port)))
    (racket-let ((c (racket-peek-char port)))
      (racket-if (racket-eof-object? c) (racket-quote nil) c))))


;; readc

(ar-def readc ((o port stdin) (o eof nil))
  (racket-define (readc (port (racket-current-input-port))
                        (eof (racket-quote nil)))
    (racket-let ((c (racket-read-char port)))
      (racket-if (racket-eof-object? c) eof c))))


;; writec

(ar-def writec (c (o port stdout))
  (racket-define (writec c (port (racket-current-output-port)))
    (racket-write-char c port)))


;; racket-module-ref

(ar-def racket-module-ref (a/module)
  (racket-define (racket-module-ref a/module)
    (racket-let ((r/module (ar-deep-fromarc a/module)))
      (racket-lambda (sym)
        (racket-dynamic-require r/module sym)))))


;; ar-apply

(ar-def ar-apply-cons (fn . racket-arg-list)
  (racket-define (ar-apply-cons fn . racket-arg-list)
    (racket-mlist-ref fn (racket-car racket-arg-list))))

(ar-def ar-apply-string (fn . racket-arg-list)
  (racket-define (ar-apply-string fn . racket-arg-list)
    (racket-string-ref fn (racket-car racket-arg-list))))

(ar-def ar-apply-hash (fn . racket-arg-list)
  (racket-define (ar-apply-hash fn . racket-arg-list)  
    (racket-hash-ref fn
      (racket-car racket-arg-list)
      (racket-let ((default (racket-if (racket-pair? (racket-cdr racket-arg-list))
                                        (racket-car (racket-cdr racket-arg-list))
                                        (racket-quote nil))))
        (racket-lambda () default)))))

(ar-def ar-apply (fn . racket-arg-list)
  (racket-define (ar-apply fn . racket-arg-list)
    (racket-cond
     ((racket-procedure? fn)
      (racket-apply fn racket-arg-list))
     ((racket-mpair? fn)
      (racket-apply ar-apply-cons fn racket-arg-list))
     ((racket-string? fn)
      (racket-apply ar-apply-string fn racket-arg-list))
     ((racket-hash? fn)
      (racket-apply ar-apply-hash fn racket-arg-list))
     (racket-else
      (err "Function call on inappropriate object" fn racket-arg-list)))))


;; ar-funcall

(ar-def ar-funcall0 (fn)
  (racket-define (ar-funcall0 fn)
    (racket-if (racket-procedure? fn)
                (fn)
                (ar-apply fn))))

(ar-def ar-funcall1 (fn arg1)
  (racket-define (ar-funcall1 fn arg1)
    (racket-if (racket-procedure? fn)
                 (fn arg1)
                 (ar-apply fn arg1))))

(ar-def ar-funcall2 (fn arg1 arg2)
  (racket-define (ar-funcall2 fn arg1 arg2)
    (racket-if (racket-procedure? fn)
                (fn arg1 arg2)
                (ar-apply fn arg1 arg2))))

(ar-def ar-funcall3 (fn arg1 arg2 arg3)
  (racket-define (ar-funcall3 fn arg1 arg2 arg3)
    (racket-if (racket-procedure? fn)
                (fn arg1 arg2 arg3)
                (ar-apply fn arg1 arg2 arg3))))

(ar-def ar-funcall4 (fn arg1 arg2 arg3 arg4)
  (racket-define (ar-funcall4 fn arg1 arg2 arg3 arg4)
    (racket-if (racket-procedure? fn)
                (fn arg1 arg2 arg3 arg4)
                (ar-apply fn arg1 arg2 arg3 arg4))))


;; apply

(ar-def ar-combine-args (as)
  (racket-define (ar-combine-args as)
    (racket-let next ((as as) (accum (racket-list)))
      (racket-cond
       ((racket-null? as)
        accum)
       ((racket-null? (racket-cdr as))
        (racket-append accum (ar-list-fromarc (racket-car as))))
       (racket-else
        (next (racket-cdr as) (racket-append accum (racket-list (racket-car as)))))))))

(ar-def apply (fn . args)
  (racket-define (apply fn . args)
    (racket-apply ar-apply fn (ar-combine-args args))))


;; The Arc compiler!

(ar-def ac (s env)
  (racket-define (ac s env)
    (err "Bad object in expression" s)))

; ...which is extended to do more below :-)


; Extending the Arc compiler

(ac-def ar-extend-impl (name test body)
  (let ((previous (get arc name)))
    (set arc name
      (lambda args
        (let ((result (apply test arc args)))
          (if ((g ar-true) result)
               (apply body arc result args)
               (apply previous args)))))))

(defmacro extend (name args test . body)
  `(add-ac-build-step
     (lambda (arc)
       ((g ar-extend-impl) ',name
        (lambda (arc . ,args) ,test)
        (lambda (arc it . ,args) ,@body)))
     '(extend ,name ,args ,test)))


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
    (set arc 'racket-readtable* #f)
    (set arc 'arc-readtable* (bracket-readtable #f))))

(ac-def racket-read-from-string (str)
  (parameterize ((current-readtable (g racket-readtable*)))
    (read (open-input-string str))))

(ar-def ar-read (input)
  (racket-define (ar-read input)
    (racket-parameterize ((racket-current-readtable arc-readtable*))
      (racket-read input))))

(ar-def ar-aload1 (p)
  (racket-define (ar-aload1 p)
    (racket-let ((x (ar-read p)))
      (racket-if (racket-eof-object? x)
                  (racket-quote nil)
                  (racket-begin
                   (eval (ar-toarc x))
                   (ar-aload1 p))))))

(ar-def ar-load filenames
  (racket-define (ar-load . filenames)
    (racket-for-each
     (racket-lambda (filename)
       (racket-call-with-input-file filename
         (racket-lambda (p) (ar-aload1 p))))
     filenames)
    nil))

(add-ac-build-step
 (lambda (arc)
   (ac-def-fn arc 'this-namespace '() (lambda () arc))))
