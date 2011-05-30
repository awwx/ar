(ail-code (ar-def ar-extend-impl (name test body)
  (racket-let ((previous (ar-var name)))
    (ar-assign name
      (racket-lambda args
        (racket-let ((result (racket-apply test arc* args)))
          (racket-if (ar-true result)
               (racket-apply body arc* args)
               (racket-apply previous args))))))))

(ail-code (racket-define-syntax-rule (ar-extend name args test body racket-...)
  (ar-extend-impl (racket-quote name)
    (racket-lambda (arc . args) test)
    (racket-lambda (arc . args) body racket-...))))

(ail-code (ar-def ac-literal? (x)
  (ar-tnil
   (racket-or (racket-char? x)
              (racket-string? x)
              (racket-number? x)
              (racket-procedure? x)))))

(ail-code (ar-extend ac (s env)
  (ac-literal? s)
  s))

(ail-code (ar-def ar-mem (v lst)
  (ar-tnil
   (racket-and (racket-mpair? lst)
               (racket-or (ar-true (is v (car lst)))
                          (ar-true (ar-mem v (cdr lst))))))))

(ail-code (ar-def ac-lex? (v env)
  (ar-mem v env)))

(ail-code (ar-def ac-global (v)
  v))

(ail-code (ar-extend ac (s env)
  (ar-tnil (racket-symbol? s))
  (racket-if (ar-true (ac-lex? s env))
              s
              (ac-global s))))

(ail-code (ar-def ac-args (names exprs env)
  (map1 (racket-lambda (expr)
          (ac expr env))
        exprs)))

(ail-code (ar-def ac-call (f args env)
  (racket-cond
   ;; if we're about to call a literal fn such as ((fn (a b) ...) 1 2)
   ;; then we know we can just call it in Racket and we don't
   ;; have to use ar-apply
   ((racket-and (racket-mpair? f) (racket-eq? (car f) (racket-quote fn)))
    (cons (ac f env)
          (ac-args (cadr f) args env)))

   (racket-else
    (cons (racket-case (len args)
            ((0) ar-funcall0)
            ((1) ar-funcall1)
            ((2) ar-funcall2)
            ((3) ar-funcall3)
            ((4) ar-funcall4)
            (racket-else ar-apply))
          (cons (ac f env)
                (map1 (racket-lambda (arg) (ac arg env)) args)))))))

(ail-code (ar-extend ac (s env)
  (ar-tnil
   (racket-and (racket-mpair? s)
               (racket-not (ar-true (is (car s) (racket-quote ail-code))))))
  (ac-call (car s) (cdr s) env)))

;; quote

; The goal here is to get the quoted value tunneled through Racket's
; compiler unscathed.  This trick uses rocketnia's method: Racket
; doesn't copy function values.

(ail-code (ar-extend ac (s env) (caris s (racket-quote quote))
  (racket-let ((v (cadr s)))
    (list (list (racket-quote racket-quote) (racket-lambda () v))))))

(ail-code (ar-def ac-body (body env)
  (map1 (racket-lambda (x) (ac x env)) body)))

(ail-code (ar-def ac-body* (body env)
  (racket-if (ar-no body)
              (list (racket-quote (racket-quote nil)))
              (ac-body body env))))

(ail-code (ar-def ac-arglist (a)
  (racket-cond
   ((ar-no a) (racket-quote nil))
   ((racket-symbol? a) (list a))
   ((racket-and (racket-symbol? (cdr a))
                (racket-not (ar-no (cdr a))))
    (list (car a) (cdr a)))
   (racket-else (cons (car a) (ac-arglist (cdr a)))))))

(ail-code (ar-def ac-body*x (args body env)
  (ac-body* body (join (ac-arglist args) env))))

(ail-code (ar-def ac-dotted-list? (x)
  (racket-cond
   ((racket-and (racket-symbol? x) (ar-true x))
    t)
   ((racket-mpair? x)
    (ac-dotted-list? (cdr x)))
   (racket-else
    nil))))

; Rest args, optional args, and arg list destructuring are implemented
; later.

(ail-code (ar-def ac-fn (args body env)
  (racket-if (ar-true (ac-dotted-list? args))
    (ac-fn-rest args body env)
    (racket-mcons (racket-quote racket-lambda)
                  (racket-mcons (ar-tunnel (ar-list-fromarc args))
                                (ac-body*x args body env))))))

(ail-code (ar-extend ac (s env)
  (caris s (racket-quote fn))
  (ac-fn (cadr s) (cddr s) env)))

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

(ail-code (ar-def qq-expand (x)
  (racket-cond
   ((ar-true (caris x (racket-quote unquote)))
    (cadr x))
   ((ar-true (caris x (racket-quote unquote-splicing)))
    (err "illegal use of ,@ in non-list quasiquote expansion"))
   ((ar-true (caris x (racket-quote quasiquote)))
    (qq-expand (qq-expand (cadr x))))
   ((racket-mpair? x)
    (qq-expand-pair x))
   (racket-else
    (list (racket-quote quote) x)))))

(ail-code (ar-def qq-expand-pair (x)
  (list (racket-quote join)
        (qq-expand-list (car x))
        (qq-expand      (cdr x)))))

(ail-code (ar-def qq-expand-list (x)
  (racket-cond
   ((ar-true (caris x (racket-quote unquote)))
    (list (racket-quote list) (cadr x)))
   ((ar-true (caris x (racket-quote unquote-splicing)))
    (cadr x))
   ((ar-true (caris x (racket-quote quasiquote)))
    (qq-expand-list (qq-expand (cadr x))))
   ((racket-mpair? x)
    (list (racket-quote list) (qq-expand-pair x)))
   (racket-else
    (list (racket-quote quote) (list x))))))

(ail-code (ar-extend ac (s env)
  (caris s (racket-quote quasiquote))
  (racket-let ((expansion (qq-expand (cadr s))))
    (ac expansion env))))

(ail-code (ar-def ac-if (args env)
  (racket-cond
   ((ar-no args)
    (racket-quote (racket-quote nil)))
   ((ar-no (cdr args))
    (ac (car args) env))
   (racket-else
    (list (racket-quote racket-if)
          (list ar-true (ac (car args) env))
          (ac (cadr args) env)
          (ac-if (cddr args) env))))))

(ail-code (ar-extend ac (s env)
  (caris s (racket-quote if))
  (ac-if (cdr s) env)))

(ail-code (ar-def ac-global-assign (a b)
  (list (racket-quote racket-set!) a b)))

(ail-code (ar-def ac-assign1 (a b1 env)
  (racket-unless (racket-symbol? a)
    (err "First arg to assign must be a symbol" a))
  (racket-let ((result (uniq)))
    (list (racket-quote racket-let)
          (list (list result (ac b1 env)))
          (racket-if (ar-true (ac-lex? a env))
                      (list (racket-quote racket-set!) a result)
                      (ac-global-assign a result))
          result))))

(ail-code (ar-def ac-assignn (x env)
  (racket-if (ar-no x)
              nil
              ;; todo: why does Arc 3.1 call ac-macex here?
              (cons (ac-assign1 (car x) (cadr x) env)
                    (ac-assignn (cddr x) env)))))

(ail-code (ar-def ac-assign (x env)
  (cons (racket-quote racket-begin)
        (ac-assignn x env))))

(ail-code (ar-extend ac (s env)
  (caris s (racket-quote assign))
  (ac-assign (cdr s) env)))

(ail-code (ar-def ac-macro? (fn)
  (racket-cond
   ((ar-true (is (type fn) (racket-quote mac)))
    (rep fn))
   ((racket-symbol? fn)
    (racket-let ((v (ar-var fn nil)))
      (racket-if (ar-true (is (type v) (racket-quote mac)))
                  (rep v)
                  nil)))
   (racket-else
    nil))))

(ail-code (ar-def ac-mac-call (m args env)
  (ac (apply m args) env)))

(ail-code (ar-extend ac-call (fn args env)
  (racket-if (ar-true (ac-lex? fn env))
              nil
              (ac-macro? fn))
  (ac-mac-call (ac-macro? fn) args env)))

(ail-code (ar-def ac-rest-param (x)
  (racket-cond
   ((racket-and (racket-symbol? x) (ar-true x))
    x)
   ((racket-mpair? x)
    (ac-rest-param (cdr x)))
   (racket-else
    (err "not a dotted list")))))

(ail-code (ar-def ac-args-without-rest (x)
  (racket-cond
   ((racket-mpair? x)
    (join (list (car x)) (ac-args-without-rest (cdr x))))
   (racket-else
    nil))))

(ail-code (racket-hash-set! sig (racket-quote ac-fn-rest-impl)
            (ar-toarc (racket-quote (args r/rest rest body env)))))

(ail-code (racket-define ac-fn-rest-impl
  (eval (ar-toarc (racket-quote
    (fn (args r/rest rest body env)
      `(racket-lambda ,(join args r/rest)
         (racket-let ((,rest (,ar-r/list-toarc ,r/rest)))
           ,@(ac-body*x (join args (list rest)) body env)))))))))

(ail-code (ar-def ac-fn-rest (args body env)
  (ac-fn-rest-impl
   (ac-args-without-rest args)
   (uniq)
   (ac-rest-param args)
   body
   env)))

(ail-code (racket-hash-set! sig (racket-quote bound)
            (ar-toarc (racket-quote (name)))))

(ail-code (racket-define bound
  (racket-let ((undef (list (racket-quote undef))))
    (racket-lambda (name)
      (ar-tnil
       (racket-not (racket-eq? (ar-var name undef) undef)))))))

(ail-code (ar-def ar-tostringf (f)
  (racket-let ((port (racket-open-output-string)))
    (racket-parameterize ((racket-current-output-port port))
      (f))
    (racket-get-output-string port))))

(ail-code (racket-hash-set! sig (racket-quote ar-disp)
            (ar-toarc (racket-quote (x (o port stdout))))))

(ail-code (racket-define (ar-disp x (port (racket-current-output-port)))
  (racket-display x port)
  (racket-flush-output port)
  x))

(ail-code (racket-hash-set! sig (racket-quote ar-write)
            (ar-toarc (racket-quote (x (o port stdout))))))

(ail-code (racket-define (ar-write x (port (racket-current-output-port)))
  (racket-write x port)
  (racket-flush-output port)
  x))

(ail-code (racket-hash-set! sig (racket-quote table)
            (ar-toarc (racket-quote ((o init))))))

(ail-code (racket-define (table (init nil))
  (racket-let ((h (racket-make-hash)))
    (racket-when (ar-true init) (init h))
    h)))

(ail-code (ar-def sref (com val ind)
  (racket-cond
   ((racket-hash? com)
    (racket-if (ar-no val)
                (racket-hash-remove! com ind)
                (racket-hash-set! com ind val)))
   ((racket-string? com)
    (racket-string-set! com ind val))
   ((racket-mpair? com)
    (racket-set-mcar! (racket-mlist-tail com ind) val))
   (racket-else
    (err "Can't set reference" com ind val)))
  val))

(ail-code (ar-def on-err (errf f)
  (racket-with-handlers ((racket-exn:fail? errf))
    (f))))

(ail-code (ar-def details (c)
  (racket-exn-message c)))

(ail-code (ar-def parameter (init)
  (racket-make-parameter init)))

(ail-code (ar-def ar-read-square-brackets (ch port src line col pos)
  (racket-quasiquote
   (square-bracket
    (racket-unquote-splicing (racket-read/recursive port #\[ #f))))))

(ail-code (ar-def ar-bracket-readtable (readtable)
  (racket-make-readtable readtable #\[ (racket-quote terminating-macro)
    ar-read-square-brackets)))

(ail-code (racket-define arc-readtable* (ar-bracket-readtable #f)))
