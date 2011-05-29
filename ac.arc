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
    (racket-let ((v (ar-var fn (racket-lambda () nil))))
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
