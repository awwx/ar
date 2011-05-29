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
