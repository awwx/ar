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
