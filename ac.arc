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
