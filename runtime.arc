(use arc extend-ontype)

(defrule type (ar-tnil (racket-namespace? x))
  'runtime)

(with (orig bound default (list 'default))
  (redef bound (name (o runtime))
    (if runtime
          (isnt (racket-namespace-variable-value
                   name (ail-code #t) (fn () default) runtime)
                 default)
          (orig name))))

(extend-ontype ar-apply-non-fn runtime (runtime (varname))
  (ail-code (racket-namespace-variable-value varname #t #f runtime)))

(extend-ontype sref runtime (runtime value varname)
  (ail-code (racket-namespace-set-variable-value!
             varname value #t runtime)))

(def runtime ((o uses) (o usepath (usepath*)))
  (let runtime (new-runtime usepath)
    (each item uses
      (runtime!use-apply item))
    runtime))
