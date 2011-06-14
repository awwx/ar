(use arc extend-ontype)

(defrule type (ar-tnil (racket-namespace? x))
  'runtime)

(extend-ontype ar-apply-non-fn runtime (runtime (varname))
  (ail-code (racket-namespace-variable-value varname #t #f runtime)))

(extend-ontype sref runtime (runtime value varname)
  (ail-code (racket-namespace-set-variable-value!
             varname value #t runtime)))

(def runtime ((o uses) (o usepath (usepath*)))
  (let runtime (new-runtime usepath)
    (each item uses
      (runtime!use-load item))
    runtime))
