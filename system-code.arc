(use arc)

(ail-code (racket-require (racket-prefix-in racket- scheme/system)))

(def system-code args
  (apply racket-system*/exit-code args))

(def check-system args
  (let code (apply system-code args)
    (when (isnt code 0)
      (err "failed" code args))
    t))
