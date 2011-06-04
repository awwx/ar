;; Allows another Arc runtime to be embedded within this one.
;;
;; The other runtime doesn't have to be this version of ar, it
;; could be a different hacked or experimental version, or one
;; built upon ar but running a different language than Arc.

(def make-empty-runtime (usepath)
  (let acpath (use-find "run.ss" usepath)
    ((ail-code (racket-dynamic-require (racket-string->path acpath)
                                       (racket-quote new-runtime)))
     usepath)))

(def runtime-access (runtime)
  (fn args
    (if (is len.args 1)
         (with (varname (car args))
             (ail-code (racket-namespace-variable-value
                        varname #t #f runtime)))
        (is len.args 2)
         (with (varname (car args)
                value   (cadr args))
           (ail-code (racket-namespace-set-variable-value!
                      varname value #t runtime)))
         (err "invalid number of arguments" arg))))

(def empty-runtime ((o usepath (usepath*)))
  (runtime-access (make-empty-runtime usepath)))
