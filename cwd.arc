(use arc)

(assign cwd
        (racket-make-derived-parameter racket-current-directory
           idfn
           (fn (path) (racket-path->string path))))
(ac-zeroarg 'cwd)

(defrule ac-global-assign (is a 'cwd)
  `(racket-current-directory ,b))

(make-w/ cwd)
