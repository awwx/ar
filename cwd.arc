(use arc)

(make-dynamic 'cwd
  (racket-make-derived-parameter racket-current-directory
     idfn
     (fn (path) (racket-path->string path))))

(make-w/ cwd)
