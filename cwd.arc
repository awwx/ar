(use arc)

(sref dynamic-parameter* racket-current-directory 'cwd)

(defvar cwd
  (fn () (racket-path->string (racket-current-directory)))
  (fn (v) (racket-current-directory v)))

(make-w/ cwd)
