(use arc cwd values re)

(ail-code (racket-require (racket-prefix-in racket- scheme/path)))

(def path xs
  (string (intersperse "/" xs)))

(def dirpart (path)
  (values (dir _1 _2) (racket-split-path path)
    (if (ar-tnil (racket-path? dir)) (racket-path->string dir))))

(def filepart (path)
  (aif (ar-fnil (racket-file-name-from-path path))
        (racket-path->string it)))

(def extension (path)
  (aand (filepart path)
        (cadr (re-match "\\.([^\\.]+)$" it))))

(def fullpath (path (o basedir cwd))
  (racket-path->string
   (racket-simplify-path
     (racket-path->complete-path
      (racket-expand-user-path path)
      basedir))))
