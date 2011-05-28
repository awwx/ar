(load "equal-wrt-testing.arc")
(load "test.arc")
(load "embed.arc")

(def matches (pattern form)
  ;; todo the cadr is cheating; what I really want is a recursive
  ;; match
  (iso (firstn len.pattern cadr.form) pattern))

(def ac-upto (pattern)
  (prn)
  (write pattern) (prn)
  (let arc (new-arc (racket-path->string (racket-current-directory)))
    (arc!ar-ail-load arc!arc* "ar.ail")
    (arc!ar-load "ac.arc")
    (catch
      (each form (readfile "ac.arc")
        (arc!eval form)
        (when (matches pattern form) (throw nil)))
      (err "pattern not found in source" pattern))
    arc))

(mac testfor (pattern . body)
  `(let a (ac-upto ',pattern)
     ,@body))

(testfor (ar-def ac-literal? (x))
  (testis (a!eval 123)   123)
  (testis (a!eval #\a)   #\a)
  (testis (a!eval "abc") "abc")
  (testis (a!eval car)   car))

(testfor (ar-def ac-lex? (v env))
  (testis (a!ac-lex? 'y '(x y z)) t)
  (testis (a!ac-lex? 'w '(x y z)) nil))
