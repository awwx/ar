;; Allows another Arc runtime to be embedded within this one.
;;
;; The other runtime doesn't have to be this version of ar, it
;; could be a different hacked or experimental version, or one
;; built upon ar but running a different language than Arc.
;;
;; Example:
;;
;; arc> (= a (new-arc))
;; #<procedure>
;; arc> a!+
;; #<procedure:+>
;; arc> (a!ar-load "arc.arc")
;; nil
;; arc> (a!eval '(map odd '(1 2 3 4 5 6)))
;; (t nil t nil t nil)

(def make-empty-runtime ((o arcdir))
  (let acpath (string (or arcdir arcdir*) "/ac.ss")
    ((racket (racket-dynamic-require (racket-string->path acpath)
                                     (racket-quote new-runtime))))))

(def arc-runtime ((o arcdir) (o options))
  (let acpath (string (or arcdir arcdir*) "/ac.ss")
    ((racket
      (racket-dynamic-require
       (racket-string->path acpath)
       (racket-quote new-arc)))
     arcdir)))

(def runtime-access (runtime)
  (fn args
    (if (is len.args 1)
         (with (varname (car args))
             (racket (racket-namespace-variable-value
                      varname #t #f runtime)))
        (is len.args 2)
         (with (varname (car args)
                value   (cadr args))
           (racket (racket-namespace-set-variable-value!
                    varname value #t runtime)))
         (err "invalid number of arguments" arg))))

(def empty-runtime ((o arcdir))
  (runtime-access (make-empty-runtime)))

(def new-arc ((o arcdir))
  (runtime-access (arc-runtime arcdir)))
