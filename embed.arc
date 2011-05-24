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

(def arc-runtime ((o arcdir) (o options))
  (with (acpath (string (or arcdir arcdir*) "/ac.ss")
         options (or options (table)))
    ((racket
      (racket-dynamic-require
       (racket-string->path acpath)
       (racket-quote new-arc)))
     options)))

(def new-arc ((o arcdir) (o options))
  (let runtime (arc-runtime arcdir options)
    (fn args
      (if (is len.args 1)
           (with (varname (car args))
             (let namespace runtime!racket-namespace*
               (racket (racket-namespace-variable-value
                        varname #t #f namespace))))
          (is len.args 2)
           (with (varname (car args)
                  value   (cadr args))
             (let namespace runtime!racket-namespace*             
               (racket (racket-namespace-set-variable-value!
                        varname value #t namespace))))
           (err "invalid number of arguments" arg)))))
