(use arc)

(= call-types* (table))

(defrule ar-apply-non-fn (racket-hash-ref call-types* type.x (fn () nil))
  (apply it x args))

(mac defcall (type args . body)
  `(= (call-types* ',type) (fn ,args ,@body)))
