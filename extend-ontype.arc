(use arc)

(mac extend-ontype (name type arglist . body)
  (w/uniq (x rest)
    `(extenddef ',name (fn (x . rest) (isa x ',type))
       (fn args
         (apply (fn ,(join '(orig) arglist) ,@body)
                (cons (car args) (cddr args)))))))
