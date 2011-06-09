(use arc)

(mac values (vars expr . body)
  (w/uniq (i o)
    `(with (,i (fn () ,expr)
            ,o (fn ,vars ,@body))
       (ail-code
        (racket-call-with-values ,i ,o)))))
