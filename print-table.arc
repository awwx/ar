(use arc)

(defrule print (isa x 'table)
  (disp "#table" port)
  (write (accum a
          (each key (sort (fn (a b) (errsafe (< a b))) (keys x))
            (a (list key x.key))))
         port))
