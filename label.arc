(use arc)

(def ltable-name (lname)
  (sym (+ "label-table-" lname "*")))

(mac getlabel (lname obj)
  `(racket-hash-ref ,(ltable-name lname) ,obj nil))

(mac setlabel (lname obj val)
  (w/uniq gval
    `(let ,gval ,val
       ((ail-code racket-hash-set!) ,(ltable-name lname) ,obj ,gval)
       ,gval)))

(mac deflabel (lname)
  `(do (= ,(ltable-name lname) (racket-make-weak-hasheq))
       (def ,lname (obj)
         (getlabel ,lname obj))
       (defset ,lname (x)
         (w/uniq g
           (list (list g x)
                 `(getlabel ,',lname ,g)
                 `(fn (val) (setlabel ,',lname ,g val)))))))
