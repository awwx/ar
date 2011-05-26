(each (name signature)
      (pair
       '(-               args
         /               args
         *               args
         <2              (x y)
         <               args
         >2              (x y)
         >               args
         +               args
         annotate        (totype rep)
         ar-deep-fromarc (x)
         ar-exint        (x)
         ar-iround       (x)
         ar-list-fromarc (x)
         ar-no           (x)
         ar-pairwise     (pred lst)
         ar-r/list-toarc (x)
         ar-tagged       (x)
         ar-tnil         (x)
         ar-toarc        (x)
         ar-true         (x)
         car             (x)
         cadr            (x)
         caris           (x val)
         cdr             (x)
         cddr            (x)
         coerce          (x totype . args)
         cons            (a b)
         err             args
         inside          (s)
         instring        (str)
         is              args
         is2             (a b)
         join            args
         len             x
         list            args
         list-len        x
         map1            (f xs)
         outstring       ()
         rep             (x)
         type            (x)
         uniq            ()))
  (= sig.name signature))
