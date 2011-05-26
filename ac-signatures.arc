(each (name signature)
      (pair
       '(-               args
         /               args
         *               args
         annotate        (totype rep)
         ar-deep-fromarc (x)
         ar-exint        (x)
         ar-list-fromarc (x)
         ar-r/list-toarc (x)
         ar-tagged       (x)
         ar-toarc        (x)
         car             (x)
         cadr            (x)
         cdr             (x)
         cddr            (x)
         cons            (a b)
         err             args
         inside          (s)
         instring        (str)
         list            args
         outstring       ()
         rep             (x)
         type            (x)
         uniq            ()))
  (= sig.name signature))
