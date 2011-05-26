(each (name signature)
      (pair
       '(-               args
         /               args
         *               args
         ar-deep-fromarc (x)
         ar-list-fromarc (x)
         ar-r/list-toarc (x)
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
         uniq            ()))
  (= sig.name signature))
