(each (name signature)
      (pair
       '(-               args
         /               args
         *               args
         ar-list-fromarc (x)
         ar-r/list-toarc (x)
         ar-toarc        (x)
         cons            (a b)
         inside          (s)
         instring        (str)
         list            args
         outstring       ()
         uniq            ()))
  (= sig.name signature))
