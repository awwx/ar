(each (name signature)
      (pair
       '(-               args
         /               args
         ar-r/list-toarc (x)
         *               args
         cons            (a b)
         inside          (s)
         instring        (str)
         list            args
         outstring       ()
         uniq            ()))
  (= sig.name signature))
