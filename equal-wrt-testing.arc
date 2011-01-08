(def equal-wrt-testing (a b)
  (if (and (isa a 'exception) (isa b 'exception))
       (is (details a) (details b))
       (iso a b)))
