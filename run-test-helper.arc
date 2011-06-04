(let args (ail-code (ar-toarc
                     (racket-vector->list
                      (racket-current-command-line-arguments))))
  (write args))
