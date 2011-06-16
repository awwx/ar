(use arc racket)

(def regexp (pattern)
   (if (isa pattern 'string) (racket-pregexp pattern) pattern))

(def re-match (pattern (o in stdin))
  (let result ((if (isa in 'string)
                    racket-regexp-match
                    racket-regexp-try-match)
               (regexp pattern) in)
    (if (is result racket-#f)
         nil
         (map [if (racket-true (racket-bytes? _))
                   (racket-bytes->string/utf-8 _)
                   _]
              (ar-r/list-toarc result)))))

; This isn't anchored at the beginning of the input unless you use
; "^" yourself.

(def re-looking-at (pattern (o in stdin))
  (racket-true:racket-regexp-match-peek (regexp pattern) in))
