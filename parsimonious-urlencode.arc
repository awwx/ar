(use arc)

(def bytehex (i)
  (if (< i 16) (writec #\0))
  (pr (upcase:coerce i 'string 16)))

(def urlsafe (c)
  (or alphadig.c (in c #\- #\_ #\. #\~)))

(def charutf8 (c)
  (ar-r/list-toarc
   (racket-bytes->list (racket-string->bytes/utf-8 (string c)))))

(def parsimonious-urlencode (s (o urlsafe urlsafe))
 (tostring
   (each c s
     (if (is c #\space)
          (pr #\+)
         urlsafe.c
          (pr c)
          (each i charutf8.c
            (writec #\%)
            (bytehex i))))))
