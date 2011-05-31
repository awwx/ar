;; tests that need to be run as root

(def euid ()
  (tostring (system "id -u")))

(do (racket-current-directory "/")
    (testis (euid) "0\n")
    (setuid 13983)
    (testis (euid) "13983\n"))
