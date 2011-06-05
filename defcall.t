(use defcall test)

(defcall char (c n)
  (string (n-of n c)))

(testis (#\A 6) "AAAAAA")
