(ail-code (racket-require (racket-prefix-in racket- scheme/port)))

(def limited-input-port (in maxbytes)
  (racket-make-limited-input-port in maxbytes (ail-code #t)))

(def socket-accept (s)
  (ail-code
    (racket-call-with-values
      (racket-lambda () (racket-tcp-accept s))
      (racket-lambda (in out)
        (racket-let ((in1 (limited-input-port in 100000)))
        (list in1
              out
              (racket-let-values (((us them) (racket-tcp-addresses out)))
                them)))))))

(mac rmodule (language . body)
  (w/uniq module
    (eval `(ail-code (racket-module ,module ,language ,@body))) 
    (racket-module-ref `',module)))

(def setuid (uid)
  ((inline
    ((rmodule scheme
       (require (lib "foreign.ss"))
       (unsafe!)
       (provide setuid)
       (define setuid (get-ffi-obj 'setuid #f (_fun _int -> _int))))
     'setuid))
   uid))

(def dir (name)
  (ar-toarc
   (ail-code
    (racket-map racket-path->string (racket-directory-list name)))))

(def rmfile (name)
  (racket-delete-file name)
  nil)

(def client-ip (port)
  (ail-code (racket-let-values (((x y) (racket-tcp-addresses port)))
              y)))

(def dead (thd)
  (aracket-true (racket-thread-dead? thd)))
