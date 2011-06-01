(ail-code (racket-require (racket-prefix-in racket- scheme/port)))

(def limited-input-port (in maxbytes)
  (racket-make-limited-input-port in maxbytes (ail-code #t)))

(mac values (vars expr . body)
  (w/uniq (i o)
    `(with (,i (fn () ,expr)
            ,o (fn ,vars ,@body))
       (ail-code
        (racket-call-with-values ,i ,o)))))

(def socket-accept (s)
  (values (in out) (racket-tcp-accept s)
    (list (limited-input-port in 100000)
          out
          (values (us them) (racket-tcp-addresses out) them))))

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
  (ar-toarc (racket-map racket-path->string (racket-directory-list name))))

(def rmfile (name)
  (racket-delete-file name)
  nil)

(def client-ip (port)
  (values (us them) (racket-tcp-addresses port) them))

(def dead (thd)
  (aracket-true (racket-thread-dead? thd)))
