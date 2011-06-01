;; todo merge into arc.arc

;; Not worrying about how ugly this is right now on the assumption
;; that I'll be rewriting it in Arc anyway.

(ail-code (racket-require (racket-prefix-in racket- scheme/tcp)))
(ail-code (racket-require (racket-prefix-in racket- scheme/port)))
(ail-code (racket-require (racket-prefix-in racket- scheme/mpair)))

(def socket-accept (s)
  (ail-code
    (racket-call-with-values
      (racket-lambda () (racket-tcp-accept s))
      (racket-lambda (in out)
        (racket-let ((in1 (racket-make-limited-input-port in 100000 #t)))
        (list in1
              out
              (racket-let-values (((us them) (racket-tcp-addresses out)))
                them)))))))

;; breaks the compiler to require foreign.ss into our namespace

(ail-code (racket-module setuid scheme
            (require (lib "foreign.ss"))
            (unsafe!)
            (provide setuid)
            (define setuid (get-ffi-obj 'setuid #f (_fun _int -> _int)))))

;; And this *is* ugly... but it has the advantage that it works.

(def setuid (uid)
  ((inline ((racket-module-ref ''setuid) 'setuid)) uid))

(def dir (name)
  (ar-toarc (racket (map path->string (directory-list name)))))

(def rmfile (name)
  (racket-delete-file name)
  nil)

(def client-ip (port)
  (ail-code (let-values (((x y) (tcp-addresses port)))
              y)))

(def dead (thd)
  (aracket-true (racket-thread-dead? thd)))
