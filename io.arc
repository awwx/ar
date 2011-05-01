;; todo merge into arc.arc

(= custodians* (table))

(def associate-custodian (c i o)
  (= (custodians* i) c)
  (= (custodians* o) c))

;; Not worrying about how ugly this is right now on the assumption
;; that I'll be rewriting it in Arc anyway.

(racket (require scheme/tcp))
(racket (require scheme/port))
(racket (require scheme/mpair))
(racket (require (only-in "ar.ss" arc-list)))

(def socket-accept (s)
  (let associate-custodian associate-custodian
    (racket "
      (let ((oc (current-custodian))
            (nc (make-custodian)))
         (current-custodian nc)
         (call-with-values
           (lambda () (tcp-accept s))
           (lambda (in out)
             (let ((in1 (make-limited-input-port in 100000 #t)))
               (current-custodian oc)
               (associate-custodian nc in1 out)
               (arc-list in1
                         out
                         (let-values (((us them) (tcp-addresses out)))
                           them))))))
   ")))

;; breaks the compiler to require foreign.ss into our namespace

(racket (module setuid scheme
          (require (lib "foreign.ss"))
          (unsafe!)
          (provide setuid)
          (define setuid (get-ffi-obj 'setuid #f (_fun _int -> _int)))))

;; And this *is* ugly... but it has the advantage that it works.

(def setuid (uid)
  ((inline ((racket-module ''setuid) 'setuid)) uid))

(def dir (name)
  (ar-toarc (racket (map path->string (directory-list name)))))

(def rmfile (name)
  (racket.delete-file name)
  nil)

(def client-ip (port)
  (racket (let-values (((x y) (tcp-addresses port)))
            y)))

(def dead (thd)
  (aracket-true (racket.thread-dead? thd)))

(def try-custodian (port)
  (whenlet custodian (custodians* port)
    (racket.custodian-shutdown-all custodian)
    (wipe (custodians* port))
    t))

(def force-close args
  (each port args
    (or (try-custodian port) (close port))))
