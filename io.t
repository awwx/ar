(= tcp-test-port* 50013)

(def tcp-connect (host port)
  (ail-code "(racket-let-values (((i o) (racket-tcp-connect host port)))
               (list i o))"))

(with (ready (make-semaphore)
       their-ip nil
       the-client-ip nil)

  (thread
   ;; todo would like to catch errors here
   (w/socket s tcp-test-port*
     (racket-semaphore-post ready)
     (let (i o ip) (socket-accept s)
       (= their-ip ip)
       (= the-client-ip (client-ip o))
       (disp "foo" o)
       (racket-flush-output o)
       (close i o))))

  (racket-semaphore-wait ready)
  (testis (let (i o) (tcp-connect "127.0.0.1" tcp-test-port*)
            (string (n-of 3 (readc i))))
          "foo")
  (testis their-ip "127.0.0.1")
  (testis the-client-ip "127.0.0.1"))

(do (clean)
    (system (+ "touch " td "/one"))
    (system (+ "touch " td "/two"))
    (testis (sort < (dir td)) '("one" "two")))

(let f (+ td "/foo")
  (clean)
  (system (+ "touch " f))
  (rmfile f)
  (testis (dir td) '()))

(with (alive (make-semaphore)
       done  (make-semaphore))
  (let th (thread (racket-semaphore-post alive)
                  (racket-semaphore-wait done))
    (racket-semaphore-wait alive)
    (testis (dead th) nil)
    (racket-semaphore-post done)
    (catch (repeat 5
             (if (dead th) (throw nil))
             (sleep 0.1))
           (err "thread not dead"))
    (prn "ok thread dead")))

