;; This primitive REPL doesn't catch errors, but it only needs to load
;; ar to work.  Thus it can be useful for working on the Arc compiler.
;;
;; Run the "repl" script to get the full featured REPL.

(use ar)

(ail-code
 (racket-define eof (racket-list (racket-quote eof)))

 (racket-define (repl)
   (racket-display ">> ")
   (racket-let ((v (ar-read (racket-current-input-port) eof)))
     (racket-unless (racket-eq? v eof)
       (racket-let ((val (eval v)))
         (racket-write (ar-deep-fromarc val))
         (racket-newline))
       (repl))))

 (repl))
