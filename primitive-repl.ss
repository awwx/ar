;; This is a simple REPL (it doesn't even catch errors) which doesn't
;; rely on Arc being up and running, and so is useful for working on
;; the compiler.
;;
;; Run arc to get the full featured REPL.

#lang scheme

(require "ac.ss")

(define arcdir*
  (path->string
   (let-values (((base _2 _3)
                 (split-path (normalize-path
                              (find-system-path 'run-file)))))
     base)))

(define eof (list 'eof))

(let ((arc (new-arc arcdir*)))
  ((runtime-get arc 'ar-load) "arc.arc")
  (let loop ()
    (display "arc> ")
    (let ((v ((runtime-get arc 'ar-read) (current-input-port) eof)))
      (unless (eq? v eof)
        (let ((val ((runtime-get arc 'eval) v)))
          (write ((runtime-get arc 'ar-deep-fromarc) val))
          (newline))
        (loop)))))
