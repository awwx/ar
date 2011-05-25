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

(let ((arc (new-arc arcdir*)))
  ((get arc 'ar-load) "arc.arc")
  (let loop ()
    (display "arc> ")
    (let ((v ((get arc 'ar-read) (current-input-port))))
      (unless (eof-object? v)
        (let ((expr (toarc v)))
          (let ((val ((get arc 'eval) expr)))
            (write (ar-deep-fromarc val))
            (newline)))
        (loop)))))
