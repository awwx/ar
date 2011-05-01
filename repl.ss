;; This is a simple REPL (it doesn't even catch errors) which doesn't
;; rely on Arc being up and running, and so is useful for working on
;; the compiler.
;;
;; Run as.ss to get the full featured REPL.

#lang scheme

(require "ac.ss")

(let ((arc (new-arc)))
  (aload arc "arc.arc")
  (let loop ()
    (display "arc> ")
    (let ((v (arc-read arc (current-input-port))))
      (unless (eof-object? v)
        (let ((expr (toarc v)))
          (let ((val ((get arc 'eval) expr)))
            (write (deep-fromarc val))
            (newline)))
        (loop)))))
