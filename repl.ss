#lang scheme

(require "ac.ss")

(define arc* (new-ac))

(aload arc* "arc.arc")

(let loop ()
  (display "arc> ")
  (let ((v (arc-read arc* (current-input-port))))
    (unless (eof-object? v)
      (let ((expr (toarc v)))
        (let ((val ((hash-ref arc* 'eval) expr)))
          (write (deep-fromarc val))
          (newline)))
      (loop))))
