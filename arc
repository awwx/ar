#!/usr/bin/env racket
#lang racket/load

(require racket/cmdline)

(define run-repl #t)
(define files-to-load '())

(command-line
 #:once-each
 (("--no-repl")
  "do not run the REPL"
  (set! run-repl #f))

 #:args files
 (set! files-to-load files))

(define arcdir*
  (path->string
   (let-values (((base _2 _3)
                 (split-path (normalize-path
                              (find-system-path 'run-file)))))
     base)))

(namespace-require `(file ,(string-append arcdir* "ac.ss")))

(let ((arc (new-arc arcdir*)))
  ((runtime-get arc 'ar-load) (string-append arcdir* "arc.arc"))
  ((runtime-get arc 'load) (string-append arcdir* "arc3.1/strings.arc"))
  (for-each (runtime-get arc 'load) files-to-load)
  (when run-repl
    ((runtime-get arc 'load) (string-append arcdir* "repl.arc"))
    ((runtime-get arc 'repl)))
  (void))
