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

(define srcdir*
  (path->string
   (let-values (((base _2 _3)
                 (split-path (normalize-path
                              (find-system-path 'run-file)))))
     base)))

(namespace-require `(file ,(string-append srcdir* "ac.ss")))

(let ((arc (new-arc)))
  (aload arc (string-append srcdir* "arc.arc"))
  ((get arc 'load) (string-append srcdir* "arc3.1/strings.arc"))
  (for-each (get arc 'load) files-to-load)
  (when run-repl
    ((get arc 'load) (string-append srcdir* "repl.arc"))
    (noprint ((get arc 'repl)))))
