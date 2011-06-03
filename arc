#!/usr/bin/env racket
#lang scheme/load

(require scheme/cmdline)

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

(namespace-require `(file ,(string-append arcdir* "arc.ss")))

(let ((arc (new-arc arcdir*)))
  (load arc arcdir* "arc.arc")
  (load arc arcdir* "arc3.1/strings.arc")
  (for-each (runtime-get arc 'load) files-to-load)
  (when run-repl
    (load arc arcdir* "repl.arc")
    ((runtime-get arc 'repl)))
  (void))
