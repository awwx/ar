#lang scheme

(require "ac.ss")

(define arcdir*
  (path->string
   (let-values (((base _2 _3)
                 (split-path (normalize-path
                              (find-system-path 'run-file)))))
     base)))

(let ((arc (new-arc arcdir*)))
  ((runtime-get arc 'ar-load)
   "arc.arc"
   "io.arc"
   "equal-wrt-testing.arc" "test.arc"
   "io.t"))
