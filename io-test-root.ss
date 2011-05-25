#lang scheme

;; tests that need to be run as root

(require "ac.ss")

(define arcdir*
  (path->string
   (let-values (((base _2 _3)
                 (split-path (normalize-path
                              (find-system-path 'run-file)))))
     base)))

(let ((arc (new-arc arcdir*)))
  ((get arc 'ar-load)
   "arc.arc"
   "io.arc"
   "equal-wrt-testing.arc" "test.arc"
   "io-root.t"))
