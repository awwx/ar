#lang scheme

;; tests that need to be run as root

(require "ac.ss")

(let ((arc (new-arc)))
  ((get arc 'ar-load)
   "arc.arc"
   "io.arc"
   "equal-wrt-testing.arc" "test.arc"
   "io-root.t"))
