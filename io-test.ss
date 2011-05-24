#lang scheme

(require "ac.ss")

(let ((arc (new-arc)))
  ((get arc 'ar-load)
   "arc.arc"
   "io.arc"
   "equal-wrt-testing.arc" "test.arc"
   "io.t"))
