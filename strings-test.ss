#lang scheme

(require "ac.ss")

(let ((arc (new-arc)))
  ((get arc 'ar-load)
   "arc.arc"
   "arc3.1/strings.arc"
   "equal-wrt-testing.arc" "test.arc"
   "strings.t"))
