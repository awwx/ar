#lang scheme

(require "ac.ss")

(let ((arc (new-arc)))
  ((get arc 'ar-load) "arc.arc" "equal-wrt-testing.arc" "test.arc" "arc.t"))
