#lang scheme

;; tests that need to be run as root

(require "ac.ss")

(aload (new-arc) "arc.arc" "io.arc" "equal-wrt-testing.arc" "test.arc"
       "io-root.t")
