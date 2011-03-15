#lang scheme

(require "ac.ss")

(aload (new-ac)
  "arc.arc"
  "arc3.1/strings.arc"
  "equal-wrt-testing.arc" "test.arc"
  "strings.t")
