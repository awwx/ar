#!/usr/bin/env racket
#lang scheme

(require "ac.ss")

(let ((arc (new-ac)))
  (aload arc "arc.arc" "repl.arc")
  (noprint ((hash-ref arc 'repl))))
