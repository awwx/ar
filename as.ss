#!/usr/bin/env racket
#lang scheme

(require "ac.ss")

(let ((arc (new-arc)))
  (aload arc "arc.arc" "repl.arc")
  (noprint ((get arc 'repl))))
