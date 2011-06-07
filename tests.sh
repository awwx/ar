#!/bin/bash
set -e -v

./run-test.pl

mzscheme run ar-test
racket   run ar-test

mzscheme run ac-test
racket   run ac-test

mzscheme run arc.t
racket   run arc.t

mzscheme run io.t
racket   run io.t

sudo `which mzscheme` run io-root.t
sudo `which racket`   run io-root.t

mzscheme run arc strings.t
racket   run arc strings.t

./arc-script-test.pl

mzscheme run defcall.t
racket   run defcall.t

mzscheme run capture.t
racket   run capture.arc

mzscheme run test-by-example.t
racket   run test-by-example.t

mzscheme run extend-ontype.t
racket   run extend-ontype.t

mzscheme run runtime.t
racket   run runtime.t
