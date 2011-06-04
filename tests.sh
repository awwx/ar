#!/bin/bash
set -e -v

./run-test.pl

./run --racket mzscheme arc ar-test
./run --racket racket   arc ar-test

./run --racket mzscheme arc ac-test
./run --racket racket   arc ac-test

./run --racket mzscheme arc equal-wrt-testing test arc.t
./run --racket racket   arc equal-wrt-testing test arc.t

./run --racket mzscheme arc io equal-wrt-testing test io.t
./run --racket racket   arc io equal-wrt-testing test io.t

sudo ./run --racket `which mzscheme` arc io equal-wrt-testing test io-root.t
sudo ./run --racket `which racket`   arc io equal-wrt-testing test io-root.t

./run --racket mzscheme arc strings equal-wrt-testing test strings.t
./run --racket racket   arc strings equal-wrt-testing test strings.t

./arc-script-test.pl

./run --racket mzscheme arc defcall equal-wrt-testing test defcall.t
./run --racket racket   arc defcall equal-wrt-testing test defcall.t
