#!/bin/bash
set -e -v

./run-test.pl

./run --racket mzscheme ar-test
./run --racket racket   ar-test

./run --racket mzscheme ac-test
./run --racket racket   ac-test

./run --racket mzscheme arc.t
./run --racket racket   arc.t

./run --racket mzscheme io.t
./run --racket racket   io.t

sudo ./run --racket `which mzscheme` io-root.t
sudo ./run --racket `which racket`   io-root.t

./run --racket mzscheme arc strings.t
./run --racket racket   arc strings.t

./arc-script-test.pl

./run --racket mzscheme defcall.t
./run --racket racket   defcall.t
