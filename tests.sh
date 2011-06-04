#!/bin/bash
set -e -v

./run --racket mzscheme arc.arc ar-test.arc
./run --racket racket   arc.arc ar-test.arc

./run --racket mzscheme arc.arc ac-test.arc
./run --racket racket   arc.arc ac-test.arc

./run --racket mzscheme arc.arc equal-wrt-testing.arc test.arc arc.t
./run --racket racket   arc.arc equal-wrt-testing.arc test.arc arc.t

./run --racket mzscheme arc.arc io.arc equal-wrt-testing.arc test.arc io.t
./run --racket racket   arc.arc io.arc equal-wrt-testing.arc test.arc io.t

sudo ./run --racket `which mzscheme` arc.arc io.arc equal-wrt-testing.arc test.arc io-root.t
sudo ./run --racket `which racket`   arc.arc io.arc equal-wrt-testing.arc test.arc io-root.t

./run --racket mzscheme arc.arc arc3.1/strings.arc equal-wrt-testing.arc test.arc strings.t
./run --racket racket   arc.arc arc3.1/strings.arc equal-wrt-testing.arc test.arc strings.t

./arc-script-test.pl

./run --racket mzscheme arc.arc defcall.arc equal-wrt-testing.arc test.arc defcall.t
./run --racket racket   arc.arc defcall.arc equal-wrt-testing.arc test.arc defcall.t
