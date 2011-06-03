#!/bin/bash
set -e -v

./run --racket mzscheme ar.arc ac.arc arc.arc ar-test.arc
./run --racket racket   ar.arc ac.arc arc.arc ar-test.arc

./run --racket mzscheme ar.arc ac.arc arc.arc ac-test.arc
./run --racket racket   ar.arc ac.arc arc.arc ac-test.arc

./run --racket mzscheme ar.arc ac.arc arc.arc equal-wrt-testing.arc test.arc arc.t
./run --racket racket   ar.arc ac.arc arc.arc equal-wrt-testing.arc test.arc arc.t

./run --racket mzscheme ar.arc ac.arc arc.arc io.arc equal-wrt-testing.arc test.arc io.t
./run --racket racket   ar.arc ac.arc arc.arc io.arc equal-wrt-testing.arc test.arc io.t

sudo ./run --racket `which mzscheme` ar.arc ac.arc arc.arc io.arc equal-wrt-testing.arc test.arc io-root.t
sudo ./run --racket `which racket`   ar.arc ac.arc arc.arc io.arc equal-wrt-testing.arc test.arc io-root.t

./run --racket mzscheme ar.arc ac.arc arc.arc arc3.1/strings.arc equal-wrt-testing.arc test.arc strings.t
./run --racket racket   ar.arc ac.arc arc.arc arc3.1/strings.arc equal-wrt-testing.arc test.arc strings.t

./arc-script-test.pl

./run --racket mzscheme ar.arc ac.arc arc.arc defcall.arc equal-wrt-testing.arc test.arc defcall.t
./run --racket racket   ar.arc ac.arc arc.arc defcall.arc equal-wrt-testing.arc test.arc defcall.t
