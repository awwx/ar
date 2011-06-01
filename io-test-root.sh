#!/bin/bash
set -e -v
sudo ./mzscheme-arc --no-repl io.arc equal-wrt-testing.arc test.arc io-root.t
sudo `which racket` arc --no-repl io.arc equal-wrt-testing.arc test.arc io-root.t
