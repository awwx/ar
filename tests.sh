#!/bin/bash
set -e -v
racket ac-test.ss
./arc --no-repl ar-test.arc
./arc --no-repl ac-test.arc
racket arc-test.ss
racket io-test.ss
sudo `which racket` io-test-root.ss
racket strings-test.ss
./arc-script-test.pl
