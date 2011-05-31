#!/bin/bash
set -e -v
./arc --no-repl ar-test.arc
./arc --no-repl ac-test.arc
./arc-test.sh
./io-test.sh
./io-test-root.sh
./strings-test.sh
./arc-script-test.pl
