#!/bin/bash
set -e -v
./mzscheme-arc --no-repl ar-test.arc
./arc          --no-repl ar-test.arc

./mzscheme-arc --no-repl ac-test.arc
./arc --no-repl ac-test.arc

ARC=./mzscheme-arc ./arc-test.sh
ARC=./arc ./arc-test.sh

ARC=./mzscheme-arc ./io-test.sh
ARC=./arc ./io-test.sh

./io-test-root.sh

ARC=./mzscheme-arc ./strings-test.sh
ARC=./arc ./strings-test.sh

./arc-script-test.pl
