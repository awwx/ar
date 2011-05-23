#!/bin/bash
set -e -v
racket ac-test.ss
racket arc-test.ss
racket io-test.ss
sudo `which racket` io-test-root.ss
racket strings-test.ss
