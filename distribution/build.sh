#!/usr/bin/env bash

WORK_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"


sbcl --noinform --non-interactive --noprint \
     --eval "(ql:quickload :cl-bodge/distribution :silent t)" \
     --eval "(load \"$1\")" \
     --eval "(ge.dist:make-distribution :$2)"
