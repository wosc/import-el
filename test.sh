#!/bin/bash

emacs24 --no-site-file -batch -L ./lib -L . -l ert -l test.el -f ert-run-tests-batch-and-exit
