#!/bin/bash

emacs --no-site-file -batch -L ./lib -L . -l ert -l tests/test_import.el -f ert-run-tests-batch-and-exit