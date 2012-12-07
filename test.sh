#!/bin/bash

emacs -batch -L ./lib -l ert -l tests/test_import.el -f ert-run-tests-batch-and-exit