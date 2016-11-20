#!/bin/bash

echo "Opening .emacs profiler..."
emacs -Q -l ~/.emacs.d/lisp/profile-dotemacs/profile-dotemacs.el -f profile-dotemacs &
