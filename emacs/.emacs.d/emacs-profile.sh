#!/bin/bash

echo -e "1)GUI\n2)Terminal\nPlease choose an Emacs to profile."
read type

if [[ $type -eq 1 ]]
then
    echo "Opening .emacs profiler for GUI..."
    emacs -Q -l ~/.emacs.d/lisp/profile-dotemacs/profile-dotemacs.el -f profile-dotemacs &
elif [[ $type -eq 2 ]]
then
    echo "Opening .emacs profiler for terminal..."
    emacs -Q -nw -l ~/.emacs.d/lisp/profile-dotemacs/profile-dotemacs.el -f profile-dotemacs
else
    echo "Please enter 1 or 2."
fi
