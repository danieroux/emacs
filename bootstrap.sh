#!/bin/sh
# Run on a clean checkout - this gets everything ready for the first launch

cd $(dirname $0)
git submodule init && git submodule update --init
cd external/org-mode && make autoloads && cd -
emacs -nw -l mine/djr-defuns.el -f djr/bootstrap

echo "Now launch emacs normally - it should install all the ELPA packages it need"
