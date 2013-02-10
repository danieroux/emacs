cd $(dirname $0)
git submodule init && git submodule update && emacs -q -l mine/djr-defuns.el -f djr/bootstrap
