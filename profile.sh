#!/bin/sh

/Applications/Emacs.app/Contents/MacOS/Emacs -Q --eval '(setq *my-primary-emacs-instance* t profile-dotemacs-file "mine/djr-init.el")' -l profile-dotemacs.el -f profile-dotemacs
