(djr/ensure-package 'auto-complete)

(require 'auto-complete-config)
(ac-config-default)

(add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
(add-to-list 'ac-modes 'emacs-lisp-mode)

(auto-complete-mode 1)

(provide 'djr-autocomplete)
