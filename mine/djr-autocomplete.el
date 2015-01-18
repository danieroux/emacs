(use-package auto-complete-config
  :ensure auto-complete
  :init
  (ac-config-default)
  :config
  (progn
    (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
    (add-to-list 'ac-modes 'emacs-lisp-mode)))

(provide 'djr-autocomplete)
