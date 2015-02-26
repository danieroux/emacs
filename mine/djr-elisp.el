(use-package elisp-slime-nav
  :ensure t
  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode)))

(require 'info-look)

(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

(provide 'djr-elisp)
