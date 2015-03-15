(use-package djr-lisps)

(use-package rainbow-delimiters
  :ensure t
  :init
  (dolist (mode-hook djr-lisp-mode-hooks)
    (add-hook mode-hook #'rainbow-delimiters-mode))
  :pin "melpa")

(provide 'djr-rainbow-delimiters)
