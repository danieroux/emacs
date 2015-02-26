(use-package djr-lisps)

(use-package paredit-mode
  :commands paredit-mode
  :ensure paredit
  :init
  (dolist (mode-hook djr-lisp-mode-hooks)
    (add-hook mode-hook #'paredit-mode)))

(provide 'djr-paredit)
