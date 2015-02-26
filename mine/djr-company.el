(use-package djr-lisps)

(use-package company
  :ensure t
  :init
  (dolist (mode-hook djr-lisp-mode-hooks)
    (add-hook mode-hook #'company-mode))
  :config
  (setq company-idle-delay 0))

(provide 'djr-company)
