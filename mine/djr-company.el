(use-package djr-lisps)

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (progn
    (use-package company-emoji
      :ensure t
      :init (company-emoji-init))
    (setq company-idle-delay 0.2)))

(provide 'djr-company)
