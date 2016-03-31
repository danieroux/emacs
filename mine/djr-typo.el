(use-package typo
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'text-mode-hook 'typo-mode)
    (add-hook 'mu4e-compose-hook 'typo-mode))
  :config (setq-default typo-language "English"))

(provide 'djr-typo)
