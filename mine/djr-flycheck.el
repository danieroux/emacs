(use-package flycheck
  :ensure t
  :pin melpa
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))

(provide 'djr-flycheck)
