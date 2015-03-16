(use-package flycheck
  :ensure t
  :pin "melpa"
  :defer t
  :init (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (progn
    (setq flycheck-completion-system 'ido)))

(provide 'djr-flycheck)
