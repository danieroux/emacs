(use-package flycheck
  :ensure t
  :defer t
  :idle (add-hook 'after-init-hook #'global-flycheck-mode)
  :pin "melpa"
  :config
  (progn
    (setq flycheck-completion-system 'ido)))

(provide 'djr-flycheck)
