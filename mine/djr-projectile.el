(use-package projectile
  :ensure t
  :pin "melpa"

  :idle
  (projectile-global-mode)

  :config
  (progn 
    (setq projectile-enable-caching t)

    (use-package helm-projectile
      :ensure t
      :bind* ("M-S-SPC" . helm-projectile)
      :pin "melpa")

    (use-package ag 
      :commands (projectile-ag ag/search)
      :ensure t)))

(provide 'djr-projectile)
