(use-package projectile
  :ensure t
  :pin melpa
  :init
  (projectile-global-mode)
  :config
  ;; Running C-u C-c p f will invalidate the cache prior to prompting you for a file to jump to.
  (setq projectile-enable-caching t))

(use-package helm-projectile
  :ensure t
  :pin melpa)

(provide 'djr-projectile)
