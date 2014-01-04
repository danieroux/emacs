(djr/ensure-package 'projectile)
(djr/ensure-package 'helm-projectile)

(require 'helm-projectile)

(projectile-global-mode)

;; Running C-u C-c p f will invalidate the cache prior to prompting you for a file to jump to.
(setq projectile-enable-caching t)

(provide 'djr-projectile)
