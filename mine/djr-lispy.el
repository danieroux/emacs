(use-package djr-lisps)

;; http://oremacs.com/lispy
(use-package lispy
  :ensure t
  :defer t
  :pin "melpa"
  :init
  (hook-into-modes #'lispy-mode djr-lisp-mode-hooks)

  :config
  (define-key lispy-mode-map "g" nil)
  (define-key lispy-mode-map "u" nil)
  (setq lispy-visit-method 'helm-projectile
        lispy-teleport-global 1
        lispy-cider-connect-method 'cider-connect))

(provide 'djr-lispy)
