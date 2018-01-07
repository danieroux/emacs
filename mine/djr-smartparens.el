(use-package djr-lisps)

(use-package smartparens
  :ensure t
  :pin "melpa"
  :init
  (require 'smartparens-config)
  (sp-use-paredit-bindings)
  (hook-into-modes #'smartparens-strict-mode djr-lisp-mode-hooks))

(use-package evil-smartparens
  :ensure t
  :pin "melpa"
  :init (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(provide 'djr-smartparens)
