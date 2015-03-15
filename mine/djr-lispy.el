(use-package djr-lisps)

;; http://oremacs.com/lispy
(use-package lispy
  :ensure t
  :defer t
  :pin melpa
  :init
  (hook-into-modes #'lispy-mode djr-lisp-mode-hooks))

(provide 'djr-lispy)
