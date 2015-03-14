(use-package djr-lisps)

;; http://oremacs.com/lispy
(use-package lispy
  :ensure t
  :pin melpa
  :config
  (hook-into-modes #'lispy-mode djr-lisp-mode-hooks))

;;;###autoload
(defun djr-lispy-load ())

(provide 'djr-lispy)
