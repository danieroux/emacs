(use-package paredit-mode
  :commands paredit-mode
  :ensure paredit
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
    (add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
    (add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
    (add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))
    (add-hook 'clojure-mode-hook          (lambda () (paredit-mode +1)))))

(provide 'djr-paredit)
