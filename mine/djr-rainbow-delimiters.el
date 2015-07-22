(use-package djr-lisps)

(use-package rainbow-delimiters
  :ensure t
  :init
  (dolist (mode-hook djr-lisp-mode-hooks)
    (add-hook mode-hook #'rainbow-delimiters-mode))
  :config
  ;; http://timothypratley.blogspot.com/2015/07/seven-specialty-emacs-settings-with-big.html
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error)
  :pin "melpa")

(provide 'djr-rainbow-delimiters)
