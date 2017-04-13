(use-package deft
  :defer t
  :ensure t
  :config
  (progn
    (setq deft-extensions '("md" "txt")
          deft-default-extension "md"
          deft-text-mode 'markdown-mode
          deft-use-filter-string-for-filename t
          deft-use-filename-as-title t
          deft-directory (expand-file-name "~/Dropbox/book-of-insignificant-movements"))))

(provide 'djr-deft)
