(use-package deft
  :defer t
  :ensure t
  :config
  (progn
    (setq deft-extensions '("txt")
          deft-text-mode 'markdown-mode
          deft-use-filter-string-for-filename t
          deft-use-filename-as-title t
          deft-directory (expand-file-name "~/Dropbox/owningyourslippers/manuscript"))))

(provide 'djr-deft)
