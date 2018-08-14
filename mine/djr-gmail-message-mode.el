(use-package djr-chrome-editboxes)

;; Sometimes, very useful
(use-package gmail-message-mode
  :ensure t
  :commands gmail-message-mode
  :pin "melpa"

  :config

  (setq ham-mode-markdown-to-html-command  '("/usr/local/bin/pandoc" "--from" "markdown" "--to" "html" file)))

(provide 'djr-gmail-message-mode)
