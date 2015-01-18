(use-package ace-jump-mode
  :ensure t
  :commands ace-jump-mode
  :init
  (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode))

(provide 'djr-ace-jump)
