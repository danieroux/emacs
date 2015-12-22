(use-package djr-completing-read)

(use-package magit
  :pin "melpa"
  :commands magit-status
  :ensure t
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(provide 'djr-magit)
