(use-package magit
  :commands magit-status
  :pin melpa
  :init
  (setq magit-completing-read-function 'magit-ido-completing-read))

(provide 'djr-magit)
