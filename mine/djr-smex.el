(use-package smex
  :ensure t
  :bind* (("M-x" . smex)
	  ("M-X" . smex-major-mode-commands))
  :init (smex-initialize))

(provide 'djr-smex)
