(use-package smex
  :ensure t
  :bind* (("M-x" . smex)
	  ("M-X" . smex-major-mode-commands))
  :config (smex-initialize))

(provide 'djr-smex)
