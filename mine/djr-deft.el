; http://jblevins.org/projects/deft

(use-package deft
  :ensure t
  :pin "melpa"
  :commands deft
  :config 
  (setq deft-extension "md"
	deft-use-filename-as-title t
	deft-text-mode 'markdown-mode
	deft-directory "/Users/danie/Dropbox/Spine/drafts"))

(provide 'djr-deft)
