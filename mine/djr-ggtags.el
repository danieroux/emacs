(use-package ggtags
  :pin "melpa"
  :ensure t
  :defer t
  :init
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode)
		(ggtags-mode 1)
		(define-key evil-normal-state-local-map (kbd "M-.") 'ggtags-find-tag-dwim)))))

(provide 'djr-ggtags)
