(use-package history
  :ensure t
  :pin "melpa"
  :bind* (("M-," . history-prev-history)
	  ("M->" . history-next-history))
  :init
  (history-mode 1)
  :config
  (progn
    (dolist (fun '(find-tag-noselect
		   ggtags-find-tag-dwim
		   org-open-at-point
		   djr/eval-sexp-on-line
		   ido-find-file
		   widget-button-press
		   elisp-slime-nav-find-elisp-thing-at-point))
      (add-to-list 'history-advised-before-functions fun))
    (history-init-advices t)))

(provide 'djr-history)
