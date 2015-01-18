(use-package elisp-slime-nav
  :ensure t
  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode)))

(require 'info-look)

(font-lock-add-keywords 'emacs-lisp-mode
			'(("(\\(lambda\\)\\>" (0 (prog1 ()
					      (compose-region (match-beginning 1)
							      (match-end 1)
							      ?λ))))))

(provide 'djr-elisp)
