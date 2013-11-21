(djr/ensure-package 'elisp-slime-nav)

(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

(font-lock-add-keywords 'emacs-lisp-mode
			'(("(\\(lambda\\)\\>" (0 (prog1 ()
					      (compose-region (match-beginning 1)
							      (match-end 1)
							      ?λ))))))

(provide 'djr-elisp)
