(djr/ensure-package 'haskell-mode)

;; http://www.mew.org/~kazu/proj/ghc-mod/en/emacs.html
(djr/ensure-melpa-package 'ghc)

(autoload 'ghc-init "ghc" nil t)

(defun djr/haskell-find-tag-no-prompt (&optional re-find)
  (interactive "P")
  "Do not prompt me giving me the default I want. Just jump to what ever is at point. On repeated invocations, find the next tag"
  (let ((ident (haskell-ident-at-point)))
    (if (or re-find
	    (string= ident last-tag))
	(find-tag ident t)
      (find-tag ident))))

(bind-key (kbd "C-c C-z") 'haskell-interactive-bring haskell-mode-map)

(add-hook 'haskell-mode-hook (lambda ()
			       (ghc-init)
			       (flymake-mode)
			       (turn-on-haskell-doc-mode)
			       (turn-on-haskell-indentation)
			       (define-key evil-normal-state-local-map (kbd "C-l") 'haskell-process-load-or-reload)
			       (define-key evil-insert-state-local-map (kbd "C-l") 'haskell-process-load-or-reload)
			       (define-key evil-normal-state-local-map (kbd "M-.") 'djr/haskell-find-tag-no-prompt)
			       (define-key evil-insert-state-local-map (kbd "M-.") 'djr/haskell-find-tag-no-prompt)
			       (define-key evil-normal-state-local-map (kbd "M-,") 'pop-tag-mark)))

(setq haskell-tags-on-save t
      haskell-interactive-mode-eval-pretty t)

(provide 'djr-haskell)
