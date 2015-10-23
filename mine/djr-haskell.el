;; https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md

(use-package djr-company)

(use-package haskell-mode
  :ensure t
  :defer t
  :config
  (progn
    (autoload 'ghc-init "ghc" nil t)
    (autoload 'ghc-debug "ghc" nil t)

    (bind-key (kbd "C-c C-z") 'haskell-interactive-bring haskell-mode-map)

    (add-hook 'haskell-mode-hook (lambda ()
				   (ghc-init)
				   (flymake-mode)
				   (turn-on-haskell-doc-mode)

				   (haskell-indentation-mode 0)
				   
				   (define-key evil-normal-state-local-map (kbd "C-l") 'haskell-process-load-or-reload)
				   (define-key evil-insert-state-local-map (kbd "C-l") 'haskell-process-load-or-reload)
				   (define-key evil-normal-state-local-map (kbd "M-.") 'djr/haskell-find-tag-no-prompt)
                                   ;; (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def)
				   (define-key evil-insert-state-local-map (kbd "M-.") 'djr/haskell-find-tag-no-prompt)))

    ;; http://www.mew.org/~kazu/proj/ghc-mod/en/emacs.html
    (use-package ghc
      :ensure t
      :pin "melpa-stable"
      :defer t)

    (use-package company-ghc
      :ensure t
      :defer t
      :init
      (add-to-list 'company-backends 'company-ghc))))

(defun djr/haskell-find-tag-no-prompt (&optional re-find)
  (interactive "P")
  "Redefine haskell-mode-jump-to-def - do not prompt me giving me the default I want. Just jump to what ever is at point. On repeated invocations, find the next tag"
  (let ((ident (haskell-ident-at-point)))
    (if (or re-find
	    (string= ident last-tag))
	(find-tag ident t)
      (find-tag ident))))

(setq haskell-tags-on-save t
      haskell-interactive-mode-eval-pretty t)

(provide 'djr-haskell)
