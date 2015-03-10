;; https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md

(use-package djr-company)

(use-package haskell-mode
  :ensure t
  :defer t
  :config
  (progn
    ;; https://github.com/chrisdone/structured-haskell-mode
    (use-package shm
      :ensure t
      :pin melpa
      :defer t)
    
    ;; http://www.mew.org/~kazu/proj/ghc-mod/en/emacs.html
    (use-package ghc
      :ensure t
      :pin melpa
      :defer t)

    (autoload 'ghc-init "ghc" nil t)

    (use-package company-ghc
      :ensure t
      :pin melpa
      :defer t
      :init
      (add-to-list 'company-backends 'company-ghc))))

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

			       (haskell-indentation-mode 0)
			       (structured-haskell-mode)

			       (define-key evil-normal-state-local-map (kbd "C-l") 'haskell-process-load-or-reload)
			       (define-key evil-insert-state-local-map (kbd "C-l") 'haskell-process-load-or-reload)
			       (define-key evil-normal-state-local-map (kbd "M-.") 'djr/haskell-find-tag-no-prompt)
			       (define-key evil-insert-state-local-map (kbd "M-.") 'djr/haskell-find-tag-no-prompt)))

(setq haskell-tags-on-save t
      haskell-interactive-mode-eval-pretty t)

(provide 'djr-haskell)
