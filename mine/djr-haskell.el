(djr/ensure-package 'haskell-mode)
;; http://www.mew.org/~kazu/proj/ghc-mod/en/emacs.html
(djr/ensure-melpa-package 'ghc)

(autoload 'ghc-init "ghc" nil t)

(add-hook 'haskell-mode-hook (lambda ()
			       (ghc-init)
			       (flymake-mode)
			       (turn-on-haskell-doc-mode)
			       (turn-on-haskell-indentation)
			       (define-key evil-normal-state-local-map (kbd "M-.") 'find-tag)
			       (define-key evil-normal-state-local-map (kbd "M-,") 'pop-tag-mark)))

(setq haskell-tags-on-save t)

(provide 'djr-haskell)
