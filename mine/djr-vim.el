;; Make Emacs feel like home

(djr/ensure-package 'evil)

;; Before require evil
(setq evil-want-C-i-jump nil)

(require 'evil)

(setq evil-normal-state-cursor '("green" box))

(dolist (mode '(mu4e-main-mode
		mu4e-headers-mode
		mu4e-view-mode
		info-mode
		elfeed-show-mode
		elfeed-search-mode
		twittering-mode
		inferior-haskell-mode
		cider-repl-mode
		calculator-mode
		deft-mode
		ert-results-mode
		*idm-record-dialog*))
  (push mode evil-emacs-state-modes))

(add-hook 'mu4e-view-mode-hook 'evil-emacs-state)

(evil-mode t)

(provide 'djr-vim)
