;; Make Emacs feel like home

;; Before require evil
(setq evil-want-C-i-jump nil)

(use-package evil
  :ensure t
  :pin "melpa"
  :init (evil-mode t)
  :config (dolist (mode '(mu4e-main-mode
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
			  makey-key-mode
			  *idm-record-dialog*))
	    (push mode evil-emacs-state-modes)))

(use-package ace-jump-mode
  :ensure t
  :commands ace-jump-mode

  :init
  (progn
    (setq evil-ace-jump-active t)
    (define-key evil-normal-state-map (kbd "SPC") 'evil-ace-jump-word-mode)))

(setq evil-normal-state-cursor '("green" box))

(provide 'djr-vim)
