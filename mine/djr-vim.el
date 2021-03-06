;; Make Emacs feel like home

;; Before require evil
(setq evil-want-C-i-jump nil)

(use-package evil-leader
  :ensure t
  :init (global-evil-leader-mode)
  :config
  
  (progn
    (setq evil-leader/leader ","
          evil-leader/no-prefix-mode-rx '(".*")
          evil-leader/in-all-states t)

    (evil-leader/set-key
      "c" 'org-capture
      "d" 'deft
      "i" 'id-manager
      "f" 'darkroom-mode
      "t" 'todotxt
      ;; There should be A Better Way
      "SPC" (lambda () (interactive) (insert ", "))
      "RET" (lambda () (interactive) (insert ",") (newline)))))

(use-package evil
  :ensure t
  :pin "melpa"
  :init (evil-mode t)
  :config
  (progn
    (dolist (mode '(mu4e-main-mode
		    mu4e-headers-mode
		    mu4e-view-mode
		    info-mode
		    elfeed-show-mode
		    elfeed-search-mode
		    twittering-mode
		    inferior-haskell-mode
                    inf-clojure-mode
		    calculator-mode
		    deft-mode
		    ert-results-mode
		    makey-key-mode
                    haskell-error-mode
                    rcirc-groups-mode
                    dig-mode
                    todotxt-mode
                    cider-stacktrace-mode
                    special-mode        ; Lispy. Stupid mode name.
                    cider-test-report-mode
		    *idm-record-dialog*))
      (push mode evil-emacs-state-modes))
    (delete 'rcirc-mode evil-emacs-state-modes)))

(use-package ace-jump-mode
  :ensure t
  :commands ace-jump-mode

  :init
  (progn
    (setq evil-ace-jump-active t)
    (define-key evil-normal-state-map (kbd "SPC") 'evil-ace-jump-word-mode)))

(setq evil-normal-state-cursor '("green" box))

(provide 'djr-vim)
