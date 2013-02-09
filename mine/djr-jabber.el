(add-to-list 'load-path (concat external-dir "/emacs-jabber-hodge-podge"))
;; The -git needs autotools to get running. Instead, do it once and copy *.el into hodge-podge
; (add-to-list 'load-path (concat external-dir "/emacs-jabber-git"))

(require 'jabber-autoloads)

(setq jabber-account-list `(("danie@danieroux.com/emacs"
			    (:network-server . "talk.google.com")
			    (:connection-type . ssl)
			    (:password . ,djr/jabber-password))))

(setq jabber-alert-presence-hooks nil
      jabber-chatstates-confirm nil
      jabber-mode-line-mode t
      jabber-auto-reconnect t
      jabber-activity-make-strings 'jabber-activity-make-strings-shorten)

(add-to-list 'evil-emacs-state-modes 'jabber-chat-mode)
(add-to-list 'evil-emacs-state-modes 'jabber-roster-mode)

(provide 'djr-jabber)
