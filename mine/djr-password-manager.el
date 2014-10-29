(add-to-list 'load-path (concat external-dir "/emacs-id-manager"))

(autoload 'id-manager "id-manager" nil t)

(setq idm-database-file "~/Dropbox/Documents/passwords.gpg")

(provide 'djr-password-manager)
