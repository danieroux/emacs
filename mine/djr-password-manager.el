(use-package id-manager
  :commands id-manager
  :ensure t
  :init
  (progn
    (setq idm-database-file "~/Dropbox/Documents/passwords.gpg")
    ;; Triggered by QuickSilver
    (bind-key* "S-<f3>" 'id-manager)
    (bind-key* "C-c i" 'id-manager)))

(provide 'djr-password-manager)
