;; Keeps ~/.emacs.d clean

(setq autosave-directory "~/tmp/emacs-cache/autosaves/")
(make-directory autosave-directory t)

(setq make-backup-files t ;; do make backups
      backup-by-copying t     ;; and copy them here
      backup-directory-alist '(("." . "~/tmp/emacs-cache/backups")) 
      version-control t
      kept-new-versions 2
      kept-old-versions 5
      delete-old-versions t
      auto-save-list-file-prefix (concat autosave-directory ".saves-")
      auto-save-file-name-transforms `((".*", autosave-directory t)))

(provide 'my-clean-emacs-directory) 

