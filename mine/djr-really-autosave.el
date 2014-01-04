(defvar my-org-really-auto-save t)

(defun djr/save-and-backup ()
  (save-buffer)
  (backup-buffer-copy buffer-file-name
		      (concat buffer-file-name "-" (format-time-string "%Y-%m-%d"))
		      (file-modes buffer-file-name)
		      nil))

(defun really-auto-save-some-modes (&optional args)
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (save-excursion
	(when (and
	       buffer-file-name
	       (buffer-modified-p)
	       (eq major-mode 'org-mode)
	       (eq my-org-really-auto-save t))
	  (message (concat "Really auto-saving (and backing up)" buffer-file-name))
	  (djr/save-and-backup))))))

(setq auto-save-timeout 20
      auto-save-interval 300)

(add-hook 'auto-save-hook 'really-auto-save-some-modes)

(provide 'djr-really-autosave)
