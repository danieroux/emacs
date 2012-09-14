(defvar my-org-really-auto-save t)

(defun really-auto-save-some-modes (&optional args)
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and
             (buffer-file-name)
             (buffer-modified-p)
             (eq major-mode 'org-mode)
	     (eq my-org-really-auto-save t))
        (message (concat "Really auto-saving " buffer-file-name))
        (save-buffer args)))))

(setq auto-save-timeout 20
      auto-save-interval 20)
(add-hook 'auto-save-hook 'really-auto-save-some-modes)

(provide 'my-really-autosave)
