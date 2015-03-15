(use-package dired
  :commands dired

  :config
  (progn
    (bind-key "a" 'gnus-dired-attach dired-mode-map)
    (bind-key "e" 'open-eshell-here dired-mode-map)
    (bind-key "E" 'open-in-external-app dired-mode-map)

    (add-hook 'dired-load-hook
	      (function (lambda () (load "dired-x"))))

    ;; http://www.djcbsoftware.nl/code/mu/mu4e/Attaching-files-with-dired.html#Attaching-files-with-dired
    ;; mark the file(s) in dired you would like to attach and press C-c RET C-a
    (require 'gnus-dired)
    ;; make the `gnus-dired-mail-buffers' function also work on
    ;; message-mode derived modes, such as mu4e-compose-mode
    (defun gnus-dired-mail-buffers ()
      "Return a list of active message buffers."
      (let (buffers)
	(save-current-buffer
	  (dolist (buffer (buffer-list t))
	    (set-buffer buffer)
	    (when (and (derived-mode-p 'message-mode)
		       (null message-sent-message-via))
	      (push (buffer-name buffer) buffers))))
	(nreverse buffers)))

    (setq gnus-dired-mail-mode 'mu4e-user-agent)
    (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)))

(defun open-eshell-here ()
  (interactive)
  (eshell '(4)))

;; http://xahlee.org/emacs/emacs_dired_open_file_in_ext_apps.html
(defun open-in-external-app ()
  "Open the current file or dired marked files in external app.
   Works in Microsoft Windows, Mac OS X, Linux."
  (interactive)
  (let (doIt
	(myFileList
	 (cond
	  ((string-equal major-mode "dired-mode") (dired-get-marked-files))
	  (t (list (buffer-file-name))))))

    (setq doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files?")))
    
    (when doIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)) ) myFileList))
       ((string-equal system-type "darwin")
        (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "open" fPath)) )  myFileList))
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath)) ) myFileList))))))

(provide 'djr-dired)
