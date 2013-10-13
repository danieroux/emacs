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

(add-hook 'dired-load-hook
	  (function (lambda () (load "dired-x"))))

(provide 'djr-dired)
