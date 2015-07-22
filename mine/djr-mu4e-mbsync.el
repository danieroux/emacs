(use-package s
  :ensure t)

(defvar djr/maildirs-for-quick-add '("INBOX/cur" "INBOX/new"
                                     "[Gmail]/.Starred/new"
                                     "[Gmail]/.Starred/cur"
                                     "[Gmail]/.All Mail/new"))

(defun djr/add-new-messages-from-maildirs (output-buffer-or-name)
  (interactive)
  (with-current-buffer output-buffer-or-name
    (insert "Start adding messages...\n")
    (dolist (scan-maildir djr/maildirs-for-quick-add)
      (unless (or (s-ends-with? "/new" scan-maildir)
                  (s-ends-with? "/cur" scan-maildir))
        (error (format "The full /cur or /new path has to specified for scanning: (%s)" scan-maildir)))
      (let* ((new-dir (concat mu4e-maildir "/" scan-maildir))
             (maildir (concat "/"
                              (s-chop-suffixes '("/new" "/cur") scan-maildir))))
        (insert (format "Doing maildir %s\n" new-dir))
        (dolist (file (directory-files new-dir t "\\([^.]$\\)"))
          (progn
            (insert (format "Adding to mu database into maildir (%s): %s\n" maildir file))
            (mu4e~proc-add file maildir)))))
    (insert (format "Done adding messages at %s\n\n" (format-time-string "%Y-%m-%d %T")))))

(defvar djr/mbsync-buffer-name "*mbsync-mail-and-update-mu4e*")
(defvar djr/mbsync-process nil
  "The mbsync process")
(defvar djr/mbsync-run-forever t)
(defvar djr/mbsync-run-forever-timer nil)

(defvar djr/mbsync-command "mbsync")
(defvar djr/mbsync-args '("-a"))

;; (setq djr/mbsync-args '("danieroux:[Gmail]/Starred"))
(setq djr/mbsync-args '("-a"))

(defun djr/add-new-messages-and-possibly-rerun-sentinel (proc msg)
  (setq djr/mbsync-process nil)

  (djr/mbsync-run-forever-set-timer)

  (let ((status (process-status proc))
        (code (process-exit-status proc))
        (buffer (get-buffer djr/mbsync-buffer-name)))
    (with-current-buffer buffer
      (insert (format "%s mbsync process finished (%s %d)\n\n"
                      (format-time-string "%Y-%m-%d %T") status code))
      (cond
       ((eq status 'exit)
        (cond
         ((eq code 0)
          (message "mbsync process finished successfully")) ;; don't do anything
         (t (error "mbsync process ended with exit code %d" code))))
       (t (error "Something bad happened to the mbsync process")))

      (djr/add-new-messages-from-maildirs djr/mbsync-buffer-name))))

(defun djr/mbsync-run-forever-set-timer ()
  (when djr/mbsync-run-forever-timer
    (cancel-timer djr/mbsync-run-forever-timer))

  (when djr/mbsync-run-forever
    (setq djr/mbsync-run-forever-timer (run-with-timer (* 10 60) nil 'djr/sync-mail-and-update-mu4e))))

(defun djr/sync-mail-and-update-mu4e ()
  (interactive)

  (when (and (boundp 'mu4e~update-buffer)
             (buffer-live-p mu4e~update-buffer)
             (process-live-p (get-buffer-process mu4e~update-buffer)))
    ;; Try again in the future
    (djr/mbsync-run-forever-set-timer)
    (mu4e-error "Update process is already running"))

  (when djr/mbsync-process
    (error "An mbsync process is already running"))

  (when djr/mbsync-run-forever-timer
    (cancel-timer djr/mbsync-run-forever-timer))

  (let* ((buffer (get-buffer-create djr/mbsync-buffer-name)))
    (with-current-buffer buffer
      (setq mbsync (apply-partially 'start-process "mbsync" buffer djr/mbsync-command))
      (setq djr/mbsync-process (apply mbsync djr/mbsync-args))
      (shell-mode)
      (set-process-filter djr/mbsync-process 'comint-output-filter)
      (set-process-sentinel djr/mbsync-process 'djr/add-new-messages-and-possibly-rerun-sentinel))))

(defun djr/sync-mail-and-update-mu4e-quickly ()
  (interactive)
  (let* ((djr/mbsync-args '("danieroux:[Gmail]/Starred" "danieroux:INBOX")))
    (djr/sync-mail-and-update-mu4e))
  (djr/mbsync-run-forever-set-timer))

(provide 'djr-mu4e-mbsync)
