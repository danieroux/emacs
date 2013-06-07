;; Google calendars

(setq mark-diary-entries-in-calendar t
      diary-number-of-entries 7
      my-diary "~/Dropbox/Documents/gtd/diary.el")

(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)

(defun getcal (url)
  "Download ics file and add to diary"
  (let ((tmpfile (url-file-local-copy url)))
    (icalendar-import-file tmpfile my-diary t)
    (kill-buffer (car (last (split-string tmpfile "/"))))))

;; google-calendars defined in private.el
(defun getcals ()
  (interactive)
  (find-file my-diary)
  (erase-buffer)
  (dolist (url google-calendars) (getcal url))
  (kill-buffer "diary.el"))

(provide 'djr-calendar)
