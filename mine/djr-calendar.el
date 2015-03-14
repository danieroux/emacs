(use-package calfw-cal
  :defer t
  :ensure calfw
  :pin melpa
  :commands djr/open-calendars

  :init
  (progn 
    (setq cfw:fchar-junction ?╋
	  cfw:fchar-vertical-line ?┃
	  cfw:fchar-horizontal-line ?━
	  cfw:fchar-left-junction ?┣
	  cfw:fchar-right-junction ?┫
	  cfw:fchar-top-junction ?┯
	  cfw:fchar-top-left-corner ?┏
	  cfw:fchar-top-right-corner ?┓
	  cfw:render-line-breaker 'cfw:render-line-breaker-none)
    (setq mark-diary-entries-in-calendar t
	  diary-number-of-entries 7
	  my-diary "~/Dropbox/Documents/gtd/diary.el")

    (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
    (add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files))

  :config
  (progn
    (use-package calfw-cal)
    (use-package calfw-org)))

(defun djr/open-calendars ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")
    (cfw:cal-create-source "Orange"))))

(defun import-cal (ics-file)
  (interactive)
  (icalendar-import-file ics-file my-diary t)
  (kill-buffer (car (last (split-string ics-file "/")))))

(defun getcal (url)
  "Download ics file and add to diary"
  (let ((tmpfile (url-file-local-copy url)))
   (import-cal tmpfile)))

;; google-calendars defined in private.el
(defun djr/refresh-calendars ()
  (interactive)
  (find-file my-diary)
  (flycheck-stop)
  (erase-buffer)
  (dolist (url google-calendars) (getcal url))
  (kill-buffer "diary.el"))

(provide 'djr-calendar)
