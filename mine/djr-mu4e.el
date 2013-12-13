;; -*- lexical-binding: t -*-

(setq mu4e-maildir (expand-file-name "~/Dropbox/Maildir"))

(setq smtpmail-queue-mail  nil  ;; start in non-queuing mode
      smtpmail-queue-dir (concat mu4e-maildir "/mu4e-queue"))

(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
;; Not synced with offlineimap
(setq mu4e-trash-folder  "/trash")
(setq mu4e-refile-folder "/gtd.support")

(setq mail-host-address "loom"
      ;; offlineimap (on loom) handles the get
      mu4e-get-mail-command "true"
      mu4e-view-wrap-lines t)

(if (equal system-type 'darwin)
    (progn
      (setq mu4e-mu-binary "/usr/pkg/bin/mu"
	    mail-host-address "danie-notebook")
      (add-to-list 'load-path "/usr/pkg/share/emacs/site-lisp/mu4e")))

(setq mu4e-use-fancy-chars t
      mu4e-attachment-dir "~/Desktop"
      mu4e-headers-skip-duplicates t
      mu4e-view-show-images t
      mu4e-view-image-max-width 800
      mu4e-view-show-addresses t
      message-kill-buffer-on-exit t)

(add-hook 'mu4e-view-mode-hook 'goto-address-mode)
(add-hook 'mu4e-compose-mode-hook 'turn-off-auto-fill)

(remove-hook 'text-mode-hook 'turn-on-auto-fill)
(setq mu4e-html2text-command "w3m -dump -T text/html")

(setq mu4e-view-show-images t)

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; More bookmarks (with email 
(setq mu4e-combined-inbox-bookmark "(maildir:/INBOX OR maildir:/[Gmail].Starred) AND NOT flag:trashed"
      mu4e-bookmarks `((,mu4e-combined-inbox-bookmark                             "Act-on inbox"                  ?i)
		       ("flag:unread AND NOT maildir:/me AND NOT flag:trashed"    "Unread messages"               ?v)
		       ("maildir:/INBOX AND flag:unread AND NOT flag:trashed"     "Unread to me"                  ?m)
		       ("maildir:/INBOX AND flag:replied AND NOT flag:trashed"    "Replied to me"                 ?r)
		       ("maildir:/accounts AND flag:unread AND NOT flag:trashed"  "Unread to accounts"            ?a)
		       ("maildir:/others AND flag:unread AND NOT flag:trashed"    "Unread not to me"              ?n)
		       ("mime:application/pdf AND NOT flag:thrashed"              "Messages with documents"       ?d)))

;; don't save message to Sent Messages, GMail/IMAP will take care of this
(setq mu4e-sent-messages-behavior 'delete)

;; setup some handy shortcuts
;; More shortcuts (with email addresses) in private.el
(setq mu4e-maildir-shortcuts
      `((,mu4e-drafts-folder    . ?d)))

(require 'smtpmail)
;; Authentication is handled by ~/.authinfo with this format:
;; machine smtp.gmail.com login USER@gmail.com password PASSWORD port 465
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'ssl
      smtpmail-smtp-server "smtp.gmail.com"
      message-user-fqdn "danieroux.com"
      mail-user-agent 'mu4e-user-agent
      smtpmail-smtp-service 465)

;; Load mu4e, it has become core to me
(require 'mu4e)
(require 'org-mu4e)

(defun org-mu4e-store-link ()
  "Redefine to never store queries, just messages"
  (cond
   ((or (eq major-mode 'mu4e-view-mode)
	(eq major-mode 'mu4e-headers-mode))
    (let* ((msg  (mu4e-message-at-point))
	   (msgid   (or (plist-get msg :message-id) "<none>"))
	   link)
      (org-store-link-props :type "mu4e" :link link
			    :message-id msgid)
      (setq link (concat "mu4e:msgid:" msgid))
      (org-add-link-props :link link
			  :description (funcall org-mu4e-link-desc-func msg))
      link))))

(add-hook 'message-sent-hook 'djr/org-mu4e-store-link-on-sent-message)

(defun djr/mu4e-inbox ()
  (interactive)
  (setq mu4e-headers-include-related nil)
  (mu4e-headers-search mu4e-combined-inbox-bookmark))

;; message-cancel-hook does not do what I expected
(defadvice message-kill-buffer (after djr/cancel-message)
  (djr/org-mu4e-capture-cancel))
(ad-activate 'message-kill-buffer)

(defadvice message-send (after djr/capture-sent-message)
  (if djr/org-mu4e-must-capture-message
      (progn 
	(org-store-link nil)
	(org-capture nil "e")))
  (djr/org-mu4e-capture-cancel))
(ad-activate 'message-send)

(defun djr/org-mu4e-capture-cancel ()
  (interactive)
  (setq djr/org-mu4e-must-capture-message nil
	global-mode-string (delq 'djr-org-capture-mode-line-string global-mode-string)))
(djr/org-mu4e-capture-cancel)

(defun djr/org-mu4e-capture-next-message ()
  (setq djr/org-mu4e-must-capture-message t
	djr-org-capture-mode-line-string "Org capturing this mail")
  (or global-mode-string (setq global-mode-string '("")))
  (or (memq 'djr-org-capture-mode-line-string global-mode-string)
      (setq global-mode-string
	    (append global-mode-string '(djr-org-capture-mode-line-string)))))

(defun djr/org-mu4e-store-link-on-sent-message ()
  (if djr/org-mu4e-must-capture-message
      (let* ((msgid (message-fetch-field "Message-ID"))
	     (description (message-fetch-field "Subject")))
	(djr~org-mu4e-store-link-on-sent-message msgid description))))

(defun djr~wipe-brackets (msgid)
  (interactive)
  (remove-if (lambda (c)
	       (or (equal c ?>)
		   (equal c ?<)))
	     msgid))

(defun djr~org-mu4e-store-link-on-sent-message (msgid description)
  (let* ((link (concat "mu4e:msgid:" (djr~wipe-brackets msgid))))
    (setq djr/org-mu4e-captured-message-p
	  `(:type mu4e
		  :description ,description
		  :link ,link
		  :message-id ,msgid))))

(setq djr/org-mu4e-must-capture-message nil)

(defun djr~org-mu4e-make-link-from-captured-message ()
  (if djr/org-mu4e-must-capture-message
      (setq org-store-link-plist djr/org-mu4e-captured-message-p)))
(add-hook 'org-store-link-functions 'djr~org-mu4e-make-link-from-captured-message)

(defun djr/mu4e-compose-new-with-follow-up ()
  (interactive)
  (djr/org-mu4e-capture-next-message)
  (mu4e-compose-new))

(defun djr/mu4e-compose-reply-with-follow-up ()
  (interactive)
  (djr/org-mu4e-capture-next-message)
  (mu4e-compose-reply))

(defun djr/mu4e-forward-with-follow-up ()
  (interactive)
  (djr/org-mu4e-capture-next-message)
  (mu4e-compose-forward))

(defun djr/mu4e-view-related-search (msg) 
  "Search for related messages to the current one" 
  (let* ((msgid (mu4e-msg-field msg :message-id)))
    (mu4e-headers-search (concat "\"msgid:" msgid "\""))))

(setq djr~mu4e-inbox-buffer-name "*djr-mu4e-inbox*")

(defun djr~mu4e-message-to-orgline (msg &optional point)
  (switch-to-buffer djr~mu4e-inbox-buffer-name)
  (let ((msgid (mu4e-message-field msg :message-id))
	(subject (mu4e-message-field msg :subject)))
    (progn
      (insert (format "* [[mu4e:msgid:%s][%s]]" msgid subject))
      (newline))))

(setq djr~mu4e-to-org-continue-fun nil)

(defun djr~mu4e-write-file (count)
  (switch-to-buffer djr~mu4e-inbox-buffer-name)
  (write-region nil
		nil
		(concat org-directory "/" "mail-inbox.org"))
  (kill-buffer)
  (if (get-buffer "mail-inbox.org")
      (save-excursion
	(switch-to-buffer "mail-inbox.org")
	(revert-buffer t t)))
  (setq mu4e-header-func 'mu4e~headers-header-handler
	mu4e-view-func 'mu4e~headers-view-handler
	mu4e-update-func 'mu4e~headers-update-handler
	mu4e-remove-func 'mu4e~headers-remove-handler
	mu4e-found-func 'mu4e~headers-found-handler)
  (if djr~mu4e-to-org-continue-fun
      (funcall djr~mu4e-to-org-continue-fun))) 

(defun djr/mu4e-to-org (&optional continue-fun)
  (interactive)
  (mu4e)
  (setq mu4e-headers-include-related nil)
  (setq djr~mu4e-to-org-continue-fun continue-fun)
  (setq mu4e-header-func 'djr~mu4e-message-to-orgline
	mu4e-found-func 'djr~mu4e-write-file
	mu4e-view-func 'identity
	mu4e-update-func 'identity
	mu4e-remove-func 'identity)
  (get-buffer-create djr~mu4e-inbox-buffer-name)
  (switch-to-buffer djr~mu4e-inbox-buffer-name)
  (insert "#+FILETAGS: refile mail")
  (newline)
  (mu4e~headers-search-execute "maildir:/INBOX" 't))

(defun djr/delete-replied-mail-in-inbox (msg)
  "Delete the replied email - they are also in the 'me' folder"
  (mu4e~headers-search-execute "maildir:/INBOX flag:replied" 't))

;(add-to-list 'mu4e-headers-actions '("Delete replied" . djr/delete-replied-mail-in-inbox) t)

(defun djr/mu4e-open-message-in-google (msg)
  (let* ((msgid (mu4e-message-field msg :message-id))
	 (url (concat "https://mail.google.com/mail/u/0/?shva=1#search/rfc822msgid%3A"
		      (url-encode-url msgid))))
    (start-process "" nil "open" url)))

(add-to-list 'mu4e-view-actions '("gopen in gmail" . djr/mu4e-open-message-in-google) t)
(add-to-list 'mu4e-view-actions '("rview related" . djr/mu4e-view-related-search) t)
(add-to-list 'mu4e-view-actions '("bview in browser" . mu4e-action-view-in-browser) t)

(provide 'djr-mu4e)
