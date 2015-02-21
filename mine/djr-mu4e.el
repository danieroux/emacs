;; -*- lexical-binding: t -*-

(setq mu4e-maildir (expand-file-name "~/Mail"))

(setq smtpmail-queue-mail t
      smtpmail-queue-dir (concat mu4e-maildir "/mu4e-queue"))

(setq mu4e-drafts-folder "/[Gmail]/.Drafts")
(setq mu4e-sent-folder   "/[Gmail]/.Sent Mail")

;; Not synced
(setq mu4e-trash-folder  "/not-really-trash")
(setq mu4e-refile-folder "/gtd")

(setq mail-host-address "weft"
      mu4e-get-mail-command "true"
      mu4e-view-wrap-lines t)

(when *osx*
  (progn
    (setq mu4e-mu-binary "/usr/local/bin/mu"
	  mail-host-address "danie-notebook")))

(setq mu4e-use-fancy-chars nil
      mu4e-headers-results-limit 100
      mu4e-attachment-dir "~/Desktop"
      mu4e-headers-skip-duplicates t
      mu4e-view-show-images t
      mu4e-view-image-max-width 800
      mu4e-view-show-addresses t
      mu4e-headers-fields '((:human-date . 12) (:from-or-to . 22) (:subject))
      message-kill-buffer-on-exit t)

;; mu4e~view-copy-contact c or C-c
(add-hook 'mu4e-view-mode-hook 'goto-address-mode)
(add-hook 'mu4e-compose-mode-hook 'turn-off-auto-fill)

(remove-hook 'text-mode-hook 'turn-on-auto-fill)

(require 'mu4e-contrib)
;;(setq mu4e-html2text-command "w3m -dump -T text/html")
(setq mu4e-html2text-command 'mu4e-shr2text)

(setq mu4e-view-show-images t)

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(defvar djr-mu4e-combined-inbox-bookmark "(maildir:/INBOX OR maildir:/[Gmail]/.Starred) AND NOT flag:trashed" "What I consider to be my 'inbox'")

;; More bookmarks
(setq mu4e-bookmarks `((,djr-mu4e-combined-inbox-bookmark                         "Act-on inbox"                  ?i)
		       ("flag:unread AND NOT maildir:/me AND NOT flag:trashed"    "Unread messages"               ?v)
		       ("maildir:/INBOX AND flag:unread AND NOT flag:trashed"     "Unread to me"                  ?m)
		       ("maildir:/INBOX AND flag:replied AND NOT flag:trashed"    "Replied to me"                 ?r)
		       ("mime:application/pdf AND NOT flag:thrashed"              "Messages with PDFs"            ?p)))

;; don't save message to Sent Messages, GMail/IMAP will take care of this
(setq mu4e-sent-messages-behavior 'delete)

;; setup some handy shortcuts
;; More shortcuts (with email addresses) in private.el
(setq mu4e-maildir-shortcuts
      `((,mu4e-drafts-folder    . ?d)))

(require 'smtpmail)
;; Authentication is handled by ~/.authinfo.gpg with this format:
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
(require 'djr-org-mu4e)

(defun djr/mu4e-inbox ()
  (interactive)

  (setq mu4e-headers-include-related nil
	mu4e~headers-sort-field :date
	mu4e~headers-sort-direction 'ascending)

  (mu4e-headers-search djr-mu4e-combined-inbox-bookmark))

(defun djr/mu4e-view-related-search (msg) 
  "Search for related messages to the current one" 
  (let* ((msgid (mu4e-msg-field msg :message-id)))
    (setq mu4e-headers-include-related t)
    (mu4e-headers-search (concat "msgid:" msgid))))

(defun djr/mu4e-open-message-in-google (msg)
  (let* ((msgid (mu4e-message-field msg :message-id))
	 (url (concat "https://mail.google.com/mail/u/0/?shva=1#search/rfc822msgid%3A"
		      (url-encode-url msgid))))
    (start-process "" nil "open" url)))

(add-to-list 'mu4e-view-actions '("gopen in gmail" . djr/mu4e-open-message-in-google) t)
(add-to-list 'mu4e-view-actions '("rview related" . djr/mu4e-view-related-search) t)
(add-to-list 'mu4e-view-actions '("bview in browser" . mu4e-action-view-in-browser) t)

(provide 'djr-mu4e)
