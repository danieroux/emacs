(add-to-list 'load-path "/usr/pkg/share/emacs/site-lisp/mu4e")

(require 'mu4e)
(require 'org-mu4e)

(setq mu4e-maildir (expand-file-name "~/Maildir"))
(setq mu4e-mu-binary "/usr/pkg/bin/mu")
(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
;; Not synced with offlineimap
(setq mu4e-trash-folder  "/trash")

(setq mail-host-address "danie-notebook"
      ;; offlineimap.el handles the get
      mu4e-get-mail-command "mu index"
      mu4e-view-wrap-lines t
      mu4e-html2text-command "/usr/pkg/bin/html2text")

(setq smtpmail-queue-mail  nil  ;; start in non-queuing mode
      smtpmail-queue-dir   "~/Maildir/queue/cur")

(add-hook 'mu4e-view-mode-hook 'goto-address-mode)

;; More bookmarks (with email addresses) in private.el
(setq mu4e-bookmarks
  '(("flag:unread AND NOT maildir:/me AND NOT flag:trashed"          "Unread messages"               ?v)
   ("maildir:/INBOX AND flag:unread AND NOT flag:trashed"            "Unread to me"                  ?m)
   ("maildir:/others AND flag:unread AND NOT flag:trashed"           "Unread not to me"              ?n)

   ("mime:application/pdf AND NOT flag:thrashed"                     "Messages with documents"       ?d)))

;; don't save message to Sent Messages, GMail/IMAP will take care of this
(setq mu4e-sent-messages-behavior 'delete)

;; setup some handy shortcuts
;; More shortcuts (with email addresses) in private.el
(setq mu4e-maildir-shortcuts
      '( ("/INBOX"               . ?i)
	 ("/me"                  . ?m)
	 ("/others"              . ?o)
	 ("/accounts"            . ?a)))

(require 'smtpmail)
;; Authentication is handled by ~/.authinfo with this format:
;; machine smtp.gmail.com login USER@gmail.com password PASSWORD port 465
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'ssl
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 465)

(provide 'my-mu4e)