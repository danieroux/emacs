(require 'mu4e)
(require 'org-mu4e)

(setq mu4e-maildir (expand-file-name "~/Desktop/Maildir"))

(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/sent")
(setq mu4e-trash-folder  "/trash")

(setq mu4e-bookmarks
  '(("flag:unread AND NOT flag:trashed"           "Unread messages"          ?u)
   ("maildir:/danie@lautus.net AND flag:unread"  "@work unread"             ?w)
   ("mime:application/pdf"                       "Messages with documents"  ?d)))
  
;; don't save message to Sent Messages, GMail/IMAP will take care of this
(setq mu4e-sent-messages-behavior 'delete)

;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '( ("/INBOX"               . ?i)))

(setq mu4e-get-mail-command "offlineimap")

(require 'smtpmail)
;; Authentication is handled by ~/.authinfo with this format:
;; machine smtp.gmail.com login USER@gmail.com password PASSWORD port 465
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'ssl
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 465)

(provide 'my-mu4e)
