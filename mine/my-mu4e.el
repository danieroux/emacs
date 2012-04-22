(add-to-list 'load-path "/usr/pkg/share/emacs/site-lisp/mu4e")

(require 'mu4e)
(require 'org-mu4e)

(setq mu4e-maildir (expand-file-name "~/Maildir"))
(setq mu4e-mu-binary "/usr/pkg/bin/mu")
(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

(setq mail-host-address "danie-notebook"
      mu4e-get-mail-command "/Users/danie/bin/oi-quick")

(add-hook 'mu4e-view-mode-hook 'goto-address-mode)

(setq mu4e-bookmarks
  '(("flag:unread AND NOT flag:trashed"           "Unread messages"          ?u)
   ("maildir:/danie@lautus.net AND flag:unread"  "@work unread"             ?w)
   ("mime:application/pdf"                       "Messages with documents"  ?d)))
  
;; don't save message to Sent Messages, GMail/IMAP will take care of this
(setq mu4e-sent-messages-behavior 'delete)

;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '( ("/INBOX"               . ?i)))

(require 'smtpmail)
;; Authentication is handled by ~/.authinfo with this format:
;; machine smtp.gmail.com login USER@gmail.com password PASSWORD port 465
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'ssl
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 465)

(provide 'my-mu4e)
