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

(setq
 mu4e-use-fancy-chars t
 mu4e-attachment-dir "~/Desktop"
 mu4e-view-show-images t
 mu4e-view-image-max-width 800
 mu4e-view-show-addresses t)

(add-hook 'mu4e-view-mode-hook 'goto-address-mode)

(setq mu4e-html2text-command "w3m -dump -T text/html")

(setq mu4e-view-show-images t)

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(add-to-list 'evil-emacs-state-modes 'mu4e-main-mode)
(add-to-list 'evil-emacs-state-modes 'mu4e-headers-mode)
(add-to-list 'evil-emacs-state-modes 'mu4e-view-mode)
(add-hook 'mu4e-view-mode-hook 'evil-emacs-state)

;; More bookmarks (with email 
(setq mu4e-bookmarks
  '(("flag:unread AND NOT maildir:/me AND NOT flag:trashed"          "Unread messages"               ?v)
   ("maildir:/INBOX AND flag:unread AND NOT flag:trashed"            "Unread to me"                  ?m)
   ("maildir:/accounts AND flag:unread AND NOT flag:trashed"         "Unread to accounts"            ?a)
   ("maildir:/others AND flag:unread AND NOT flag:trashed"           "Unread not to me"              ?n)
   ("mime:application/pdf AND NOT flag:thrashed"                     "Messages with documents"       ?d)))

;; don't save message to Sent Messages, GMail/IMAP will take care of this
(setq mu4e-sent-messages-behavior 'delete)

;; setup some handy shortcuts
;; More shortcuts (with email addresses) in private.el
(setq mu4e-maildir-shortcuts
      '( ("/INBOX"               . ?i)
         ("/gtd"                 . ?g)
	 ("/accounts"            . ?a)))

(require 'smtpmail)
;; Authentication is handled by ~/.authinfo with this format:
;; machine smtp.gmail.com login USER@gmail.com password PASSWORD port 465
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'ssl
      smtpmail-smtp-server "smtp.gmail.com"
      message-user-fqdn "danieroux.com"
      mail-user-agent 'mu4e-user-agent
      smtpmail-smtp-service 465)

(autoload 'mu4e "mu4e")

(eval-after-load "mu4e"
  '(progn
    (require 'org-mu4e)

    (defun my-mu4e-file-email-in-gtd ()
      (interactive)
      ;; (mu4e-view-mark-for-delete)
      (make-capture-frame))

    (defun my-mu4e-gtd-inbox ()
      (interactive)
      (mu4e~proc-index mu4e-maildir)
      (sleep-for 5)
      (mu4e-headers-search "maildir:/gtd"))))
   
(provide 'djr-mu4e)
