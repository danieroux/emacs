(if (equal system-type 'darwin)
    (progn
      (setq mu4e-mu-binary "/usr/pkg/bin/mu")
      (add-to-list 'load-path "/usr/pkg/share/emacs/site-lisp/mu4e")))

(setq mu4e-maildir (expand-file-name "~/Maildir"))
(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
;; Not synced with offlineimap
(setq mu4e-trash-folder  "/trash")

(setq mail-host-address "danie-notebook"
      ;; offlineimap.el handles the get
      mu4e-get-mail-command "mu index"
      ;mu4e-split-view nil
      ;; Do not wrap by default
      mu4e-view-wrap-lines nil)

(setq smtpmail-queue-mail  nil  ;; start in non-queuing mode
      smtpmail-queue-dir   "~/Maildir/queue/cur")

(add-hook 'mu4e-view-mode-hook 'goto-address-mode)

(add-to-list 'evil-emacs-state-modes 'mu4e-main-mode)
(add-to-list 'evil-emacs-state-modes 'mu4e-headers-mode)
;(add-to-list 'evil-emacs-state-modes 'mu4e-view-mode)
(add-hook 'mu4e-view-mode-hook 'evil-emacs-state)

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
         ("/gtd"                 . ?g)
	 ("/accounts"            . ?a)))

(require 'smtpmail)
;; Authentication is handled by ~/.authinfo with this format:
;; machine smtp.gmail.com login USER@gmail.com password PASSWORD port 465
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'ssl
      smtpmail-smtp-server "smtp.gmail.com"
      message-user-fqdn "danieroux.com"
      smtpmail-smtp-service 465)

;; From mu4e mailing list ce50fd59-4694-40ae-98f0-fcd2b9ed2d32@dg7g2000vbb.googlegroups.com - Re: missing link in the view buffer
(defun mu4e-action-view-in-browser (msg)
  "Hack to view the html part for MSG in a web browser."
  (let* ((shellpath (shell-quote-argument (mu4e-msg-field msg :path)))
	 (partnum
	  (shell-command-to-string
	   (format "%s extract %s | grep 'text/html' | awk '{print $1}'"
		   mu4e-mu-binary shellpath))))
    (unless (> (length partnum) 0)
      (error "No html part for this message"))
    (call-process-shell-command
     (format "cd %s; %s extract %s --parts=%s --overwrite --play"
	     (shell-quote-argument temporary-file-directory)
	     mu4e-mu-binary shellpath (substring partnum 0 -1)))))

(autoload 'mu4e "mu4e")
(eval-after-load "mu4e"
 '(progn
    (require 'org-mu4e)
    (add-to-list 'mu4e-view-actions
		 '("View in browser" ?v mu4e-action-view-in-browser) t)))

(provide 'my-mu4e)
