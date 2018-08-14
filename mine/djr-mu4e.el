;; -*- lexical-binding: t -*-

(let ((default-directory "/usr/local/Cellar/mu/1.0/share/emacs/site-lisp"))
   (normal-top-level-add-subdirs-to-load-path))

(use-package mu4e
  :bind* (("C-c m" . hydra-mail/body))
  :commands (mu4e mu4e-headers-search mu4e-compose-new mu4e~proc-add)

  :init
  (progn
    (defvar djr-mu4e-combined-inbox-bookmark "(maildir:/INBOX OR maildir:/[Gmail]/Starred) AND NOT flag:trashed" "What I consider to be my 'inbox'")

    (setq mu4e-bookmarks `((,djr-mu4e-combined-inbox-bookmark                         "Act-on inbox"                  ?i)
                           ((concat ,djr-mu4e-combined-inbox-bookmark " AND flag:unread")    "Unread to me"                  ?m)
                           ("flag:unread AND NOT flag:trashed"  "Unread messages"               ?v)))

    (setq mu4e-maildir (expand-file-name "~/Maildir"))
    (setq mu4e-drafts-folder "/[Gmail]/.Drafts")
    (setq mu4e-sent-folder   "/[Gmail]/.Sent Mail")

    ;; don't save message to Sent Messages, GMail/IMAP will take care of this
    (setq mu4e-sent-messages-behavior 'delete)

    ;; setup some handy shortcuts
    ;; More shortcuts (with email addresses) in private.el
    (setq mu4e-maildir-shortcuts
          `((,mu4e-drafts-folder    . ?d)))

    ;; Not synced
    (setq mu4e-trash-folder  "/not-really-trash")
    (setq mu4e-refile-folder "/gtd")
    (setq mail-host-address "weft"
          mu4e-get-mail-command "true")

    (when (equal system-type 'darwin)
      (progn
        (setq mu4e-mu-binary "/usr/local/bin/mu"
              mail-host-address "danie-notebook")))

    (setq mu4e-use-fancy-chars nil
          mu4e-attachment-dir "~/Desktop"
          mu4e-headers-results-limit 100
          mu4e-headers-skip-duplicates t
          mu4e-headers-leave-behavior 'apply
          mu4e-view-show-images t
          mu4e-view-image-max-width 800
          mu4e-view-show-addresses t
          mu4e-headers-fields '((:human-date . 12) (:from-or-to . 22) (:subject))
          message-kill-buffer-on-exit t)

    (setq mu4e-html2text-command 'mu4e-shr2text)
    (setq mu4e-view-show-images t)

    (when (fboundp 'imagemagick-register-types)
      (imagemagick-register-types)))

  :config
  (progn
    (use-package mu4e-contrib)
    (use-package org-mu4e)
    (use-package djr-org-mu4e)

    (add-to-list 'mu4e-view-actions '("gopen in gmail" . djr/mu4e-open-message-in-google) t)
    (add-to-list 'mu4e-view-actions '("rview related" . djr/mu4e-view-related-search) t)
    (add-to-list 'mu4e-view-actions '("bview in browser" . mu4e-action-view-in-browser) t)

    ;; mu4e~view-copy-contact c or C-c
    (add-hook 'mu4e-view-mode-hook 'goto-address-mode)
    (add-hook 'mu4e-compose-mode-hook 'turn-off-auto-fill)
    ;; Hack. The cache sometimes gets lost or is never initialised
    (add-hook 'mu4e-compose-mode-hook (lambda () (if (not mu4e~contacts)
                                                (mu4e~request-contacts))))

    (add-hook 'mu4e-headers-mode-hook
              (lambda ()
                (define-key mu4e-headers-mode-map "r" 'djr/mu4e-compose-reply-with-follow-up)
                (define-key mu4e-headers-mode-map "M" 'mu4e~main-toggle-mail-sending-mode)
                ;; If I go to the next message, it means I want the current thread as read.
                (define-key mu4e-headers-mode-map "n" 'djr/mu4e-mark-thread-as-read)
                (define-key mu4e-headers-mode-map "D" 'djr/mu4e-mark-thread-as-deleted)
                (define-key mu4e-headers-mode-map "d" 'mu4e-headers-mark-for-delete)
                (define-key mu4e-headers-mode-map "f" 'djr/mu4e-forward-with-follow-up)))

    (add-hook 'mu4e-view-mode-hook
              (lambda ()
                (define-key mu4e-view-mode-map "r" 'djr/mu4e-compose-reply-with-follow-up)
                (define-key mu4e-view-mode-map "M" 'mu4e~main-toggle-mail-sending-mode)
                (define-key mu4e-view-mode-map "d" 'mu4e-view-mark-for-delete)
                (define-key mu4e-view-mode-map "f" 'djr/mu4e-forward-with-follow-up)))

    (remove-hook 'text-mode-hook 'turn-on-auto-fill)))

;; Ugh.
(defun gnutls-available-p ()
  "Function redefined in order not to use built-in GnuTLS support"
  nil)

(use-package smtpmail
  :commands (smtpmail-send-queued-mail message-send-and-exit)

  :init
  ;; Authentication is handled by ~/.authinfo.gpg with this format:
  ;; machine smtp.gmail.com login USER@gmail.com password PASSWORD port 465
  (setq message-send-mail-function 'smtpmail-send-it
        sendmail-coding-system 'utf-8
        mm-coding-system-priorities '(utf-8)
        smtpmail-stream-type 'ssl
        smtpmail-smtp-server "smtp.gmail.com"
        message-user-fqdn "danieroux.com"
        mail-user-agent 'mu4e-user-agent
        smtpmail-smtp-service 465

        smtpmail-queue-mail nil
        smtpmail-queue-dir (concat mu4e-maildir "/mu4e-queue")))

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

(defun djr/mu4e-mark-thread-as-read ()
  (interactive)
  (mu4e-headers-mark-thread-using-markpair '(read)))

(defun djr/mu4e-mark-thread-as-deleted ()
  (interactive)
  (mu4e-headers-mark-thread-using-markpair '(delete)))

(defun djr/mu4e-open-message-in-google (msg)
  (let* ((msgid (mu4e-message-field msg :message-id))
         (url (concat "https://inbox.google.com/search/rfc822msgid%3A" (url-encode-url msgid))))
    (start-process "" nil "open" url)))

;; http://www.emacswiki.org/emacs/FlySpell#toc5
(defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "afrikaans") "english" "afrikaans")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)))

(provide 'djr-mu4e)
