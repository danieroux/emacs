;; -*- lexical-binding: t -*-

;;; Store emails that are send/forwarded/replied to 

;; Invoking djr/org-mu4e-capture-next-message or setting djr/org-mu4e-must-capture-message before composing a message will call org-capture after the email was sent successfully (using the capture template from djr/mu4e-org-mode-capture-template-for-sent-email)

(defvar djr/org-mu4e-must-capture-message nil
  "If set, the next composed mu4e message will automatically be captured with the template specified in djr/mu4e-org-mode-capture-template-for-sent-email")

(defvar djr/mu4e-captured-message-p nil
  "Plist with info about the most recently sent mu4e email for OrgMode purposes")

(defvar djr/mu4e-org-mode-capture-template-for-sent-email "e"
  "The specific template from org-capture-templates to use when capturing a sent email automatically")

(add-hook 'message-sent-hook 'djr/org-mu4e-store-link-on-sent-message)

(add-hook 'message-mode-hook (lambda ()
			       (message-add-action 'djr/org-mu4e-capture-cancel
						   'send 'postpone 'kill)

			       (message-add-action 'djr/capture-sent-message-if-needed
						   'send)))

(defun djr~wipe-brackets (msgid)
  (interactive)
  (remove-if (lambda (c)
	       (or (equal c ?>)
		   (equal c ?<)))
	     msgid))

(defun djr/org-mu4e-store-link-on-sent-message ()
  "Store the sent message in many useful places"
  (interactive)
  (let* ((msgid (message-fetch-field "Message-ID"))
	 (description (message-fetch-field "Subject"))
	 (link (concat "mu4e:msgid:" (djr~wipe-brackets msgid)))
	 (org-link-string (org-make-link-string link description))
	 (captured-message-p
	  `(:type mu4e
		 :description ,description
		 :link ,link
		 :annotation ,org-link-string
		 :message-id ,msgid))
	 (stored-link (list link description)))
    (push stored-link org-stored-links)
    (setq org-store-link-plist captured-message-p
	  djr/mu4e-captured-message-p org-store-link-plist)))

(defun djr/capture-sent-message-if-needed ()
  (interactive)
  (if djr/org-mu4e-must-capture-message
      (progn 
	(setq org-store-link-plist djr/mu4e-captured-message-p)
	(setq org-capture-link-is-already-stored t)
	(org-capture nil djr/mu4e-org-mode-capture-template-for-sent-email))))

(defun djr/org-mu4e-capture-cancel ()
  (interactive)
  (setq djr/org-mu4e-must-capture-message nil 
	global-mode-string (delq 'djr-org-capture-mode-line-string global-mode-string)))
(djr/org-mu4e-capture-cancel)

(defun djr/org-mu4e-capture-next-message ()
  (setq djr/org-mu4e-must-capture-message t
	djr-org-capture-mode-line-string "Org capturing current mail")
  (or global-mode-string (setq global-mode-string '("")))
  (or (memq 'djr-org-capture-mode-line-string global-mode-string)
      (setq global-mode-string
	    (append global-mode-string '(djr-org-capture-mode-line-string)))))

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

;;; Redefines

(defun org-mu4e-store-link ()
  "Redefined to never store queries, just messages. I want to capture the message at point in headers mode as well."
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

;;; Define djr/mu4e-to-org that turns an email query into an OrgMode file

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

(defun djr/mu4e-to-org (mu4e-query &optional continue-fun)
  "Execute the query and turn that into an OrgMode file with links to the messages"
  (interactive)
  (mu4e)
  (setq mu4e-headers-include-related nil)
  (setq djr~mu4e-inbox-buffer-name "*djr-mu4e-inbox*")
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
  (mu4e~headers-search-execute mu4e-query 't))

(provide 'djr-org-mu4e)
