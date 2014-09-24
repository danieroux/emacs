;; -*- lexical-binding: t -*-

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

;;; Store emails that are send/forwarded/replied to 

(add-hook 'message-sent-hook 'djr/org-mu4e-store-link-on-sent-message)

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
  (setq djr/org-mu4e-must-capture-message t
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

(setq djr/org-mu4e-must-capture-message t)

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

(defun djr/mu4e-to-org (&optional continue-fun)
  "Execute a specific, hard-coded query and turn that into an OrgMode file with links to the messages."
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
  (mu4e~headers-search-execute djr-mu4e-combined-inbox-bookmark 't))

(provide 'djr-org-mu4e)
