;; -*- lexical-binding: t -*-

(require 'djr-org-mu4e-capture-sent)

;;; Redefines

(setq org-mu4e-link-query-in-headers-mode nil)

(setq djr~mu4e-to-org-continue-fun nil)

;;; Define djr/mu4e-to-org that turns an email query into an OrgMode file

(defun djr~mu4e-message-to-orgline (msg &optional point)
  (switch-to-buffer djr~mu4e-inbox-buffer-name)
  (let ((msgid (mu4e-message-field msg :message-id))
        (subject (mu4e-message-field msg :subject)))
    (progn
      (insert (format "* [[mu4e:msgid:%s][%s]]" msgid subject))
      (newline))))

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
