(use-package helm-config
  :ensure helm)

(require 'helm-files)

(helm-autoresize-mode 1)

(setq helm-recentf-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-apropos-fuzzy-match t
      helm-lisp-fuzzy-completion t)

(defun djr/helm-occur (buffer-names &optional a-helm-buffer-name)
  (setq helm-multi-occur-buffer-list buffer-names)
  (helm-other-buffer '(helm-source-moccur)
		     (or a-helm-buffer-name
			 "*helm-occur*")))

(defun djr/helm-occur-my-brain ()
  (interactive)
  (djr/helm-occur (list "brain.org.gpg")
		  "search my brain"))

(defun djr/helm-occur-org ()
  (interactive)
  (djr/helm-occur 
   (remove nil (mapcar (lambda (buffer)
			 (with-current-buffer buffer
			   (if (eq major-mode 'org-mode)
			       buffer)))
		       (buffer-list)))))

(unless helm-source-buffers-list
  (setq helm-source-buffers-list
	(helm-make-source "Buffers" 'helm-source-buffers)))

(defun djr/helm ()
  (interactive)
  (helm-other-buffer '(helm-source-buffers-list
		       helm-source-recentf 
		       helm-source-bookmarks)
		     "*helm*"))

(provide 'djr-helm)
