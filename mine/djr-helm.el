(use-package helm-config
  :ensure helm
  :pin "melpa"
  :bind* ("M-SPC" . djr/helm)
  :init
  (progn
    (use-package helm-files)
    (helm-autoresize-mode 1)
    (unless helm-source-buffers-list
      (setq helm-source-buffers-list
	    (helm-make-source "Buffers" 'helm-source-buffers)))

    (setq helm-recentf-fuzzy-match t
	  helm-buffers-fuzzy-matching t
	  helm-apropos-fuzzy-match t
	  helm-lisp-fuzzy-completion t)))

(defun djr/helm-occur (buffer-names)
  (helm-multi-occur-1 buffer-names))

(defun djr/helm-occur-my-brain ()
  (interactive)
  (djr/helm-occur (list "brain.org.gpg")))

(defun djr/helm-occur-org ()
  (interactive)
  (djr/helm-occur 
   (remove nil (mapcar (lambda (buffer)
			 (with-current-buffer buffer
			   (if (eq major-mode 'org-mode)
			       buffer)))
		       (buffer-list)))))

(defun djr/helm ()
  (interactive)
  (helm-other-buffer '(helm-source-buffers-list
		       helm-source-recentf 
		       helm-source-bookmarks)
		     "*helm*"))

(provide 'djr-helm)
