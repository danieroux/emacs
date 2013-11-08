(djr/ensure-melpa-package 'helm)

(require 'helm-config)
(require 'helm-files)

(defun djr/helm-occur (buffer-names &optional a-helm-buffer-name)
  (setq helm-multi-occur-buffer-list buffer-names)
  (helm-other-buffer '(helm-source-moccur)
		     (or a-helm-buffer-name
			 "*helm-occur*")))

(defun djr/helm-occur-my-brain ()
  (interactive)
  (djr/helm-occur (list "brain.org")
		  "search my brain"))

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
		       helm-source-bookmarks
		       helm-source-file-cache 
		       helm-source-files-in-current-dir)
		     "*helm*"))

(provide 'djr-helm)
