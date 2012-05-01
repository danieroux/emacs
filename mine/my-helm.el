(add-to-list 'load-path (concat external-dir "/helm"))

(require 'helm-config)

(require 'helm-buffers)
(require 'helm-files)
(require 'helm-org)

(defun my-helm-invoke ()
  (interactive)
  (helm-other-buffer '(helm-c-source-buffers-list
		       helm-c-source-recentf
		       helm-c-source-bookmarks
		       helm-c-source-org-headline)
		     "*my helm*"))

(provide 'my-helm)
