(use-package helm-config
  :ensure helm
  :pin "melpa"
  :bind* (("M-SPC" . helm-mini)
	  ("M-x" . helm-M-x)
	  ;; helm-semantic-or-imenu
	  ("M-:" . helm-eval-expression-with-eldoc)
	  ("M-o" . helm-find-files))
  :init
  (progn
    (use-package helm-files)
    (helm-autoresize-mode 1)

    ;; Make all functions in Emacs that use `completing-read'or `read-file-name' and friends use helm interface
    (helm-mode)
    (unless helm-source-buffers-list
      (setq helm-source-buffers-list
	    (helm-make-source "Buffers" 'helm-source-buffers)))

    (setq helm-mini-default-sources
          '(helm-source-buffers-list helm-source-recentf helm-source-bookmarks helm-source-buffer-not-found))

    (setq helm-recentf-fuzzy-match t
	  helm-buffers-fuzzy-matching t
	  helm-apropos-fuzzy-match t
	  helm-lisp-fuzzy-completion t
	  helm-M-x-fuzzy-match t)))

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

(provide 'djr-helm)
