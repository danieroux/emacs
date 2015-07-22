;; From http://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html

(make-face 'mode-line-evil-status-normal-face)
(set-face-attribute 'mode-line-evil-status-normal-face nil
		    :inherit 'mode-line-face)

(make-face 'mode-line-evil-status-insert-face)
(set-face-attribute 'mode-line-evil-status-insert-face nil
		    :inherit 'mode-line-face
		    :foreground "#0000ff"
		    :background "#eab700")

(make-face 'mode-line-evil-status-emacs-face)
(set-face-attribute 'mode-line-evil-status-emacs-face nil
		    :inherit 'mode-line-face
		    :foreground "#ffffff"
		    :background "#e80000")

(make-face 'mode-line-mail-queued-face)
(set-face-attribute 'mode-line-mail-queued-face nil
		    :inherit 'mode-line-face
		    :foreground "#ffffff"
		    :background "#e80000")

(make-face 'mode-line-buffer-status-face)
(set-face-attribute 'mode-line-buffer-status-face nil
		    :inherit 'mode-line-face)

(make-face 'mode-line-buffer-status-face-modified)
(set-face-attribute 'mode-line-buffer-status-face-modified nil
		    :inherit 'mode-line-buffer-status-face
		    :foreground "#ffffff"
		    :background "#e80000")

(make-face 'mode-line-directory-face)
(set-face-attribute 'mode-line-directory-face nil
		    :inherit 'mode-line-face)

(make-face 'mode-line-filename-face)
(set-face-attribute 'mode-line-filename-face nil
		    :inherit 'mode-line-face
		    :foreground "#eab700"
		    :weight 'bold)

(make-face 'mode-line-mode-face)
(set-face-attribute 'mode-line-mode-face nil
		    :inherit 'mode-line-face
		    :foreground "gray80")

(setq djr-mode-line-modified-never-saveable
      (list 'org-agenda-mode
	    'org-agenda-commands-mode
	    'mu4e-main-mode
	    'mu4e-view-mode
	    'magit-status-mode
	    'mu4e-headers-mode
	    'erc-mode
	    'elfeed-show-mode
	    'elfeed-search-mode
	    'twittering-mode
	    'ert-results-mode
	    'rcirc-mode
            'haskell-interactive-mode
	    'eshell-mode))

(setq djr-mode-line-buffer-status
      '(:eval (cond 
	       ((and (buffer-modified-p)
		     (not (member major-mode djr-mode-line-modified-never-saveable)))
		(propertize "*" 'face 'mode-line-buffer-status-face-modified))
	       (t
		(propertize " " 'face 'mode-line-buffer-status-face)))))

(setq djr-mode-line-mail-status
      '(:eval (if (djr/has-queued-mail-p)
		  (propertize " Unsent Mail " 'face 'mode-line-mail-queued-face))))

(setq djr-mode-line-evil-status
      '(:eval (cond 
	       ((evil-normal-state-p)
		(propertize "V" 'face 'mode-line-evil-status-normal-face))
	       ((evil-insert-state-p)
		(propertize "I" 'face 'mode-line-evil-status-insert-face))
	       ((member major-mode evil-emacs-state-modes)
		(propertize "E" 'face 'mode-line-evil-status-normal-face))
	       ((evil-emacs-state-p)
		(propertize "E" 'face 'mode-line-evil-status-emacs-face))
	       (t
		(propertize "?" 'face 'mode-line-evil-status-emacs-face)))))

(setq djr-mode-line-buffer-name
      '(:eval (cond
	       ((buffer-file-name)
		(list
		 (propertize (abbreviate-file-name (file-name-directory (buffer-file-name))) 'face 'mode-line-directory-face)
		 (propertize (file-name-nondirectory (buffer-file-name)) 'face 'mode-line-filename-face)))
	       (t
		(propertize "%b" 'face 'mode-line-filename-face)))))

(setq djr-mode-line-mode-name
      '(:eval (cond
	       ((string-equal "Fundamental" mode-name) "")
	       (t
		(propertize mode-name 'face 'mode-line-mode-face)))))

(setq djr-mode-line-possible-spinner
      '(:eval (cond
	       ((bound-and-true-p spinner-current)
		(format " %s "
			(elt spinner-current
			     (% spinner--counter
				(length spinner-current)))))
	       (t "-- "))))

(setq djr-mode-line-format 
      (list djr-mode-line-possible-spinner
	    djr-mode-line-mail-status
	    djr-mode-line-evil-status
	    djr-mode-line-buffer-status
	    " "
	    djr-mode-line-buffer-name
	    "  "
	    djr-mode-line-mode-name
	    "  "
	    '(:eval global-mode-string)
	    ; "%l"
	    " %-"))

(setq-default mode-line-format djr-mode-line-format)
(setq mode-line-format djr-mode-line-format)

(provide 'djr-modeline)
