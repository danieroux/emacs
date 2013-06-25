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

(setq djr-mode-line-buffer-status
      '(:eval (cond 
	       ((buffer-modified-p)
		(propertize "*" 'face 'mode-line-buffer-status-face-modified))
	       (t
		(propertize " " 'face 'mode-line-buffer-status-face)))))

(setq djr-mode-line-evil-status
      '(:eval (cond 
	       ((evil-normal-state-p)
		(propertize "V" 'face 'mode-line-evil-status-normal-face))
	       ((evil-insert-state-p)
		(propertize "I" 'face 'mode-line-evil-status-insert-face))
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

(setq djr-mode-line-format 
      (list "-- "
	    djr-mode-line-evil-status
	    djr-mode-line-buffer-status
	    " "
	    djr-mode-line-buffer-name
	    " "
	    djr-mode-line-mode-name
	    " "
	    (propertize "%M" 'face 'font-lock-type-face)
	    "%-"))

(setq-default mode-line-format djr-mode-line-format)
(setq mode-line-format djr-mode-line-format)

(provide 'djr-modeline)
