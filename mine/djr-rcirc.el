(use-package rcirc
  :commands rcirc

  :init 
  ;; rcirc-server-alist, rcirc-authinfo and rcirc-default-nick is defined in private.el.gpg
  (setq rcirc-prompt "%n %t> "
	rcirc-scroll-show-maximum-output nil
	rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY")
	rcirc-fill-flag nil
	rcirc-kill-channel-buffers t
	;; Cheap hack to configure the QUIT and PART messages
	rcirc-id-string "look!")

  :config
  (progn 
    ;; brew install terminal-notifier
    ;; https://github.com/nicferrier/rcirc-notify
    (use-package rcirc-notify
      :ensure t)

    (use-package rcirc-groups
      :ensure t)

    (set-face-foreground 'rcirc-dim-nick "grey" nil)

    (defun djr/rcirc-mode-setup ()
      (interactive)
      (setq rcirc-omit-mode nil)
      (rcirc-omit-mode)
      (flyspell-mode 1)
      ;; (set (make-local-variable 'scroll-conservatively) 8192)
      ;; Make C-l "clear" the screen 
      (set (make-local-variable 'recenter-positions) '(top)))

    (defun djr/rcirc-clear-screen ()
      (interactive)
      (goto-char (point-max))
      ;; Assumes (set (make-local-variable 'recenter-positions) '(top))
      (recenter-top-bottom)
      (rcirc-clear-unread (current-buffer)))

    ;; https://www.gnu.org/software/emacs/manual/html_mono/rcirc.html#Reconnecting-after-you-have-lost-the-connection
    ;; Yes, I too don't know why this just isn't part of the codebase.
    (defun-rcirc-command reconnect (arg)
      "Reconnect the server process."
      (interactive "i")
      (unless process
	(error "There's no process for this target"))
      (let* ((server (car (process-contact process)))
	     (port (process-contact process :service))
	     (nick (rcirc-nick process))
	     channels query-buffers)
	(dolist (buf (buffer-list))
	  (with-current-buffer buf
	    (when (eq process (rcirc-buffer-process))
	      (remove-hook 'change-major-mode-hook
			   'rcirc-change-major-mode-hook)
	      (if (rcirc-channel-p rcirc-target)
		  (setq channels (cons rcirc-target channels))
		(setq query-buffers (cons buf query-buffers))))))
	(delete-process process)
	(rcirc-connect server port nick
		       rcirc-default-user-name
		       rcirc-default-full-name
		       channels)))

    ;; http://www.emacswiki.org/emacs/rcircOccur
    (defun-rcirc-command occur (regexp)
      "Run `multi-occur' for all buffers in `rcirc-mode'."
      (interactive "sList lines matching regexp: ")
      (multi-occur (let (result)
		     (dolist (buf (buffer-list))
		       (with-current-buffer buf
			 (when (eq major-mode 'rcirc-mode)
			   (setq result (cons buf result)))))
		     result) regexp))
    
    (bind-key "C-l" 'djr/rcirc-clear-screen rcirc-mode-map)
    (add-hook 'rcirc-mode-hook 'djr/rcirc-mode-setup)
    (rcirc-track-minor-mode)))

(provide 'djr-rcirc)
