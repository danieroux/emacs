(use-package djr-helm)

(use-package eshell
  :bind* ("C-'" . shell-switcher-switch-buffer)
  :init
  (setq eshell-where-to-jump 'begin
        eshell-scroll-to-bottom-on-output t
	eshell-review-quick-commands 'not-even-short-output
	eshell-glob-case-insensitive t
	eshell-prefer-lisp-functions t
	eshell-smart-space-goes-to-end t)

  :config
  (progn
    (require 'em-smart)

    (add-hook 'eshell-mode-hook
	      '(lambda ()
                 (eshell/export "EDITOR=emacsclient --socket-name ~/.emacs-server-shared")
		 (bind-key "C-c C-l" 'helm-eshell-history eshell-mode-map)
		 (eshell-smart-initialize)))

    ;; https://github.com/emacs-helm/helm/wiki#helmeshellcompletion
    (add-hook 'eshell-mode-hook
              #'(lambda ()
                  (define-key eshell-mode-map
                    [remap eshell-pcomplete]
                    'helm-esh-pcomplete)))

    (add-hook 'eshell-pre-command-hook 'djr/spinner-start)
    (add-hook 'eshell-post-command-hook 'djr/spinner-stop)

    (use-package shell-switcher
      :ensure t
      :init
      (progn
	(add-hook 'eshell-mode-hook 'shell-switcher-manually-register-shell)
	(setq shell-switcher-mode t)))

    ;; https://github.com/jwiegley/dot-emacs/blob/master/init.el#L2916
    (defun eshell/git (&rest args)
      (cond
       ((or (null args)
	    (and (string= (car args) "status") (null (cdr args))))
	(magit-status default-directory))
       ((and (string= (car args) "log") (null (cdr args)))
	(magit-log))
       (t (throw 'eshell-replace-command
		 (eshell-parse-command
		  (concat "*" command)
		  (eshell-stringify-list (eshell-flatten-list args)))))))

    ;; http://www.emacswiki.org/emacs/EshellFunctions#toc3
    (defun eshell/e (&rest args)
      "Edit a file with +line-number as an option"
      (while args
	(if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
	    (let* ((line (string-to-number (match-string 1 (pop args))))
		   (file (pop args)))
	      (find-file file)
	      (forward-line line))
	  (find-file (pop args)))))

    (defun eshell/ag (needle)
      (ag/search needle (eshell/pwd)))

    (defun djr/execute-in-main-eshell ()
      (interactive)
      (require 'eshell)
      (let* ((start (if (use-region-p)
                        (region-beginning)
                      (save-excursion
                        (beginning-of-line)
                        (point))))
             (end (if (use-region-p)
                      (region-end)
                    (save-excursion
                      (end-of-line)
                      (point))))
             (command (buffer-substring start end)))
        (let ((buf (current-buffer)))
          (unless (get-buffer eshell-buffer-name)
            (eshell))
          (display-buffer eshell-buffer-name t)
          (switch-to-buffer-other-window eshell-buffer-name)
          (end-of-buffer)
          (eshell-kill-input)
          (insert command)
          (eshell-send-input)
          (end-of-buffer)
          (switch-to-buffer-other-window buf))))))

(provide 'djr-eshell)
