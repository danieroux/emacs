(use-package eshell
  :init
  (setq eshell-where-to-jump 'begin
	eshell-review-quick-commands 'not-even-short-output
	eshell-glob-case-insensitive t
	eshell-prefer-lisp-functions t
	eshell-smart-space-goes-to-end t)
  :config
  (progn
    (require 'em-smart)
    (add-hook 'eshell-mode-hook 'eshell-smart-initialize)))

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
	  (goto-line line))
      (find-file (pop args)))))
  
(defun eshell/ag (needle)
    (ag/search needle (eshell/pwd)))

(provide 'djr-eshell)
