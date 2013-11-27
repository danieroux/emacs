(require 'eshell)
(require 'em-smart)

(setq eshell-where-to-jump 'begin
      eshell-review-quick-commands nil
      eshell-glob-case-insensitive t
      eshell-smart-space-goes-to-end t)

(add-hook 'eshell-mode-hook
	  (lambda ()
	    (setq pcomplete-ignore-case t)
	    (add-to-list 'eshell-visual-commands "less")))

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

(provide 'djr-eshell)
