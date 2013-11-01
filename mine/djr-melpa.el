(djr/ensure-package 'melpa)

;; For the filtering of packages per repository
(setq package-archive-enable-alist
      '(("melpa"
	 helm)))

(setq package-archive-exclude-alist
	'(("melpa"
	   magit)))

(provide 'djr-melpa)
