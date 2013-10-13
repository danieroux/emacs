(djr/ensure-package 'melpa)

;; For the filtering of packages per repository
(setq package-archive-enable-alist
      '(("melpa"
	 helm)))

(provide 'djr-melpa)
