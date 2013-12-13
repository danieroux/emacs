(require 'djr-addressbook)

(setq *osx* t)

(setq locate-command "mdfind")

(add-hook 'window-setup-hook
	  (lambda nil
	    (set-face-attribute
	     'default nil
	     :family "Monaco"
	     :height 180
	     :weight 'normal)))

(provide 'djr-osx)
