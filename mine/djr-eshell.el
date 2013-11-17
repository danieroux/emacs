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

(provide 'djr-eshell)
