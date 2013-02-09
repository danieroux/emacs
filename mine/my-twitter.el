(add-to-list 'load-path (concat external-dir "/twittering-mode"))

(require 'twittering-mode)

; (twittering-icon-mode)
(setq twittering-use-master-password t)

(setq twittering-private-info-file (expand-file-name "~/.emacs.d/twittering-mode.gpg"))

(provide 'my-twitter)
