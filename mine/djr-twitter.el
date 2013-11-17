; https://github.com/hayamiz/twittering-mode/blob/3.0.x/README.markdown

(add-to-list 'load-path (concat external-dir "/twittering-mode"))

(require 'twittering-mode)

(setq twittering-use-icon-storage t
      twittering-use-master-password t
      twittering-reverse-mode t
      twittering-timer-interval 600)

(setq twittering-private-info-file (expand-file-name "~/.emacs.d/twittering-mode.gpg"))

(twittering-enable-unread-status-notifier)

(add-hook 'twittering-mode-init-hook 'twittering-icon-mode)
;; (add-hook 'twittering-mode-hook (lambda ()
;;				  (cancel-timer twittering-timer-for-redisplaying)
;;				  (cancel-timer twittering-timer)))

(provide 'djr-twitter)
