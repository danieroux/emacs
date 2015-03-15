; https://github.com/hayamiz/twittering-mode/blob/3.0.x/README.markdown

(use-package twittering-mode
  :pin "melpa"
  :ensure t
  :commands twit
  :defer t

  :config 
  (progn 
    (require 'twittering-mode)

    (setq twittering-use-icon-storage t
	  twittering-use-master-password t
	  twittering-reverse-mode t
	  twittering-timer-interval 600
	  twittering-number-of-tweets-on-retrieval 100)

    (setq twittering-private-info-file (expand-file-name "~/.emacs.d/twittering-mode.gpg"))

    (twittering-disable-unread-status-notifier)

    (add-hook 'twittering-mode-init-hook 'twittering-icon-mode)
    (add-hook 'twittering-edit-mode-hook 'flyspell-mode)

    (defun djr/twittering-browse-first-url-in-tweet ()
      (interactive)
      (twittering-goto-next-uri)
      (browse-url-at-point))

    (bind-key "b" 'djr/twittering-browse-first-url-in-tweet twittering-mode-map)
    (bind-key (kbd "*") 'twittering-favorite twittering-mode-map)))

(defun djr/twittering-fix-clobbering ()
  "twittering-mode seems to clobber the current buffer?"
  (interactive)
  (switch-to-buffer (get-buffer-create "*twittering*"))
  (twit)
  (twittering-visit-timeline "danieroux/will-read"))

(provide 'djr-twitter)
