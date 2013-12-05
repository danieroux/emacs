; https://github.com/hayamiz/twittering-mode/blob/3.0.x/README.markdown

(add-to-list 'load-path (concat external-dir "/twittering-mode"))

(require 'twittering-mode)

(setq twittering-use-icon-storage t
      twittering-use-master-password t
      twittering-reverse-mode t
      twittering-timer-interval 600
      twittering-number-of-tweets-on-retrieval 100)

(setq twittering-private-info-file (expand-file-name "~/.emacs.d/twittering-mode.gpg"))

(twittering-enable-unread-status-notifier)

(add-hook 'twittering-mode-init-hook 'twittering-icon-mode)
;; (add-hook 'twittering-mode-hook (lambda ()
;;				  (cancel-timer twittering-timer-for-redisplaying)
;;				  (cancel-timer twittering-timer)))

(add-hook 'twittering-edit-mode-hook 'flyspell-mode)

(defun twittering-get-uri-at-point ()
  (or (get-text-property (point) 'uri)
      (if (get-text-property (point) 'field)
	  (let ((id (or (get-text-property (point) 'retweeted-id)
			(get-text-property (point) 'id)))
		(username (get-text-property (point) 'username)))
	    (twittering-get-status-url username id))
	nil)))

(defun twittering-get-tweet-at-point ()
  "Returns the tweet at point (format: \"username: text\") or nil"
  (let* ((username (get-text-property (point) 'username))
	 (text (get-text-property (point) 'text)))
    (if (and username text)
	(format "%s: %s" username text)
      nil)))

(defun org-twittering-store-link ()
  "Stores the web link to a tweet"
  (interactive)
  (if (eq major-mode 'twittering-mode)
      (let* ((uri (twittering-get-uri-at-point))
	     (tweet (twittering-get-tweet-at-point))
	     (org-link (concat "%s:%s" uri tweet)))
	(org-store-link-props
	 :description tweet
	 :type "http"
	 :link uri
	 :url uri)
	uri)))

(add-hook 'org-store-link-functions 'org-twittering-store-link)

(defun djr/twittering-browse-first-url-in-tweet ()
  (interactive)
  (twittering-goto-next-uri)
  (browse-url-at-point))

(bind-key "b" 'djr/twittering-browse-first-url-in-tweet twittering-mode-map)

(provide 'djr-twitter)
