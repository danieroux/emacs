;; https://gist.github.com/danieroux/7838667 from https://gist.github.com/cvmat/3357444

(org-add-link-type "twittering" 'twittering-org:open)
(add-hook 'org-store-link-functions 'twittering-org:store-link)
(defun twittering-org:open (id-str)
  (twittering-visit-timeline (concat ":single/" id-str)))
 
(defun twittering-org:store-link ()
  "Store a link to a tweet."
  (when (and (twittering-buffer-p) (twittering-get-id-at))
    (let ((status (twittering-find-status (twittering-get-id-at))))
      (apply 'org-store-link-props
             :type "twittering"
             :link (concat "twittering:"
                           (or (cdr (assq 'retweeting-id status))
                               (cdr (assq 'id status))))
	     :description (format "@%s: %s"
				  (cdr (assq 'user-screen-name status))
				  (cdr (assq 'text status)))
             :url (twittering-get-status-url-from-alist status)
             :date
             (format-time-string (org-time-stamp-format)
                                 (cdr (assq 'created-at status)))
             :date-timestamp
             (format-time-string (org-time-stamp-format t)
                                 (cdr (assq 'created-at status)))
             (apply 'append
                    (mapcar
                     (lambda (sym)
                       (let ((name (symbol-name sym)))
                       `(,(intern (concat ":" name))
                         ,(or (cdr (assq sym status))
                              (concat "[no " name "]")))))
                     '(text
                       id
                       user-id user-name user-screen-name user-description
                       user-url user-location
                       source source-url
                       retweeting-user-id retweeting-user-name
                       retweeting-user-screen-name
                       retweeting-user-description
                       retweeting-user-url
                       retweeting-user-location
                       retweeting-source retweeting-source-url)))))))

(provide 'twittering-org)
