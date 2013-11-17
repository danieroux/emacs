;; https://github.com/skeeto/elfeed
;; https://github.com/skeeto/.emacs.d/blob/master/etc/feed-setup.el

(djr/ensure-package 'elfeed)

(defvar elfeed-tagger-db (make-hash-table :test 'equal)
  "Marks what feeds get what tags.")

(defvar feed-patterns
  '((feedburner "http://feeds.feedburner.com/%s")
    (gmane     "http://rss.gmane.org/topics/complete/gmane.%s")
    (subreddit "http://www.reddit.com/r/%s/.rss"))
  "How certain types of feeds automatically expand.")

(defun feed-expand (tags url)
  "Expand URL depending on its TAGS."
  (loop for tag in tags
        when (assoc tag feed-patterns)
        return (format (cadr it) url)
        finally (return url)))

;; elfeed-feeds-alist is defined in ~/.emacs.d/private.el.gpg
(setq elfeed-feeds
      (loop with specials = (mapcar #'car feed-patterns)
            for (url . tags) in elfeed-feeds-alist
            for real-url = (feed-expand tags url)
            do (setf (gethash real-url elfeed-tagger-db) tags)
            collect real-url))

(defun elfeed-tagger-db-tagger (entry)
  "Tag ENTRY using the `elfeed-tagger-db'."
  (let* ((feed-url (elfeed-feed-url (elfeed-entry-feed entry)))
         (tags (gethash feed-url elfeed-tagger-db)))
    (apply #'elfeed-tag entry tags)))

(add-hook 'elfeed-new-entry-hook 'elfeed-tagger-db-tagger)

(provide 'djr-elfeed)
