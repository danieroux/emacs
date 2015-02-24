;; https://github.com/skeeto/elfeed
;; https://github.com/skeeto/.emacs.d/blob/master/etc/feed-setup.el

(use-package elfeed
  :ensure t
  :pin melpa
  :bind* ("<f2> n" . elfeed)
  :config 
  (add-hook 'elfeed-search-mode-hook
	    (lambda ()
	      (bind-key "f" 'djr/elfeed-update-frequent elfeed-search-mode-map)
	      (bind-key "l" 'djr/elfeed-limit elfeed-search-mode-map)
	      (bind-key "B" 'djr/elfeed-open-visible-in-browser elfeed-search-mode-map)
	      (bind-key "R" 'djr/elfeed-mark-all-read-in-buffer elfeed-search-mode-map))))

(use-package org-elfeed)

(setq elfeed-sort-order 'ascending)

;; <---- Copy and paste from https://github.com/skeeto/.emacs.d/blob/master/etc/feed-setup.el

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

;; ----> End copy and paste from https://github.com/skeeto/.emacs.d/blob/master/etc/feed-setup.el

(defun djr/elfeed-mark-all-read-in-buffer ()
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread)
  (elfeed-search-update :force))

(defun djr/elfeed-feeds-with-tag (tag)
  "Find all feed URLs that matches a certain tag"
  (loop for (url . tags) in elfeed-feeds-alist
	when (member tag tags)
	collect url))

(defun djr/elfeed-update-frequent ()
  "Some feeds I want to update many times a day, and I mark them with 'frequent'"
  (interactive)
  (let* ((elfeed-feeds (djr/elfeed-feeds-with-tag 'frequent)))
    (elfeed-update)))

(defun djr/elfeed-get-search-term-from-char (kar)
  (let* ((lookup '((?c . "+comic")
		   (?f . "+frequent")
		   (?h . "haskell")))
	 (search (assoc-default kar lookup)))
    (concat "+unread " search)))

(defun djr/elfeed-limit ()
  "Shortcuts to often used filters"
  (interactive)
  (let* ((limit (read-char "Limit to ..."))
	 (search (djr/elfeed-get-search-term-from-char limit)))
    (setq elfeed-search-filter search)
    (elfeed-search-update :force)))

(defun djr/elfeed-open-visible-in-browser ()
  "Opens all the visible feeds in the browser"
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-browse-url))

(provide 'djr-elfeed)
