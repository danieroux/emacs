;; https://github.com/skeeto/elfeed
;; https://github.com/skeeto/.emacs.d/blob/master/etc/feed-setup.el

(djr/ensure-melpa-package 'elfeed)

(require 'org-elfeed)

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

(defun djr/elfeed-rewrite-linked-in (title)
  "Modify the LinkedIn feed title to to make scanning much easier"
  (or (save-match-data
	(and (string-match "\\(.*\\) is now connected to \\(.*\\)" title)
	     (let* ((known (match-string 1 title))
		    (new (match-string 2 title)))
	       (and new
		    known
		    (concat "NC: " new " -> " known)))))
      (save-match-data
	(and (string-match "\\(.*\\) has updated their current title to \\(.*\\)" title)
	     (let* ((person (match-string 1 title))
		    (new-job-title (match-string 2 title)))
	       (and person
		    new-job-title
		    (concat "NT: " person " -> " new-job-title)))))
      title))

(require 'ert)
(ert-deftest test-djr/elfeed-rewrite-linked-in ()
  "Ensure that only LinkedIn connections titles get rewritten"
  (should (equal "As is" (djr/elfeed-rewrite-linked-in "As is")))
  (should (equal "connected" (djr/elfeed-rewrite-linked-in "connected")))
  (should (equal "is now connected to" (djr/elfeed-rewrite-linked-in "is now connected to")))
  (should (equal "NC: Sarie Smit (Some Random Job Title) -> Piet Potgieter"
		 (djr/elfeed-rewrite-linked-in "Piet Potgieter is now connected to Sarie Smit (Some Random Job Title)")))
  (should (equal "NT: Sarie Smit -> Some Random Job Title at Somewhere"
		 (djr/elfeed-rewrite-linked-in "Sarie Smit has updated their current title to Some Random Job Title at Somewhere"))))

(defun djr/elfeed-entry-filter-title (entry)
  "Filter entry and change title if required"
  (let ((current (elfeed-entry-title entry)))
    (setf (elfeed-entry-title entry)
          (djr/elfeed-rewrite-linked-in current))))

(add-hook 'elfeed-new-entry-hook 'djr/elfeed-entry-filter-title)

(provide 'djr-elfeed)
