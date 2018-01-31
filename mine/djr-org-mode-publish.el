(setq org-jekyll-use-src-plugin t)

;; http://endlessparentheses.com/how-i-blog-one-year-of-posts-in-a-single-org-file.html
(use-package ox-jekyll)
(use-package ox-jekyll-subtree)

(setq endless/blog-base-url "http://conversations.danieroux.com/")
(setq endless/blog-dir (expand-file-name "~/source/danieroux.github.io/"))

(provide 'djr-org-mode-publish)
