; https://github.com/punchagan/org2blog
; (not http://repo.or.cz/r/org2blog.git - the Atom option)

(use-package xml-rpc
  :ensure t)

(add-to-list 'load-path (concat external-dir "/metaweblog"))
(add-to-list 'load-path (concat external-dir "/org2blog"))

(require 'org2blog-autoloads)

(setq org2blog/wp-blog-alist
      `(("wordpress"
	 :url ,wordpress-xmlrpc-url 
	 :username "admin"
	 :password ,wordpress-password))) 

(provide 'djr-blog)
