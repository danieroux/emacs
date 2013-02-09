; https://github.com/punchagan/org2blog
; (not http://repo.or.cz/r/org2blog.git - the Atom option)

(djr/ensure-package 'xml-rpc)
(djr/ensure-package 'org2blog)

(require 'org2blog-autoloads)

(setq org2blog/wp-blog-alist
      `(("wordpress"
	 :url "http://blog.danieroux.com/xmlrpc.php"
	 :username "admin"
	 :password ,wordpress-password))) 

(provide 'djr-blog)
