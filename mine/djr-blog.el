; https://github.com/punchagan/org2blog
; (not http://repo.or.cz/r/org2blog.git - the Atom option)

(djr/install-packages '(xml-rpc org2blog))

(require 'org2blog-autoloads)

(setq org2blog/wp-blog-alist
      `(("wordpress"
	 :url ,wordpress-xmlrpc-url 
	 :username "admin"
	 :password ,wordpress-password))) 

(provide 'djr-blog)
