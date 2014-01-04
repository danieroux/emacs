(add-to-list 'load-path (concat external-dir "/makey.el"))
(add-to-list 'load-path (concat external-dir "/discover.el"))

(load "makey")
(require 'discover)      
(global-discover-mode 1) 

(provide 'djr-discover)
