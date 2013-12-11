(djr/ensure-melpa-package 'instapaper)

(defun djr/w3m-browse-url-in-new-session (url &rest ignored)
  (w3m-browse-url url t))

(defun djr/browse-on-mac (url &rest ignored)
  (let ((browse-url-browser-function (quote browse-url-generic))
	(browse-url-generic-program "open"))
    (browse-url url)))

(setq browse-url-browser-function '(("sheldoncomics\\|twitter\\|youtube\\|slideshare\\|instagram" . djr/browse-on-mac)
				    ("." . djr/w3m-browse-url-in-new-session)))

(setq w3m-default-display-inline-images t
      w3m-use-cookies t
      w3m-add-tab-number t
      w3m-new-session-in-background t
      w3m-init-file "~/.emacs.d/mine/djr-w3m-init.el" ;; <- loads of things happen in here
      w3m-use-favicon t)

(require 'w3m-load)
(require 'org-w3m)
(require 'instapaper)

(add-to-list 'load-path (concat external-dir "/pinboard-api"))
(require 'pinboard-api)

(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

(provide 'djr-w3m)
