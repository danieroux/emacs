(require 'w3m-load)
(require 'org-w3m)

(add-to-list 'load-path (concat external-dir "/pinboard"))
(require 'pinboard)

(defun djr/w3m-browse-url-in-new-session (url &rest ignored)
  (w3m-browse-url url t))

(defun djr/browse-on-mac (url &rest ignored)
  (let ((browse-url-browser-function (quote browse-url-generic))
	(browse-url-generic-program "open"))
    (browse-url url)))

(setq browse-url-browser-function '(("sheldoncomics\|twitter\|youtube" . djr/browse-on-mac)
				    ("." . djr/w3m-browse-url-in-new-session)))

(setq w3m-default-display-inline-images t
      w3m-use-cookies t
      w3m-add-tab-number t
      w3m-new-session-in-background t
      w3m-use-favicon t)

(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

(defun djr/w3m-pinboard-add-current-buffer ()
  (interactive)
  (pinboard-add-interactively w3m-current-url w3m-current-title))

(add-hook 'w3m-load-hook (lambda ()
			   (bind-key "a" 'djr/w3m-pinboard-add-current-buffer w3m-mode-map)
			   (bind-key "S" 'w3m-history w3m-mode-map)

(push '("text/html" "\\.s?html?\\'" ("open" url) nil)
      w3m-content-type-alist)

			   (bind-key "s" 'w3m-search w3m-mode-map)))

(provide 'djr-w3m)
