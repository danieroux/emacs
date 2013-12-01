(require 'w3m-load)
(require 'org-w3m)

(require 'pinboard)

(setq browse-url-browser-function 'w3m-browse-url
      w3m-default-display-inline-images t
      w3m-use-cookies t)

(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

(defun djr/w3m-pinboard-add-current-buffer ()
  (interactive)
  (pinboard-add-interactively w3m-current-url w3m-current-title))

(bind-key "a" 'djr/w3m-pinboard-add-current-buffer w3m-mode-map)

(provide 'djr-w3m)
