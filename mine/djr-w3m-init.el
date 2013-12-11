(message "Loading the w3m-init file")

(defun djr/w3m-pinboard-add-current-buffer ()
  (interactive)
  (pinboard-add-interactively w3m-current-url w3m-current-title))

(bind-key "*" 'djr/w3m-pinboard-add-current-buffer w3m-mode-map)
(bind-key "i" 'instapaper-add-from-w3m w3m-mode-map)
(bind-key "S" 'w3m-history w3m-mode-map)
(bind-key "s" 'w3m-search w3m-mode-map)

(push '("text/html" "\\.s?html?\\'" ("open" url) nil)
      w3m-content-type-alist)
