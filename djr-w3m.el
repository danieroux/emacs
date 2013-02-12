(add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m")

(setq browse-url-browser-function 'w3m-browse-url
      w3m-use-cookies t)

(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

(provide djr-w3m)
