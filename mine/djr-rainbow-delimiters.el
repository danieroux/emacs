(require 'rainbow-delimiters)

(dolist (mode '(emacs-lisp-mode-hook
		clojure-mode-hook))
  (add-hook mode 'rainbow-delimiters-mode))

(provide 'djr-rainbow-delimiters)
