;; Some common LISP-y variables

(defvar djr-lisp-mode-hooks 
  '(inf-clojure-mode-hook
    clojure-mode-hook
    emacs-lisp-mode-hook
    lisp-mode-hook
    lisp-interaction-mode-hook
    scheme-mode-hook
    inferior-emacs-lisp-mode)
  "A list of modes that are considered to be LISP modes")

(provide 'djr-lisps)
