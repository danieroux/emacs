;; Some common LISP-y variables

(defvar djr-lisp-mode-hooks 
  '(cider-mode-hook
    cider-repl-mode-hook
    clojure-mode-hook
    emacs-lisp-mode-hook
    inferior-emacs-lisp-mode)
  "A list of modes that are considered to be LISP modes")

(provide 'djr-lisps)
