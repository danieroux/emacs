;; (package-initialize)

(setq *my-primary-emacs-instance* t)

(setq literate t)

(if literate
    (progn
      (require 'org-install)
      (require 'ob-tangle)
      (require 'org-element)
      (org-babel-load-file
       (expand-file-name "djr.org" user-emacs-directory)))
  (load (concat user-emacs-directory "mine/djr-init")))
