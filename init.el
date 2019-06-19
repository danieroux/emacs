;; (package-initialize)

(setq *my-primary-emacs-instance* t)

(setq literate t)

(if literate
    (load (expand-file-name "djr.el" user-emacs-directory))
  (load (concat user-emacs-directory "mine/djr-init")))
