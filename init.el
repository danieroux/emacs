;; (package-initialize)

(setq *my-primary-emacs-instance* t)

(setq new-config t)

(if new-config
    (load (expand-file-name "djr.el" user-emacs-directory))
  (load (concat user-emacs-directory "mine/djr-init")))
