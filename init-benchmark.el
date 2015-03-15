(setq *my-primary-emacs-instance* t)

(add-to-list 'load-path (concat user-emacs-directory "external/benchmark-init-el"))
(require 'benchmark-init-loaddefs)

(benchmark-init/activate)

(load (concat user-emacs-directory "mine/djr-init"))
