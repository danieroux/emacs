;; Small 4 line package installations

(use-package google-this
  :ensure t
  :config (google-this-mode 1))

(use-package beacon
  :ensure t
  :pin melpa
  :config (beacon-mode 1))

(use-package dash-at-point
  :ensure t
  :commands dash-at-point
  :bind* ("C-c d" . dash-at-point))

(use-package discover
  :ensure t
  :init (global-discover-mode 1))

(use-package todotxt
  :ensure t
  :config
  (setq todotxt-file "/Users/danie/Dropbox/todo/todo.txt"))

(use-package adoc-mode
  :ensure t
  :mode ("\\.ad\\'" . adoc-mode))

(use-package csv-mode
  :ensure t)

(use-package yaml-mode
  :ensure t
  ;; http://kaitai.io files
  :mode ("\\.ksy\\'" . yaml-mode))

;(use-package git-gutter
;  :ensure t
;  :config (global-git-gutter-mode +1))

;; https://github.com/k1LoW/emacs-ansible
(use-package ansible
  :ensure t)

(use-package terraform-mode
  :ensure t)

(provide 'djr-smalls)
