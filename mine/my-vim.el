;; Make Emacs feel like home

(add-to-list 'load-path (concat external-dir "/evil"))
(require 'evil)
(evil-mode 1)

(setq evil-normal-state-cursor '("green" box))

(add-to-list 'evil-emacs-state-modes 'mu4e-view-mode)
(add-to-list 'evil-emacs-state-modes 'mu4e-hdrs-mode)
(add-to-list 'evil-emacs-state-modes 'mu4e-main-mode)

(provide 'my-vim)
