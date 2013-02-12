;; Make Emacs feel like home

(djr/ensure-package 'evil)

(require 'evil)
(evil-mode 1)

(setq evil-normal-state-cursor '("green" box))

(provide 'my-vim)
