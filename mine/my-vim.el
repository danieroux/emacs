;; Make Emacs feel like home

(add-to-list 'load-path (concat external-dir "/evil"))
(require 'evil)
(evil-mode 1)

(setq evil-normal-state-cursor '("green" box))

(provide 'my-vim)
