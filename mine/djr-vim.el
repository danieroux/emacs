;; Make Emacs feel like home

(djr/ensure-package 'evil)

(setq evil-want-C-i-jump nil
      evil-normal-state-cursor '("green" box))

(require 'evil)

(evil-mode t)

(provide 'djr-vim)
