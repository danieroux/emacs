;; Make Emacs feel like home

(djr/ensure-package 'evil)

(require 'evil)

(setq evil-want-C-i-jump nil
      evil-normal-state-cursor '("green" box))

(dolist (mode '(mu4e-main-mode
		mu4e-headers-mode
		mu4e-view-mode))
  (push mode evil-emacs-state-modes))

(add-hook 'mu4e-view-mode-hook 'evil-emacs-state)

(evil-mode t)

(provide 'djr-vim)
