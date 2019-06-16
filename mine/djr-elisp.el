;; org'ed
(use-package elisp-slime-nav
  :ensure t
  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode))
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (define-key evil-normal-state-local-map (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point)))

    (add-hook 'ielm-mode-hook
              (lambda ()
                (define-key evil-normal-state-local-map (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point)))))

(require 'info-look)

(provide 'djr-elisp)
