(use-package counsel
  :ensure t
  :bind* (("C-s" . swiper)
          ("M-x" . counsel-M-x)
          ("M-o" . counsel-find-file)
          ("C-h f" . counsel-describe-function)
          ("C-c C-r" . ivy-resume)
          ("C-h v" . counsel-describe-variable))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t))

(provide 'djr-counsel)
