(use-package counsel
  :ensure t
  :bind* (("C-s" . swiper)
          ("M-x" . counsel-M-x)
          ("M-o" . counsel-find-file)
          ("C-h f" . counsel-describe-function)
          ("C-h v" . counsel-describe-variable)
          ("C-c C-r" . ivy-resume))

  :config
  (unbind-key "C-j" icomplete-minibuffer-map)
  ;; Does not bind?
  (bind-key "C-j" 'ivy-alt-done ivy-minibuffer-map)
  (bind-key "<return>" 'ivy-alt-done ivy-minibuffer-map)
  (bind-key "<tab>" 'ivy-partial ivy-minibuffer-map)
  (bind-key "C-l" 'ivy-backward-delete-char ivy-minibuffer-map)

  (setq counsel-find-file-at-point t)
  (ivy-mode 1)
  (setq ivy-display-style 'fancy
        ivy-use-virtual-buffers t))

(provide 'djr-counsel)
