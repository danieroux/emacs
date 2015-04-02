(use-package ido
  :ensure t
  :bind* ("M-o" . ido-find-file)

  :init
  (ido-mode 'files)

  :config
  (progn
    (use-package ido-ubiquitous
      :init (ido-ubiquitous-mode)
      :ensure t))

  (use-package ido-vertical-mode
    :ensure t
    :init (ido-vertical-mode))

  (use-package flx-ido
    :ensure t
    :pin "melpa"
    :init (flx-ido-mode t)
    :config
    (setq flx-ido-use-faces nil
	  flx-ido-threshold 2000))

  (setq ido-enable-flex-matching t
	ido-everywhere t
	ido-enable-regexp t
	ido-use-url-at-point t
	ido-use-filename-at-point 'guess
	;; Use the current window when visiting files and buffers with ido
	ido-default-file-method 'selected-window
	ido-default-buffer-method 'selected-window
	;; Use the current window for indirect buffer display
	org-indirect-buffer-display 'current-window))

(provide 'djr-ido)
