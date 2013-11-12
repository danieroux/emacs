(use-package ido)

(djr/ensure-package 'ido-ubiquitous)

(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-ubiquitous t
      ido-enable-regexp t
      ido-use-url-at-point t
      ido-use-filename-at-point 'guess
      ;; Use the current window when visiting files and buffers with ido
      ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window
      ;; Use the current window for indirect buffer display
      org-indirect-buffer-display 'current-window)

(ido-mode 'both)

(provide 'djr-ido)
