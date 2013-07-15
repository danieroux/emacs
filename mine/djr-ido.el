(require 'ido)

(djr/ensure-package 'ido-ubiquitous)

(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-ubiquitous t
      ido-enable-regexp t
      ido-use-url-at-point t
      ido-use-filename-at-point 'guess)

(ido-mode 1)

(provide 'djr-ido)
