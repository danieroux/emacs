;; Consider helm-dash-browser-func set to eww
(use-package dash-at-point
  :ensure t
  :commands dash-at-point
  :bind* ("C-c d" . dash-at-point))

(provide 'djr-dash)
