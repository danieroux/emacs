(use-package spinner
  :ensure t
  :commands djr/spinner-stop
  :config 
  (progn 
    (defun djr/spinner-start ()
      (spinner-start))

    (defun djr/spinner-stop ()
      (spinner-stop))))

(provide 'djr-spinner)
