(use-package spinner
  :ensure t
  :config 
  (progn 
    (defun djr/spinner-start ()
      (spinner-start))

    (defun djr/spinner-stop ()
      (spinner-stop))))

(provide 'djr-spinner)
