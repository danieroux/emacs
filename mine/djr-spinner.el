(use-package spinner
  :ensure t
  :commands djr/spinner-stop
  :config 
  (progn 
    (defun djr/spinner-start ()
      (spinner-start))

    (defun djr/spinner-stop ()
      (if (bound-and-true-p spinner-current)
          (spinner-stop)))))

(provide 'djr-spinner)
