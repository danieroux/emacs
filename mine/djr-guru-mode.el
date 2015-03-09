(use-package guru-mode
  :ensure t
  :init
  (progn
    (setq my-affected-bindings-list '(("C-x b" "M-SPC" ido-switch-buffer)
				      ("C-x C-f" "M-o" ido-find-file)
				      ("C-x 1" "M-z" delete-other-windows)
				      ("C-x o" "M-j" other-window)))

    (dolist (cell my-affected-bindings-list)
      (let ((original-key (car cell))
	    (recommended-key (car (cdr cell)))
	    (original-binding (car (cdr (cdr cell)))))
	(define-key guru-mode-map
	  (read-kbd-macro (car cell)) (guru-rebind original-key recommended-key original-binding))))
    
    :idle (guru-global-mode +1)))

(provide 'djr-guru-mode)
