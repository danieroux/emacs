(use-package clojure-mode
  :ensure t

  :mode (("\\.edn$" . clojure-mode)
	 ("\\.cljs$" . clojurescript-mode))

  :config

  (defun figwheel-android-repl ()
    (interactive)
    (inf-clojure "lein figwheel android"))

  (use-package inf-clojure
    :ensure t
    :config
    (progn
      (setq inferior-lisp-program "lein repl")
      (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode))))

(provide 'djr-clojure)
