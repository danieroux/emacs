(use-package f
  :ensure t)

(defun djr/clojure-connect-to-repl ()
  (interactive)
  (let* ((port (f-read ".nrepl-port")))
    (cider-create-sibling-cljs-repl
     (cider-connect "localhost" port))))

(use-package clojure-mode
  :ensure t

  :mode (("\\.edn$" . clojure-mode)
	 ("\\.cljs$" . clojurescript-mode))

  :config

  (use-package clj-refactor
    :ensure t

    :config

    ;; Lifted from cljr--add-keybindings
    (dolist (details cljr--all-helpers)
      (let ((key (car details))
            (fn (cadr details)))
        (evil-leader/set-key (concat "r" key) fn))))

  (use-package cider
    :ensure t

    :config
    (cider-register-cljs-repl-type 'custom-figwheel "(go-figwheel)")
    (clj-refactor-mode 1)
    (yas-minor-mode 1)))

(provide 'djr-clojure)
