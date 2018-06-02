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

  (evil-leader/set-key "mj" 'djr/clojure-connect-to-repl))

(use-package cider
  :ensure t
  :pin melpa-stable

  :config
  (cider-register-cljs-repl-type 'custom-figwheel "(go-figwheel)")
  (clj-refactor-mode 1)
  (yas-minor-mode 1))

(use-package clj-refactor
  :ensure t

  :config

  (setq cljr-warn-on-eval nil)

  ;; Lifted from cljr--add-keybindings
  (dolist (details cljr--all-helpers)
    (let ((key (car details))
          (fn (cadr details)))
      (evil-leader/set-key (concat "r" key) fn))))

(provide 'djr-clojure)
