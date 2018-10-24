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

  :init
  (progn
    (add-hook 'clojure-mode-hook
              (lambda ()
                (define-key evil-normal-state-local-map (kbd "M-.") 'cider-find-var))))

  :config
  (evil-leader/set-key "mj" 'djr/clojure-connect-to-repl))

(use-package cider
  :ensure t
  :pin melpa-stable

  :config

  (cider-register-cljs-repl-type 'custom-figwheel "(go-figwheel)")
  (setq nrepl-log-messages nil)
  (setq nrepl-sync-request-timeout nil)
  (setq clojure-align-forms-automatically t)
  (setq cider-prompt-for-symbol nil))

(use-package clj-refactor
  :ensure t

  :hook (clojure-mode . (lambda ()
                          (clj-refactor-mode 1)
                          (yas-minor-mode 1)))
  :config

  (setq cljr-warn-on-eval nil)
  (setq cljr-middleware-ignored-paths ".*cljs")

  ;; Lifted from cljr--add-keybindings
  (dolist (details cljr--all-helpers)
    (let ((key (car details))
          (fn (cadr details)))
      (evil-leader/set-key (concat "r" key) fn))))

;; https://github.com/Malabarba/speed-of-thought-clojure
(use-package sotclojure
  :ensure t
  :config (speed-of-thought-mode))

(provide 'djr-clojure)
