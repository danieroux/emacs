(use-package f
  :ensure t)

;; The `clojure-indent-function` has hardcoded 'always-align statements in it
;; This specifically messes with (:require) statements
(defun djr-tonsky-indent (indent-point state)
  (goto-char (elt state 1))
  (if (clojure--not-function-form-p)
      (1+ (current-column))
    (forward-char 1)
    (clojure--normal-indent calculate-lisp-indent-last-sexp 'always-indent)))

(use-package clojure-mode
  :ensure t

  :mode (("\\.edn$" . clojure-mode)
         ("\\.repl$" . clojure-mode)
	 ("\\.cljs$" . clojurescript-mode))

  :init
  (progn
    (add-hook 'clojure-mode-hook
              (lambda ()
                ;; Enable https://tonsky.me/blog/clojurefmt
                (setq clojure-indent-style 'always-indent)
                (setq clojure-align-forms-automatically t)
                (setq-local lisp-indent-function #'djr-tonsky-indent)))
    (add-hook 'clojure-mode-hook
              (lambda ()
                (define-key evil-normal-state-local-map (kbd "M-.") 'sotclojure-find-or-define-function)
                (setq evil-symbol-word-search t))))

  :config
  (evil-leader/set-key "mj" 'cider-connect-clj&cljs))

(use-package cider
  :ensure t
  :pin melpa-stable

  :config

  ;; Expects a function in the user namespace
  (cider-register-cljs-repl-type 'user-custom "(start-cljs-repl)")
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
  :config

  ;; namespace, require
  (sotclojure-define-function-abbrev "nr" "(:require [$])")
  (sotclojure-define-function-abbrev "ni" "(:import [$])")

  (sotclojure-define-function-abbrev "t" "throw (ex-info \"$\" {:cause e})")
  (sotclojure-define-function-abbrev "rc" ";;; Rich Comments\n(comment\n$\n()\n)")
  (speed-of-thought-mode)

  :init
  
  ;; New cider renamed this, and sotclojure tracks the old one
  (defalias 'cider--find-var 'cider-find-var))

;; (use-package zprint-mode
;;   :hook (clojure-mode clojurescript-mode clojurec-mode))

(provide 'djr-clojure)
