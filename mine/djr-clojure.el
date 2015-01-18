(use-package clojure-mode :ensure t)
(use-package ac-nrepl :ensure t)
(use-package cider :ensure t)

(setq cider-popup-stacktraces nil
      cider-popup-stacktraces-in-repl t
      cider-repl-pop-to-buffer-on-connect nil
      cider-repl-popup-stacktraces t
      cider-repl-display-in-current-window t
      cider-auto-select-error-buffer t)

(setq auto-mode-alist (cons '("\\.edn$" . clojure-mode) auto-mode-alist))  ; *.edn are Clojure files
(setq auto-mode-alist (cons '("\\.cljs$" . clojure-mode) auto-mode-alist)) ; *.cljs are Clojure files
 
(add-to-list 'same-window-buffer-names "*nrepl*")

(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(dolist (mode '(ac-nrepl-setup paredit-mode subword-mode rainbow-delimiters-mode))
  (add-hook 'cider-repl-mode-hook mode))

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(provide 'djr-clojure)
