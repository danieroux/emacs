(djr/install-packages '(clojure-mode
			ac-nrepl
			nrepl
			nrepl-ritz))

(require 'ac-nrepl)

(setq nrepl-popup-stacktraces nil
      nrepl-popup-stacktraces-in-repl t)

(setq auto-mode-alist (cons '("\\.edn$" . clojure-mode) auto-mode-alist))  ; *.edn are Clojure files
(setq auto-mode-alist (cons '("\\.cljs$" . clojure-mode) auto-mode-alist)) ; *.cljs are Clojure files
 
(add-to-list 'same-window-buffer-names "*nrepl*")

(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'nrepl-mode-hook 'subword-mode)
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(provide 'djr-clojure)
