(use-package clojure-mode
  :ensure t
  :defer t

  :mode (("\\.edn$" . clojure-mode)
	 ("\\.cljs$" . clojure-mode))

  :config
  (use-package cider
    :ensure t
    :defer t
    :config
    (progn
      (add-hook 'cider-repl-mode-hook #'subword-mode)
      (add-hook 'cider-mode-hook #'eldoc-mode)

      (setq cider-show-error-buffer nil
	    cider-repl-history-file "~/.cider_history"
	    cider-popup-stacktraces-in-repl t
	    cider-prompt-save-file-on-load nil
	    cider-repl-pop-to-buffer-on-connect nil
	    cider-repl-popup-stacktraces t
	    cider-repl-display-in-current-window t
	    cider-auto-select-error-buffer t))))

(provide 'djr-clojure)
