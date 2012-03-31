(add-to-list 'load-path (concat external-dir "/ghc-mod-1.10.11"))

(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))

(provide 'my-haskell)
