(use-package hydra
  :ensure t
  :pin melpa)

(defhydra launch-hydra ()
  "Launch random useful things"
  ("g" magit-status "Magit")
  ("n" elfeed "Elfeed")
  ("t" djr/twittering-fix-clobbering "Twitter"))

(bind-key* "C-x l" 'launch-hydra/body)

(provide 'djr-hydra)
