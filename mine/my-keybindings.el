(global-unset-key [(control x)(control z)])
(global-set-key [f11] 'toggle-maximize)
(global-set-key [S-f11] 'djcb-full-screen-toggle)
(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "M-o") 'ido-find-file)
(global-set-key (kbd "M-O") 'recentf-ido-find-file)
(global-set-key (kbd "M-b") 'ido-switch-buffer)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key (kbd "C-M-r") 'org-capture)
(global-set-key (kbd "<f9>c") 'getcals)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(key-chord-define-global ",a" 'org-agenda)
(key-chord-define-global ",." 'org-velocity-read)
(key-chord-define-global ",r" 'org-capture)
(key-chord-define-global ",h" '(lambda () (interactive) (org-agenda nil "H")))
(key-chord-define-global ",w" '(lambda () (interactive) (org-agenda nil "W")))
(key-chord-define-global ",c" 'compile)

(provide 'my-keybindings)
