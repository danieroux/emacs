(global-unset-key [(control x)(control z)])

(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "M-o") 'ido-find-file)
(global-set-key (kbd "M-O") 'recentf-ido-find-file)
(global-set-key (kbd "M-SPC") 'ido-switch-buffer)

(global-set-key "\C-cl" 'org-store-link)

;; Should be mirrored in .tmux.commands
(global-set-key (kbd "<f1>") 'org-capture)
(global-set-key (kbd "S-<f2>") 'org-agenda)
(global-set-key (kbd "<f2>") 'djr/agenda)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(global-set-key (kbd "C-x 2") 'djr/split-window-below)
(global-set-key (kbd "C-x 3") 'djr/split-window-right)

;; Ctrl-Space 
(global-set-key (kbd "C-@") 'er/expand-region)

;; (key-chord-define-global "\h" '(lambda () (interactive) (org-agenda nil "H")))

(add-hook 'nrepl-mode-hook
	  (lambda ()
	    (define-key evil-normal-state-local-map (kbd "M-.") 'nrepl-jump)
	    (define-key evil-normal-state-local-map (kbd "M-,") 'nrepl-jump-back)))

(add-hook 'clojure-mode-hook
	  (lambda ()
	    (define-key evil-normal-state-local-map (kbd "M-.") 'nrepl-jump)
	    (define-key evil-normal-state-local-map (kbd "M-,") 'nrepl-jump-back)))

(add-hook 'dired-mode-hook
	  (lambda ()
	    (mapcar (lambda (state)
		      (evil-declare-key state dired-mode-map
			(kbd "E") 'open-in-external-app))
		    '(normal insert))))

(add-hook 'mu4e-headers-mode-hook
	  (lambda ()
	    (define-key mu4e-headers-mode-map "r" 'djr/mu4e-compose-reply-with-follow-up)
	    (define-key mu4e-headers-mode-map "f" 'djr/mu4e-forward-with-follow-up)))

(add-hook 'mu4e-view-mode-hook
	  (lambda ()
	    (define-key mu4e-view-mode-map "r" 'djr/mu4e-compose-reply-with-follow-up)
	    (define-key mu4e-view-mode-map "f" 'djr/mu4e-forward-with-follow-up)
	    (define-key mu4e-view-mode-map "c" 'djr/mu4e-view-related-search)))

(add-hook 'mu4e-main-mode-hook
	  (lambda ()
	    (define-key mu4e-main-mode-map "g" 'mu4e-update-mail-show-window)))

(add-hook 'org-mode-hook (lambda ()
			   (mapcar (lambda (state)
				     (evil-declare-key state org-mode-map
				       (kbd "M-l") 'org-metaright
				       (kbd "M-h") 'org-metaleft
				       (kbd "M-k") 'org-metaup
				       (kbd "M-j") 'org-metadown
				       (kbd "M-L") 'org-shiftmetaright
				       (kbd "M-H") 'org-shiftmetaleft
				       (kbd "M-K") 'org-shiftmetaup
				       (kbd "M-J") 'org-shiftmetadown))
				   '(normal insert))))

(defun fix-up-xterm-control-arrows ()
  (interactive)
  (let ((map (if (boundp 'input-decode-map)
		 input-decode-map
	       function-key-map)))
    (define-key map "\e[1;5A" [C-up])
    (define-key map "\e[1;5B" [C-down])
    (define-key map "\e[1;5C" [C-right])
    (define-key map "\e[1;5D" [C-left])

    (define-key map "\e[5A"   [C-up])
    (define-key map "\e[5B"   [C-down])
    (define-key map "\e[5C"   [C-right])
    (define-key map "\e[5D"   [C-left])

    (define-key map "\e[1;2A" [S-up])
    (define-key map "\e[1;2B" [S-down])
    (define-key map "\e[1;2C" [S-right])
    (define-key map "\e[1;2D" [S-left])

    (define-key map "\e[1;3A" [M-up])
    (define-key map "\e[1;3B" [M-down])
    (define-key map "\e[1;3C" [M-right])
    (define-key map "\e[1;3D" [M-left])

    (define-key map "\e[1;4A" [M-S-up])
    (define-key map "\e[1;4B" [M-S-down])
    (define-key map "\e[1;4C" [M-S-right])
    (define-key map "\e[1;4D" [M-S-left])

    (define-key map "\e[5~"   [prior])
    (define-key map "\e[6~"   [next])
    (define-key map "\e[5;5~" [C-prior])
    (define-key map "\e[6;5~" [C-next])))

(add-hook 'after-init-hook
	  'fix-up-xterm-control-arrows)

(provide 'djr-keybindings)
