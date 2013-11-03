(global-unset-key [(control x)(control z)])

(global-set-key (kbd "M-/") 'djr/helm-occur-org)
(global-set-key (kbd "M-?") 'djr/helm-occur-my-brain)

(global-set-key (kbd "M-o") 'ido-find-file)
;; Interferes with arrow keys
;;(global-set-key (kbd "M-O") 'recentf-ido-find-file)
;;(global-set-key (kbd "M-SPC") 'ido-switch-buffer)
(global-set-key (kbd "M-SPC") 'djr/helm)

;; OrgMode link handling
(global-unset-key [(control c) (control l)])
(global-unset-key [(control c) (l)])
(global-set-key (kbd "C-c C-l") 'org-store-link)
(global-set-key (kbd "C-c l") 'org-insert-link)

;; Should be mirrored in .tmux.commands
(global-set-key (kbd "<f1>") 'org-capture)
(global-set-key (kbd "S-<f1>") 'org-capture)
(global-set-key (kbd "S-<f2>") 'org-agenda)
(global-set-key (kbd "<f2>") 'djr/agenda)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(global-set-key (kbd "C-x 2") 'djr/split-window-below)
(global-set-key (kbd "C-x 3") 'djr/split-window-right)

;; Ctrl-Space 
;; (global-set-key (kbd "C-@") 'er/expand-region)

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

(defun djr/enter-org-speedmode ()
  "Moves to start of heading where org-use-speed-commands starts to work. Switch to insert for it to take effect."
  (interactive)
  (org-back-to-heading)
  (evil-change-state 'insert))

(defun djr/org-insert-new-heading ()
  (interactive)
  (end-of-buffer)
  (org-insert-heading-respect-content)
  (evil-change-state 'insert))

(add-hook 'org-agenda-mode-hook
          (lambda ()
            (define-key org-agenda-mode-map "q" 'bury-buffer))
          'append)

(add-hook 'org-mode-hook (lambda ()
			   (mapcar (lambda (state)
				     (fix-up-xterm-control-arrows)
				     (evil-declare-key 'normal org-mode-map
				       (kbd "s") 'djr/enter-org-speedmode)
				     (evil-declare-key state org-mode-map
				       (kbd "M-<right>") 'org-metaright
				       (kbd "M-<left>") 'org-metaleft
				       (kbd "M-<up>") 'org-metaup
				       (kbd "C-<down>") 'djr/org-insert-new-heading

				       (kbd "M-<right>") 'org-metaright
				       (kbd "M-<left>") 'org-metaleft
				       (kbd "M-<up>") 'org-metaup
				       (kbd "M-<down>") 'org-metadown

				       (kbd "M-S-<right>") 'org-shiftmetaright
				       (kbd "M-S-<left>") 'org-shiftmetaleft
				       (kbd "M-S-<up>") 'org-shiftmetaup
				       (kbd "M-S-<down>") 'org-shiftmetadown))
				   '(normal insert))))

(evil-declare-key 'motion calendar-mode-map
  "h" 'calendar-backward-day
  "j" 'calendar-forward-week
  "k" 'calendar-backward-week
  "l" 'calendar-forward-day)

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

;; Does not work, since I start emacsclient sessions - find the hook for that TODO
;; And server-visit-hook does not work either?
(add-hook 'server-visit-hook
	  'fix-up-xterm-control-arrows)

(when *osx*
 (autoload 'omlg-grab-link "org-mac-link")
 (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link))
(require 'org-mac-link)

(provide 'djr-keybindings)
