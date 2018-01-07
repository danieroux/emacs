(use-package djr-xmonad)

(bind-key* "<f10>" 'toggle-frame-fullscreen)

(global-unset-key [(control x)(control z)])
(global-unset-key [(control c)(control a)])

(global-set-key (kbd "M-/") 'djr/helm-occur-org)
(global-set-key (kbd "M-?") 'djr/helm-occur-my-brain)

(global-unset-key (kbd "<f2>"))

(when *my-primary-emacs-instance*
  (bind-key* "<f2> p" 'djr/pull))

(global-set-key (kbd "C-x 2") 'djr/split-window-below)
(global-set-key (kbd "C-x 3") 'djr/split-window-right)

(defun tpa ()
  (interactive)
  (message "%s" (text-properties-at (point))))

(define-key evil-normal-state-map (kbd "Q") 'tpa)

(add-hook 'org-mode-hook
	  (lambda ()
	    (define-key evil-normal-state-local-map (kbd "M-.") 'org-open-at-point)))

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

(provide 'djr-keybindings)
