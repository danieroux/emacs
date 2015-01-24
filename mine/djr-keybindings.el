(require 'djr-xmonad)

(bind-key* "<f10>" 'toggle-fullscreen)

(global-unset-key [(control x)(control z)])
(global-unset-key [(control c)(control a)])

(global-set-key (kbd "M-/") 'djr/helm-occur-org)
(global-set-key (kbd "M-?") 'djr/helm-occur-my-brain)

(global-set-key (kbd "M-o") 'ido-find-file)
;; Interferes with arrow keys
;;(global-set-key (kbd "M-O") 'recentf-ido-find-file)
;;(global-set-key (kbd "M-SPC") 'ido-switch-buffer)
(global-set-key (kbd "M-SPC") 'djr/helm)
(global-set-key (kbd "M-S-SPC") 'helm-projectile)

;; OrgMode link handling
(global-unset-key [(control c) (control l)])
(global-unset-key [(control c) (l)])

(bind-key* "C-c C-l" 'org-store-link)
(bind-key* "C-c l" 'org-insert-link)

;; Some mirrored in .tmux.commands
(global-set-key (kbd "S-<f1>") 'org-capture)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'djr/show-org-agenda-refreshing-if-empty)
(global-set-key (kbd "S-<f3>") 'org-agenda)
(global-set-key (kbd "C-c A") 'org-agenda)
(global-set-key (kbd "<f12>") 'djr/agenda-notebook)
(global-set-key (kbd "S-<f12>") 'djr/agenda-home)
;;(global-set-key (kbd "C-c N") 'djr/agenda-notebook)

(bind-key (kbd "*") 'twittering-favorite twittering-mode-map)

(defun djr/twittering-fix-clobbering ()
  "twittering-mode seems to clobber the current buffer?"
  (interactive)
  (switch-to-buffer (get-buffer-create "*twittering*"))
  (twit)
  (twittering-visit-timeline "danieroux/will-read"))

(global-unset-key (kbd "<f2>"))
(bind-key* "<f2> c" 'djr/mu4e-compose-new-with-follow-up)
(bind-key* "C-c n" 'djr/mu4e-compose-new-with-follow-up)

(bind-key* "<f2> e" 'eshell)
(bind-key* "<f2> g" 'org-clock-goto)
(bind-key* "C-c g" 'org-clock-goto)
(bind-key* "<f2> m" 'djr/mu4e-inbox)
(bind-key* "C-c m" 'djr/mu4e-inbox)
(bind-key* "<f2> p" 'djr/pull)
(bind-key* "<f2> s" 'w3m-search)
(bind-key* "<f2> t" 'djr/twittering-fix-clobbering)
(bind-key* "<f2> n" 'elfeed)
(bind-key* "<f2> w" 'w3m)
; Triggered by QuickSilver
(bind-key* "S-<f3>" 'id-manager)
(bind-key* "C-c i" 'id-manager)

(add-hook 'elfeed-search-mode-hook
	  (lambda ()
	    (bind-key "f" 'djr/elfeed-update-frequent elfeed-search-mode-map)
	    (bind-key "l" 'djr/elfeed-limit elfeed-search-mode-map)
	    (bind-key "B" 'djr/elfeed-open-visible-in-browser elfeed-search-mode-map)
	    (bind-key "R" 'djr/elfeed-mark-all-read-in-buffer elfeed-search-mode-map)))

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(global-set-key (kbd "C-x 2") 'djr/split-window-below)
(global-set-key (kbd "C-x 3") 'djr/split-window-right)

(add-hook 'cider-mode-hook
	  (lambda ()
	    (define-key evil-normal-state-local-map (kbd "M-.") 'cider-jump)
	    (define-key evil-normal-state-local-map (kbd "M-,") 'cider-jump-back)))

(add-hook 'clojure-mode-hook
	  (lambda ()
	    (define-key evil-normal-state-local-map (kbd "M-.") 'cider-jump)
	    (define-key evil-normal-state-local-map (kbd "M-,") 'cider-jump-back)))

(add-hook 'scala-mode-hook
	  (lambda ()
	    (define-key evil-normal-state-local-map (kbd "M-.") 'ensime-edit-definition)
	    (define-key evil-normal-state-local-map (kbd "M-,") 'ensime-pop-find-definition-stack)))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (define-key evil-normal-state-local-map (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point)
	    (define-key evil-normal-state-local-map (kbd "M-,") 'pop-tag-mark)))

(add-hook 'ielm-mode-hook
	  (lambda ()
	    (define-key evil-normal-state-local-map (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point)
	    (define-key evil-normal-state-local-map (kbd "M-,") 'pop-tag-mark)))

(add-hook 'dired-mode-hook
	  (lambda ()
	    (mapcar (lambda (state)
		      (evil-declare-key state dired-mode-map
			(kbd "E") 'open-in-external-app))
		    '(normal insert))))

(add-hook 'mu4e-headers-mode-hook
	  (lambda ()
	    (define-key mu4e-headers-mode-map "r" 'djr/mu4e-compose-reply-with-follow-up)
	    (define-key mu4e-headers-mode-map "M" 'mu4e~main-toggle-mail-sending-mode)
	    (define-key mu4e-headers-mode-map "d" 'mu4e-headers-mark-for-delete)
	    (define-key mu4e-headers-mode-map "f" 'djr/mu4e-forward-with-follow-up)))

(add-hook 'mu4e-view-mode-hook
	  (lambda ()
	    (define-key mu4e-view-mode-map "r" 'djr/mu4e-compose-reply-with-follow-up)
	    (define-key mu4e-view-mode-map "M" 'mu4e~main-toggle-mail-sending-mode)
	    (define-key mu4e-view-mode-map "d" 'mu4e-view-mark-for-delete)
	    (define-key mu4e-view-mode-map "f" 'djr/mu4e-forward-with-follow-up)))

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
