;; From http://bzg.fr/emacs-strip-tease.html
(defvar bzg-big-fringe-mode nil)
(define-minor-mode bzg-big-fringe-mode
  "Minor mode to use big fringe in the current buffer."
  :init-value nil
  :global t
  :variable bzg-big-fringe-mode
  :group 'editing-basics
  (if (not bzg-big-fringe-mode)
      (set-fringe-style nil)
    (set-fringe-mode
     (/ (- (frame-pixel-width)
           (* 100 (frame-char-width)))
        2))))

(defvar-local djr-pomodoro-top-line-mode-saved-old nil)
(defvar djr-pomodoro-top-mode-line-mode)
(define-minor-mode djr-pomodoro-top-mode-line-mode
  "A minor mode that removes the bottom mode-line and shows the current pomodoro on top."
  :init-value nil
  :global nil
  :variable djr-pomodoro-top-mode-line-mode
  :group 'editing-basics
  (if djr-pomodoro-top-mode-line-mode
      (setq djr-pomodoro-top-line-mode-saved-old mode-line-format
	    mode-line-format nil
	    header-line-format (list '(:eval org-pomodoro-mode-line)))
    (setq mode-line-format djr-pomodoro-top-line-mode-saved-old
	    header-line-format nil
	    djr-pomodoro-top-line-mode-saved-old nil))
  (force-mode-line-update)
  (redraw-display))

(defvar djr-focus-mode nil)
(define-minor-mode djr-focus-mode
  "Combines big fringe and pomodoro on top"
  :init-value nil
  :global nil
  :variable djr-focus-mode
  :group 'editing-basics
  (if djr-focus-mode
      (progn
        (set-frame-parameter nil 'fullscreen 'fullboth)
	(bzg-big-fringe-mode 1)
	(djr-pomodoro-top-mode-line-mode 1))
    (progn
      (bzg-big-fringe-mode -1)
      (djr-pomodoro-top-mode-line-mode -1)
      (set-frame-parameter nil 'fullscreen 'fullscreen-restore))))

(provide 'djr-focus-mode)
