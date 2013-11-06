;; Follows my ~/.tmux.xmonad.conf keys

;; From https://gist.github.com/mariusaeriksen/287633
(defun djr/window-swap-with (dir)
  (interactive)
  (let ((other-window (windmove-find-other-window dir)))
    (when other-window
      (let* ((this-window  (selected-window))
             (this-buffer  (window-buffer this-window))
             (other-buffer (window-buffer other-window))
             (this-start   (window-start this-window))
             (other-start  (window-start other-window)))
        (set-window-buffer this-window  other-buffer)
        (set-window-buffer other-window this-buffer)
        (set-window-start  this-window  other-start)
        (set-window-start  other-window this-start)
	(select-window other-window)))))

(bind-key* "M-z" 'delete-other-windows)

(bind-key* "C-x |" 'split-window-horizontally)
(bind-key* "C-x -" 'split-window-vertically)

(bind-key* "M-J" (lambda () (interactive) (djr/window-swap-with 'down)))
(bind-key* "M-K" (lambda () (interactive) (djr/window-swap-with 'up)))
(bind-key* "M-H" (lambda () (interactive) (djr/window-swap-with 'left)))
(bind-key* "M-L" (lambda () (interactive) (djr/window-swap-with 'right)))

(bind-key* "M->" (lambda ()
		   (interactive)
		   (enlarge-window 1)
		   (enlarge-window 1 t)))

(bind-key* "M-<" (lambda ()
		   (interactive)
		   (enlarge-window -1)
		   (enlarge-window -1 t)))

(bind-key* "M-j" (lambda () (interactive) (ignore-errors (windmove-down))))
(bind-key* "M-k" (lambda () (interactive) (ignore-errors (windmove-up))))
(bind-key* "M-h" (lambda () (interactive) (ignore-errors (windmove-left))))
(bind-key* "M-l" (lambda () (interactive) (ignore-errors (windmove-right))))

(provide 'djr-xmonad)
