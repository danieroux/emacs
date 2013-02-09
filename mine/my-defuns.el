;; From http://stackoverflow.com/questions/1167484/how-to-gracefully-shutdown-emacs-daemon/2270603
(defun shutdown-emacs-server ()
  (interactive)
  (when (not (eq window-system 'x))
    (message "Initializing x windows system.")
    (x-initialize-window-system)
    (when (not x-display-name) (setq x-display-name (getenv "DISPLAY")))
    (select-frame (make-frame-on-display x-display-name '((window-system . x)))) )
  (let ((last-nonmenu-event nil)(window-system "x"))(save-buffers-kill-emacs)))

;; From emacs-starter-kit
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

;; From emacs-starter-kit
(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;; From djcb
(defun djcb-full-screen-toggle ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

;; From emacs-starter-kit
(defun toggle-maximize ()
  (interactive)
  ;; TODO: this only works for X. patches welcome for other OSes.
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

(defun djr/ensure-package (p)
  (when (not (package-installed-p p))
    (package-install p)))

(defun djr/bootstrap ()
  (interactive)
  (package-refresh-contents))

(provide 'my-defuns)
