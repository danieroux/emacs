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

(defun djr/initialise-package ()
  (interactive)

  (require 'package)

  ;; Initialise ELPA with all three sources I know of
  (dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
		    ("elpa" . "http://tromey.com/elpa/")
		    ("gnu" . "http://elpa.gnu.org/packages/")))
    (add-to-list 'package-archives source t))

  (package-initialize))

(defun djr/ensure-package (p)
  (when (not (package-installed-p p))
    (package-install p)))

(defun djr/install-packages (packages)
  "Ensures that the packages are installed"
  (dolist (p packages)
    (djr/ensure-package p)))

(defun djr/bootstrap ()
  (interactive)
  (djr/initialise-package)
  (package-refresh-contents)
  (save-buffers-kill-terminal))

(defun djr/agenda ()
  (interactive)
  (org-agenda nil "H"))

(defun djr/split-window-below ()
  (interactive)
  (if (getenv "TMUX")
      (shell-command "tmux split-window -h e")
    (split-window-below)))

(defun djr/split-window-right ()
  (interactive)
  (if (getenv "TMUX")
      (shell-command "tmux split-window -v e")
    (split-window-right)))

(defun djr/org-capture-exit ()
  (if (equal "single-frame-capture" (frame-parameter nil 'name))
      (save-buffers-kill-terminal)))

(defun djr/org-capture ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "single-frame-capture")))
  (select-frame-by-name "single-frame-capture")
  (setq word-wrap 1)
  (setq truncate-lines nil)
  (org-capture nil "i"))

(defadvice org-capture-finalize
  (after delete-capture-frame activate)
  (djr/org-capture-exit))

(defadvice org-capture-destroy
  (after delete-capture-frame activate)
  (djr/org-capture-exit))

;; make the frame contain a single window. by default org-capture
;; splits the window.
(add-hook 'org-capture-mode-hook
	  'delete-other-windows)

(provide 'djr-defuns)
