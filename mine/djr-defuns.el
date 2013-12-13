;; -*- lexical-binding: t -*-

;; From emacs-starter-kit
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun djr/prepend-to-paths (path)
  "Adds directory to exec and ENV paths"
  (setq exec-path (cons path exec-path))
  (setenv "PATH" (concat path ":" (getenv "PATH") ":"))
  (setq eshell-path-env (getenv "PATH")))

(defun djr/initialise-package ()
  (interactive)

  (require 'package)

  (dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
		    ("elpa" . "http://tromey.com/elpa/")
		    ("gnu" . "http://elpa.gnu.org/packages/")
		    ("org" . "http://orgmode.org/elpa/")))
    (add-to-list 'package-archives source t))

  (package-initialize))

(defun djr/ensure-package (p)
  (when (not (package-installed-p p))
    (package-install p)))

(defun djr/ensure-melpa-package (p)
  (when (not (package-installed-p p))
    (progn
      (let* ((package-archives '(("melpa" . "http://melpa.milkbox.net/packages/"))))
        (package-initialize)
	(package-refresh-contents)
        (package-install p))
      (djr/initialise-package))))

(defun djr/install-packages (packages)
  "Ensures that the packages are installed"
  (dolist (p packages)
    (djr/ensure-package p)))

(defun djr/bootstrap ()
  "Run this after a fresh git clone - the bootstrap.sh script calls this"
  (interactive)
  (djr/initialise-package)
  (package-refresh-contents)
  (save-buffers-kill-terminal))

(defadvice mu4e-update-index
  (after djr/wait-for-mu4e-update-index)
  (djr/agenda-home)
  (ad-deactivate 'mu4e-update-index))

(defun djr/agenda-full-refresh ()
  (interactive)
  (ad-activate 'mu4e-update-index)
  (mu4e-update-mail-and-index nil))

(defun djr/agenda-home ()
  (interactive)
  (djr~agenda "H"))

(defun djr/agenda-notebook ()
  (interactive)
  (djr~agenda "N"))

(defun djr~agenda (mode)
  (djr/mu4e-to-org (lambda ()
		     (org-mobile-pull)
		     (org-agenda nil mode)
		     (delete-other-windows))))

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

(defun djr/has-queued-mail-p ()
  (let* ((qfile (expand-file-name smtpmail-queue-index-file
				  smtpmail-queue-dir))
	 (size (eighth (file-attributes qfile))))
    (not (= 0 size))))

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(defun djr/pull ()
  "Pull information sources (moving away from push based)"
  (interactive)
  (elfeed-update)
  (twittering-update-active-buffers t)
  (mu4e-update-index))

(provide 'djr-defuns)
