;; -*- lexical-binding: t -*-

;; From emacs-starter-kit
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun djr/initialise-package ()
  (interactive)

  (require 'package)

  (dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
		    ("elpa" . "http://tromey.com/elpa/")
		    ("melpa" . "http://melpa.milkbox.net/packages/")
		    ("gnu" . "http://elpa.gnu.org/packages/")
		    ("org" . "http://orgmode.org/elpa/")))
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

(provide 'djr-defuns)
