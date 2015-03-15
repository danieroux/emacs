;; -*- lexical-binding: t -*-

;; From emacs-starter-kit
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun djr/prepend-to-paths (path)
  "Adds directory to exec, ENV and eshell paths"
  (setq exec-path (cons path exec-path))
  (setenv "PATH" (concat path ":" (getenv "PATH") ":"))
  (setq eshell-path-env (getenv "PATH")))

(defun djr/initialise-package ()
  (interactive)

  (require 'package)

  (dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
		    ("elpa" . "http://tromey.com/elpa/")
		    ("melpa" . "http://melpa.org/packages/")
		    ("gnu" . "http://elpa.gnu.org/packages/")))
    (add-to-list 'package-archives source t))

  (package-initialize))

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

(defun djr/has-mu4e-compose-buffer-open-p ()
  (assq 'mu4e-compose-mode
	(mapcar
	 (lambda (b)
	   (cons (buffer-local-value 'major-mode b) b))
	 (buffer-list))))

(defun djr/queue-dir-has-files-p ()
  (and (boundp 'smtpmail-queue-index-file)
       (let* ((qfile (expand-file-name smtpmail-queue-index-file
				       smtpmail-queue-dir))
	      (size (eighth (file-attributes qfile))))
	 (not (= 0 size)))))

(defun djr/has-queued-mail-p ()
  (or (djr/queue-dir-has-files-p)
      (djr/has-mu4e-compose-buffer-open-p)))

(defun djr/make-fullscreen ()
  (interactive)
   (set-frame-parameter
     nil 'fullscreen
     'fullboth))

(defun djr/pull ()
  "Pull information sources (moving away from push based)"
  (interactive)
  (elfeed-update)
  (twittering-update-active-buffers t)
  (mu4e-update-index))

(defun djr/fetch-mail ()
  (interactive)
  (async-shell-command (expand-file-name "~/bin/mbsync-all-forever") "*mbsync*")
  (bury-buffer))

(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))

(provide 'djr-defuns)
