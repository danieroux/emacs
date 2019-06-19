;;; djr.el  --- Danie Roux's new and improved Emacs Config.

;;; Commentary:
;; What if I have all my config in one file?
;; For many years I've had many files, and I liked it that way.  Now
;; I'm considering keeping this file under a 1000 lines as an exercise
;; in minimalism - decided this day, 2019-06-19.

;;; Mark Start time
(defconst emacs-start-time (current-time))

;;; Bootstrap straight.el
(setq straight-use-package-by-default 't)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;; Standard Emacs packages and twiddles

(use-package cl :defer t :straight nil)
(use-package uniquify :defer t :straight nil)
(use-package ansi-color :defer t :straight nil)
(use-package flymake :defer t :straight nil)
(use-package ffap :defer t :straight nil)

(use-package paren
  :straight nil
  :init (show-paren-mode t))

(use-package recentf
  :straight nil
  :init (recentf-mode t))

(use-package saveplace
  :straight nil
  :init (setq-default save-place t))

(turn-off-auto-fill)
(put 'narrow-to-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)
(setq-default indent-tabs-mode nil)

;;; Custom functions

(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))

;; From spacemacs
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (if (region-active-p)
	(progn
	  (indent-region (region-beginning) (region-end))
	  (message "Indented selected region."))
      (progn
	(evil-indent (point-min) (point-max))
	(message "Indented buffer.")))
    (whitespace-cleanup)))

(defun djr/prepend-to-paths (plain-path)
  "Adds directory to exec, ENV and eshell paths"
  (let ((path (expand-file-name plain-path)))
    (setq exec-path (cons path exec-path))
    (setenv "PATH" (concat path ":" (getenv "PATH") ":"))
    (setq eshell-path-env (getenv "PATH"))))

;;; Path management
(dolist (path '("/usr/local/bin"
		"/opt/local/bin"
		"~/bin"
		"/usr/local/sbin"))
  (djr/prepend-to-paths path))

;;; looks
(use-package zenburn-theme
  :config
  (progn
    (load-theme 'zenburn t)

    (when (equal system-type 'darwin)
      (add-hook 'window-setup-hook
		(lambda nil
		  (set-face-attribute 'default nil
				      :family "Monaco"
				      :height 180
				      :weight 'normal)
		  (toggle-frame-maximized))))

    ;; Kill the tool bar and scroll bar
    ;; I leave the menu bar, because I use it to discover features available in a mode
    (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
    (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

    (global-hl-line-mode 1)
    (global-highlight-changes-mode -1)
    (global-visual-line-mode t)

    ;; Turn "lambda" into λ - for example. In all modes that support it.
    (global-prettify-symbols-mode t)

    (blink-cursor-mode (- (*) (*) (*)))

    (setq visible-bell t
	  inhibit-startup-message t
	  initial-scratch-message nil
	  transient-mark-mode t
	  font-lock-maximum-decoration t)))

;;; Modeline

;; From http://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html

(make-face 'mode-line-face)
(set-face-attribute 'mode-line-face nil)

(make-face 'mode-line-evil-status-normal-face)
(set-face-attribute 'mode-line-evil-status-normal-face nil)

(make-face 'mode-line-evil-status-insert-face)
(set-face-attribute 'mode-line-evil-status-insert-face nil
		    :foreground "#0000ff"
		    :background "#eab700")

(make-face 'mode-line-evil-status-emacs-face)
(set-face-attribute 'mode-line-evil-status-emacs-face nil
		    :foreground "#ffffff"
		    :background "#e80000")

(make-face 'mode-line-buffer-status-face)
(set-face-attribute 'mode-line-buffer-status-face nil)

(make-face 'mode-line-buffer-status-face-modified)
(set-face-attribute 'mode-line-buffer-status-face-modified nil
		    :inherit 'mode-line-buffer-status-face
		    :foreground "#ffffff"
		    :background "#e80000")

(make-face 'mode-line-batter-low-face)
(set-face-attribute 'mode-line-batter-low-face nil
		    :inherit 'mode-line-buffer-status-face-modified)

(make-face 'mode-line-directory-face)
(set-face-attribute 'mode-line-directory-face nil)

(make-face 'mode-line-filename-face)
(set-face-attribute 'mode-line-filename-face nil
		    :foreground "#eab700"
		    :weight 'bold)

(make-face 'mode-line-mode-face)
(set-face-attribute 'mode-line-mode-face nil
		    :foreground "gray80")

(setq djr-mode-line-modified-never-saveable
      (list 'org-agenda-mode
	    'org-agenda-commands-mode
	    'magit-status-mode
	    'twittering-mode
	    'ert-results-mode
	    'eshell-mode))

(setq djr-mode-line-buffer-status
      '(:eval (cond
	       ((and (buffer-modified-p)
		     (not (member major-mode djr-mode-line-modified-never-saveable)))
		(propertize "*" 'face 'mode-line-buffer-status-face-modified))
	       (t
		(propertize " " 'face 'mode-line-buffer-status-face)))))

;; Always enable display-battery
(display-battery-mode)
;; Remaining minutes
(setq battery-mode-line-format "%m")

(setq djr-mode-line-battery-status
      '(:eval (let ((remaining (string-to-number battery-mode-line-string)))
		(if (and (< 0 remaining)
			 (< remaining 30))
		    (propertize battery-mode-line-string 'face 'mode-line-batter-low-face)
		  (propertize "✓" 'face 'mode-line-face)))))

(setq display-time-24hr-format t
      display-time-string-forms '(24-hours ":" minutes)
      display-time-format "%R")

(setq display-time-format "%I:%M:S")

(display-time)

(setq djr-mode-line-time
      '(:eval (propertize display-time-string 'face 'mode-line-face)))

(setq djr-mode-line-evil-status
      '(:eval (cond
	       ((evil-normal-state-p)
		(propertize "V" 'face 'mode-line-evil-status-normal-face))
	       ((evil-insert-state-p)
		(propertize "I" 'face 'mode-line-evil-status-insert-face))
	       ((member major-mode evil-emacs-state-modes)
		(propertize "E" 'face 'mode-line-evil-status-normal-face))
	       ((evil-emacs-state-p)
		(propertize "E" 'face 'mode-line-evil-status-emacs-face))
	       (t
		(propertize "?" 'face 'mode-line-evil-status-emacs-face)))))

(setq djr-mode-line-buffer-name
      '(:eval (cond
	       ((buffer-file-name)
		(list
		 (propertize (abbreviate-file-name (file-name-directory (buffer-file-name))) 'face 'mode-line-directory-face)
		 (propertize (file-name-nondirectory (buffer-file-name)) 'face 'mode-line-filename-face)))
	       (t
		(propertize "%b" 'face 'mode-line-filename-face)))))

(setq djr-mode-line-mode-name
      '(:eval (cond ((string-equal "Fundamental" mode-name) "")
		    (t
		     (propertize mode-name 'face 'mode-line-mode-face)))))

(setq djr-mode-line-possible-spinner
      '(:eval (cond
	       ((bound-and-true-p spinner-current)
		(spinner-print spinner-current))
	       (t "-- "))))

(setq djr-mode-line-format
      (list djr-mode-line-possible-spinner
	    djr-mode-line-evil-status
	    djr-mode-line-buffer-status
	    " "
	    djr-mode-line-buffer-name
	    "  "
	    djr-mode-line-mode-name
	    "  "
	    "-- "
	    djr-mode-line-battery-status
	    " - "
	    djr-mode-line-time
	    ;; '(:eval global-mode-string)
	    ;; "%l"
	    " %-"
	    ))

(setq-default mode-line-format djr-mode-line-format)
(setq mode-line-format djr-mode-line-format)

;;; VIM

;; Make Emacs feel like home

;; Before require evil
(setq evil-want-C-i-jump nil)

(use-package evil-leader
  :init (global-evil-leader-mode)

  :config
  (progn
    (setq evil-leader/leader ","
	  evil-leader/no-prefix-mode-rx '(".*")
	  evil-leader/in-all-states t)

    (evil-leader/set-key
      "c" 'org-capture
      "i" 'id-manager
      "f" 'darkroom-mode
      "t" 'todotxt
      ;; There should be A Better Way
      "SPC" (lambda () (interactive) (insert ", "))
      "RET" (lambda () (interactive) (insert ",") (newline)))))

(use-package evil
  :init (evil-mode t)

  :config
  (progn
    (dolist (mode '(mu4e-main-mode
		    mu4e-headers-mode
		    mu4e-view-mode
		    info-mode
		    elfeed-show-mode
		    elfeed-search-mode
		    twittering-mode
		    inferior-haskell-mode
		    inf-clojure-mode
		    calculator-mode
		    deft-mode
		    ert-results-mode
		    makey-key-mode
		    haskell-error-mode
		    rcirc-groups-mode
		    dig-mode
		    todotxt-mode
		    cider-stacktrace-mode
		    special-mode        ; Lispy. Stupid mode name.
		    cider-test-report-mode
		    *idm-record-dialog*))
      (push mode evil-emacs-state-modes))
    (delete 'rcirc-mode evil-emacs-state-modes)))

(use-package ace-jump-mode
  :commands ace-jump-mode

  :init
  (progn
    (setq evil-ace-jump-active t)
    (define-key evil-normal-state-map (kbd "SPC") 'evil-ace-jump-word-mode)))

(setq evil-normal-state-cursor '("green" box))

;;; Keep ~/.emacs.d clean

(setq autosave-directory "~/tmp/emacs-cache/autosaves/")
(make-directory autosave-directory t)

(setq make-backup-files t ;; do make backups
      backup-by-copying t     ;; and copy them here
      backup-directory-alist '(("." . "~/tmp/emacs-cache/backups"))
      version-control t
      kept-new-versions 2
      kept-old-versions 5
      delete-old-versions t
      auto-save-list-file-prefix (concat autosave-directory ".saves-")
      auto-save-file-name-transforms `((".*", autosave-directory t)))

;;; Helm

(use-package helm
  :bind* (("M-SPC" . helm-mini)
	  ("M-x" . helm-M-x)
	  ;; helm-semantic-or-imenu
	  ("M-:" . helm-eval-expression-with-eldoc)
	  ("M-o" . helm-find-files))
  :config
  (progn
    (helm-autoresize-mode 1)

    ;; Make all functions in Emacs that use `completing-read'or `read-file-name' and friends use helm interface
    (helm-mode)
    (unless helm-source-buffers-list
      (setq helm-source-buffers-list
	    (helm-make-source "Buffers" 'helm-source-buffers)))

    (setq helm-mini-default-sources
	  '(helm-source-buffers-list helm-source-recentf helm-source-bookmarks helm-source-buffer-not-found))

    (setq helm-recentf-fuzzy-match t
	  helm-buffers-fuzzy-matching t
	  helm-apropos-fuzzy-match t
	  helm-lisp-fuzzy-completion t
	  helm-M-x-fuzzy-match t)))

(defun djr/helm-occur (buffer-names)
  (helm-multi-occur-1 buffer-names))

(defun djr/helm-occur-my-brain ()
  (interactive)
  (djr/helm-occur (list "brain.org.gpg")))

(defun djr/helm-occur-org ()
  (interactive)
  (djr/helm-occur
   (remove nil (mapcar (lambda (buffer)
			 (with-current-buffer buffer
			   (if (eq major-mode 'org-mode)
			       buffer)))
		       (buffer-list)))))

;;; Projectile

(use-package projectile
  :init
  (projectile-global-mode)

  :config
  (progn
    (setq projectile-enable-caching t)

    (use-package helm-projectile
      :bind* ("M-S-SPC" . helm-projectile))))

;;; Lisp Editing

(defvar djr-lisp-mode-hooks
  '(inf-clojure-mode-hook
    clojure-mode-hook
    emacs-lisp-mode-hook
    lisp-mode-hook
    lisp-interaction-mode-hook
    scheme-mode-hook
    inferior-emacs-lisp-mode)
  "A list of modes that are considered to be LISP modes")

(use-package elisp-slime-nav

  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode))
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook
	      (lambda ()
		(define-key evil-normal-state-local-map (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point)))

    (add-hook 'ielm-mode-hook
	      (lambda ()
		(define-key evil-normal-state-local-map (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point)))))

(require 'info-look)

;; http://oremacs.com/lispy
(use-package lispy
  :defer t
  :init
  (hook-into-modes #'lispy-mode djr-lisp-mode-hooks)

  :config
  (define-key lispy-mode-map "g" nil)
  (define-key lispy-mode-map "u" nil)
  (setq lispy-visit-method 'helm-projectile
	lispy-teleport-global 1
	lispy-cider-connect-method 'cider-connect))

;; https://github.com/noctuid/lispyville
;; Keeps brackets balanced
(use-package lispyville
  :commands lispyville-mode
  :init
  (add-hook 'lispy-mode-hook #'lispyville-mode))

;;; Clojure

;; The `clojure-indent-function` has hardcoded 'always-align statements in it
;; This specifically messes with (:require) statements
(defun djr-tonsky-indent (indent-point state)
  (goto-char (elt state 1))
  (if (clojure--not-function-form-p)
      (1+ (current-column))
    (forward-char 1)
    (clojure--normal-indent calculate-lisp-indent-last-sexp 'always-indent)))

(use-package clojure-mode
  :mode (("\\.edn$" . clojure-mode)
	 ("\\.repl$" . clojure-mode)
	 ("\\.cljs$" . clojurescript-mode))

  :init
  (progn
    (add-hook 'clojure-mode-hook
	      (lambda ()
		;; Enable https://tonsky.me/blog/clojurefmt
		(setq clojure-indent-style 'always-indent)
		(setq clojure-align-forms-automatically t)
		(setq-local lisp-indent-function #'djr-tonsky-indent)))
    (add-hook 'clojure-mode-hook
	      (lambda ()
		(define-key evil-normal-state-local-map (kbd "M-.") 'sotclojure-find-or-define-function)
		(setq evil-symbol-word-search t))))

  :config
  (evil-leader/set-key "mj" 'cider-connect-clj&cljs))

(use-package cider
  :config
  ;; Expects a function in the user namespace
  (cider-register-cljs-repl-type 'user-custom "(start-cljs-repl)")
  (setq nrepl-log-messages nil)
  (setq nrepl-sync-request-timeout nil)
  (setq clojure-align-forms-automatically t)
  (setq cider-prompt-for-symbol nil))

(use-package clj-refactor
  :hook (clojure-mode . (lambda ()
			  (clj-refactor-mode 1)
			  (yas-minor-mode 1)))

  :config
  (setq cljr-warn-on-eval nil)
  (setq cljr-middleware-ignored-paths ".*cljs")

  ;; Lifted from cljr--add-keybindings
  (dolist (details cljr--all-helpers)
    (let ((key (car details))
	  (fn (cadr details)))
      (evil-leader/set-key (concat "r" key) fn))))

;; https://github.com/Malabarba/speed-of-thought-clojure
(use-package sotclojure
  :init
  ;; New cider renamed this, and sotclojure tracks the old one
  (defalias 'cider--find-var 'cider-find-var)

  :config
  ;; namespace, require
  (sotclojure-define-function-abbrev "nr" "(:require [$])")
  (sotclojure-define-function-abbrev "ni" "(:import [$])")

  (sotclojure-define-function-abbrev "t" "throw (ex-info \"$\" {:cause e})")
  (sotclojure-define-function-abbrev "rc" ";;; Rich Comments\n(comment\n$\n()\n)")
  (speed-of-thought-mode))

;;; Save on tab-out and auto-revert

(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))
(global-auto-revert-mode t)

;;; Window management

(bind-key* "M-j" (lambda () (interactive) (ignore-errors (windmove-down))))
(bind-key* "M-k" (lambda () (interactive) (ignore-errors (windmove-up))))
(bind-key* "M-h" (lambda () (interactive) (ignore-errors (windmove-left))))
(bind-key* "M-l" (lambda () (interactive) (ignore-errors (windmove-right))))

(bind-key* "M-z" 'delete-other-windows)

(bind-key* "C-x |" 'split-window-horizontally)
(bind-key* "C-x -" 'split-window-vertically)

;;; Company

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)

  :config
  (setq company-idle-delay 0.2))

;;; Flycheck

(use-package flycheck
  :defer t
  :init (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (progn
    (setq flycheck-completion-system 'ido)))

;;; Password management
(use-package id-manager
  :commands id-manager

  :init
  (progn
    (setq idm-database-file "~/Dropbox/Documents/passwords.gpg")))

;;; Paranoia for org file saving

(defvar my-org-really-auto-save t)

(defun djr/save-and-backup ()
  (save-buffer)
  (backup-buffer-copy buffer-file-name
		      (concat buffer-file-name "-" (format-time-string "%Y-%m-%d"))
		      (file-modes buffer-file-name)
		      nil))

(defun djr/really-auto-save-some-modes (&optional args)
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (save-excursion
	(when (and
	       buffer-file-name
	       (buffer-modified-p)
	       (eq major-mode 'org-mode)
	       (eq my-org-really-auto-save t))
	  (message (concat "Really auto-saving (and backing up)" buffer-file-name))
	  (djr/save-and-backup))))))

(setq auto-save-timeout 20
      auto-save-interval 300)

(add-hook 'auto-save-hook 'djr/really-auto-save-some-modes)

;;; Magit

(use-package magit :commands magit-status)

;;; File variables
(setq todotxt-file "~/Dropbox/Documents/todotxt/todo.txt"
      someday-file "~/Dropbox/Documents/gtd/someday_maybe.org.gpg"
      brain-file "~/Dropbox/Documents/brain/brain.org.gpg"
      conversations-file "~/Dropbox/Documents/gtd/conversations.org"
      period-log-file "~/Dropbox/Documents/journal/period.org.gpg"
      daily-log-file "~/Dropbox/Documents/journal/daily.org.gpg"
      matter-log-file "~/Dropbox/Documents/matter/matter-log.org.gpg"
      blog-ideas-file "~/Dropbox/Documents/gtd/blog_ideas.org.gpg")

;;; Org-Mode
(setq org-capture-templates `(("b" "Brain" entry (file ,brain-file) "* %?
  %u

%a")
			      ("l" "liam" entry (file "~/Dropbox/Documents/liam.org") "* %?" :clock-in t :clock-resume t)
			      ("p" "period" entry (file ,period-log-file) "* %U

%?")
			      ("d" "daily" entry (file ,daily-log-file) "* %U

- %?")
			      ;; https://www.farnamstreetblog.com/2014/02/decision-journal
			      ("D" "Decision" entry (file "~/Dropbox/Documents/journal/decisions.org.gpg") "* %U %?
** Situation or context
** Problem statement / frame
** Variables that govern the situation
** Complications or complexity as I see it
** Alternatives that were seriously considered and why they were not chosen
** A paragraph explaining the range of outcomes
** A paragraph explaining what you expect to happen and the reasoning and actual probabilities you assign to each projected outcome
** The time of day you're making the decision and how you feel physically and mentally
%?")))

;;; todotxt

(use-package todotxt)

;;; Emacs Debug

(setq debug-on-error t
      debug-on-quit t)

(defun print-point ()
  (interactive)
  (let* ((text-properties (text-properties-at (point))))
    (message "Text properties %s - overlays: %s"
	     text-properties
	     (overlays-at (point)))))

(define-key evil-normal-state-map (kbd "Q") 'print-point)

;;; Startup Timing
(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
					    emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
	    `(lambda ()
	       (let ((elapsed (float-time (time-subtract (current-time)
							 emacs-start-time))))
		 (message "Loading %s...done (%.3fs) [after-init]"
			  ,load-file-name elapsed))) t))

;;; Sanity check

(defun sanity-check (line-count)
  "Complain about LINE-COUNT."
  (cond ((< line-count 800) (message "Config comfortably under 800 lines: %d" line-count))
	((< line-count 900) (display-warning :warning (format "Config still under 900 lines, for now: %d" line-count)))
	(t (display-warning :error (format "WHY is it so big?! Time to shed: %d" line-count)))))

(save-excursion
  (find-file load-file-name)
  (let* ((line-count (count-lines (point-min) (point-max))))
    (sanity-check line-count)))

(switch-to-buffer "*Messages*")

;;; End
(provide 'djr)
;;; djr.el ends here
