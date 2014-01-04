;; Inspiration from:
;; - https://github.com/technomancy/emacs-starter-kit
;; - http://www.djcbsoftware.nl/dot-emacs.html

(toggle-debug-on-error)

;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)
;; (byte-recompile-directory (expand-file-name "~/.emacs.d/mine") 0)

;; Kill the tool bar and scroll bar
;; I leave the menu bar, because I use it to discover features available in a mode
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; after copy Ctrl+c in X11 apps, you can paste by `yank' in emacs
(setq x-select-enable-clipboard t
;; after mouse selection in X11, you can paste by `yank' in emacs
      x-select-enable-primary t)

(setq dotfiles-dir (file-name-directory load-file-name))

;; Extensions that have been downloaded manually (not through ELPA)
(setq external-dir (concat dotfiles-dir "external"))

;; jwiegley's: https://github.com/jwiegley/use-package
(add-to-list 'load-path (concat external-dir "/use-package"))
(require 'use-package)
(require 'bind-key)

;; My own files
(setq my-dir (concat dotfiles-dir "mine"))

(dolist (config-directory `(,dotfiles-dir
			    ,external-dir
			    ,my-dir))
  (push config-directory load-path))

;; Standard EMACS packages
(use-package cl)
(use-package uniquify)
(use-package ansi-color)
(use-package flymake)
(use-package ffap)

(use-package paren
  :init (show-paren-mode t))

(use-package recentf
  :init (recentf-mode t))

(use-package saveplace
  :init (setq-default save-place t))

(use-package djr-defuns)

(djr/initialise-package)

(dolist (path '("/usr/local/bin"
		"/opt/local/bin"
		"/usr/pkg/sbin"
		"/usr/pkg/bin"
		"/Users/danie/bin"
		"/Users/danie/Library/Haskell/bin"))
  (djr/prepend-to-paths path))

(djr/ensure-package 'zenburn-theme)
(load-theme 'zenburn t)

(add-to-list 'load-suffixes ".el.gpg")

;; My packages
(use-package private)

(when (equal system-type 'darwin)
  (use-package djr-osx))

(use-package djr-ido)
(use-package djr-smex)
(use-package djr-vim)
(use-package djr-org-mode)
(use-package djr-really-autosave)
(use-package djr-clean-emacs-directory)
(use-package djr-calendar)
(use-package djr-autocomplete)
(use-package djr-frequency)
(use-package djr-dired)
(use-package djr-mu4e)
(use-package djr-twitter)
(use-package djr-paredit)
(use-package djr-clojure)
(use-package djr-erc)
;(use-package djr-blog)
(use-package djr-w3m)
(use-package djr-expand-region)
(use-package djr-projectile)
(use-package djr-magit)
(use-package djr-multiple-cursors)
(use-package djr-modeline)
(use-package djr-easymenu)
(use-package djr-rainbow-delimiters)
(use-package djr-aspell)
(use-package djr-helm)
(use-package djr-guru-mode)
(use-package djr-ace-jump)
(use-package djr-scala)
(use-package djr-eshell)
(use-package djr-asciidoc)
(use-package djr-elfeed)
(use-package djr-midnight)
(use-package djr-elisp)
(use-package djr-haskell)
(use-package djr-discover)

(use-package djr-keybindings)

(mouse-avoidance-mode 'exile)
(turn-off-auto-fill)

(ansi-color-for-comint-mode-on)
(fset 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode 1)

(setq confirm-kill-emacs 'y-or-n-p)

(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-input-method nil)

(icomplete-mode 1) 

(delete-selection-mode 1)

(global-highlight-changes-mode -1)

(global-auto-revert-mode t)

(visual-line-mode t)

(blink-cursor-mode (- (*) (*) (*)))

; No fringe
(set-fringe-mode '(0 . 0))

(setq visible-bell t
      echo-keystrokes 0.1
      font-lock-maximum-decoration t
      inhibit-startup-message t
      transient-mark-mode t
      mouse-yank-at-point t
      truncate-partial-width-windows nil
      uniquify-buffer-name-style 'post-forward
      enable-recursive-minibuffers t ;; http://www.masteringemacs.org/articles/2011/10/19/executing-shell-commands-emacs
      minibuffer-depth-indicate-mode t
      redisplay-dont-pause t ;; http://www.masteringemacs.org/articles/2011/10/02/improving-performance-emacs-display-engine
      xterm-mouse-mode t
      gnutls-min-prime-bits nil)

(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)

(setq server-socket-dir "/tmp/danie-emacs-shared")
(server-start)
