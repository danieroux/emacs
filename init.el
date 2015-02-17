;; Inspiration from:
;; - https://github.com/technomancy/emacs-starter-kit
;; - http://www.djcbsoftware.nl/dot-emacs.html

;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)
;; (byte-recompile-directory (expand-file-name "~/.emacs.d/mine") 0)

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

;; My own files
(setq my-dir (concat dotfiles-dir "mine"))

(dolist (config-directory `(,external-dir
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

(add-to-list 'load-suffixes ".el.gpg")

;; My packages
(use-package private)

(when (equal system-type 'darwin)
  (use-package djr-osx))

(use-package djr-looks)
(use-package djr-ido)
(use-package djr-smex)
(use-package djr-vim)
(use-package djr-mu4e)
(use-package djr-org-mode)
(use-package djr-really-autosave)
(use-package djr-clean-emacs-directory)
(use-package djr-calendar)
(use-package djr-autocomplete)
(use-package djr-frequency)
(use-package djr-dired)
(use-package djr-twitter)
(use-package djr-paredit)
(use-package djr-clojure)
(use-package djr-erc)
(use-package djr-blog)
(use-package djr-expand-region)
(use-package djr-magit)
(use-package djr-multiple-cursors)
(use-package djr-modeline)
(use-package djr-easymenu)
(use-package djr-rainbow-delimiters)
(use-package djr-helm)
(use-package djr-projectile)
(use-package djr-guru-mode)
(use-package djr-ace-jump)
(use-package djr-eshell)
(use-package djr-asciidoc)
(use-package djr-elfeed)
;(use-package djr-midnight)
(use-package djr-elisp)
(use-package djr-haskell)
(use-package djr-discover)
(use-package djr-markdown)
(use-package djr-deft)
(use-package djr-spell)
(use-package djr-password-manager)
(use-package djr-emms)
(use-package djr-flycheck)
(use-package djr-intellij)

(use-package djr-keybindings)

(mouse-avoidance-mode 'exile)
(turn-off-auto-fill)

(ansi-color-for-comint-mode-on)
(fset 'yes-or-no-p 'y-or-n-p)

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

(global-auto-revert-mode t)

(setq echo-keystrokes 0.1
      initial-major-mode 'org-mode ;; http://emacsredux.com/blog/2014/07/25/configure-the-scratch-buffers-mode/
      mouse-yank-at-point t
      truncate-partial-width-windows nil
      uniquify-buffer-name-style 'post-forward
      enable-recursive-minibuffers t ;; http://www.masteringemacs.org/articles/2011/10/19/executing-shell-commands-emacs
      minibuffer-depth-indicate-mode t
      redisplay-dont-pause t ;; http://www.masteringemacs.org/articles/2011/10/02/improving-performance-emacs-display-engine
      xterm-mouse-mode t
      gc-cons-threshold 20000000 ;; https://github.com/lewang/flx - GC opt
      gnutls-min-prime-bits nil)

(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)

(setq server-socket-dir "/tmp/danie-emacs-shared")

(server-start)
