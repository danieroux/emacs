;; Inspiration from:
;; - https://github.com/technomancy/emacs-starter-kit
;; - http://www.djcbsoftware.nl/dot-emacs.html

(toggle-debug-on-error)

;; Kill the tool bar and scroll bar
;; I leave the menu bar, because I use it to discover features available in a mode
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(if (eq window-system 'x)
    (add-hook 'window-setup-hook (lambda nil (set-default-font "Bitstream Vera Sans Mono-14"))))
(setq default-frame-alist '((font . "Bitstream Vera Sans Mono 14")))

;; after copy Ctrl+c in X11 apps, you can paste by `yank' in emacs
(setq x-select-enable-clipboard t
;; after mouse selection in X11, you can paste by `yank' in emacs
      x-select-enable-primary t)

(setq dotfiles-dir (file-name-directory load-file-name))
(add-to-list 'load-path dotfiles-dir)

;; Extensions that have been downloaded manually (not through ELPA)
(setq external-dir (concat dotfiles-dir "external"))

;; My own files
(setq my-dir (concat dotfiles-dir "mine"))
(add-to-list 'load-path external-dir)
(add-to-list 'load-path my-dir)

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/opt/local/bin")
(add-to-list 'exec-path "~/bin")
(add-to-list 'exec-path "/usr/pkg/bin")

(setenv "PATH" (concat (getenv "PATH") ":~/bin"))
(setenv "PATH" (concat (getenv "PATH") ":/usr/pkg/bin"))
(setenv "PATH" (concat (getenv "PATH") ":/opt/local/bin"))

;; Standard EMACS packages
(require 'cl)
(require 'uniquify)
(require 'ansi-color)
(require 'flymake)
(require 'ffap)

(require 'paren)
(show-paren-mode 1)

(require 'recentf)
(recentf-mode 1)

(require 'saveplace)
(setq-default save-place t)

(require 'djr-defuns)

(djr/initialise-package)

(djr/ensure-package 'zenburn-theme)
(load-theme 'zenburn t)

;; External packages
; (require 'key-chord)
; (key-chord-mode 1)

(require 'rainbow-delimiters)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(add-to-list 'load-suffixes ".el.gpg")

;; My packages
(require 'private)

(require 'djr-ido)
(require 'djr-smex)
(require 'djr-vim)
(require 'djr-org-mode)
(require 'djr-really-autosave)
(require 'djr-clean-emacs-directory)
(require 'djr-calendar)
(require 'djr-autocomplete)
(require 'djr-frequency)
(require 'djr-dired-open)
(require 'djr-guru-mode)
(require 'djr-keybindings)
(require 'djr-addressbook)
(require 'djr-mu4e)
(require 'djr-offlineimap)
(require 'djr-twitter)
(require 'djr-paredit)
(require 'djr-clojure)
(require 'djr-ledger)
(require 'djr-erc)
;(require 'djr-blog)
(require 'djr-w3m)
(require 'djr-expand-region)
(require 'djr-projectile)
(require 'djr-magit)
(require 'djr-modeline)

(setq browse-url-browser-function (quote browse-url-generic)
      browse-url-generic-program "open")

(mouse-avoidance-mode 'exile)
(turn-off-auto-fill)

(ansi-color-for-comint-mode-on)
(fset 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode 1)

(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-input-method nil)

(icomplete-mode 1) 

(delete-selection-mode 1)

(global-highlight-changes-mode t)

(blink-cursor-mode (- (*) (*) (*)))

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
      xterm-mouse-mode t)

(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)

(setq server-socket-dir "/tmp/danie-emacs-shared")
(server-start)
