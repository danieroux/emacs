;; Inspiration from:
;; - https://github.com/technomancy/emacs-starter-kit
;; - http://www.djcbsoftware.nl/dot-emacs.html
;; - https://github.com/jwiegley/dot-emacs/blob/master/init.el
;; - https://github.com/zenspider/elisp

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name)
  
  (setq debug-on-error t
        debug-on-quit t))

;; (byte-recompile-directory (expand-file-name "~/.emacs.d/mine") 0)

;; after copy Ctrl+c in X11 apps, you can paste by `yank' in emacs
(setq x-select-enable-clipboard t
;; after mouse selection in X11, you can paste by `yank' in emacs
      x-select-enable-primary t)

;; Homebrew
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
   (normal-top-level-add-subdirs-to-load-path))

(setq dotfiles-dir user-emacs-directory)

;; Extensions that have been downloaded manually (not through ELPA)
(setq external-dir (concat dotfiles-dir "external"))

;; jwiegley's: https://github.com/jwiegley/use-package
(add-to-list 'load-path (concat external-dir "/use-package"))

(eval-and-compile
  (defvar use-package-verbose t))

(require 'use-package)
(require 'bind-key)

;; My own files
(setq my-dir (concat dotfiles-dir "mine"))

(dolist (config-directory `(,external-dir
			    ,my-dir))
  (push config-directory load-path))

;; Standard EMACS packages
(use-package cl :defer t)
(use-package uniquify :defer t)
(use-package ansi-color :defer t)
(use-package flymake :defer t)
(use-package ffap :defer t)

(use-package paren
  :init (show-paren-mode t))

(when *my-primary-emacs-instance*
  (use-package recentf
    :init (recentf-mode t))

  (use-package saveplace
    :init (setq-default save-place t)))

(use-package djr-defuns)

(djr/initialise-package)

(dolist (path '("/usr/local/bin"
		"/opt/local/bin"
		"~/bin"
		"~/.cabal/bin"
                "/usr/local/sbin"
                "/Applications/ghc-7.10.1.app/Contents/bin"
                "/Users/danie/.stack/programs/x86_64-osx/ghc-7.10.2/bin"
		"~/Library/Haskell/bin"))
  (djr/prepend-to-paths path))

(add-to-list 'load-suffixes ".el.gpg")

;; My packages
(use-package djr-private)

(when (equal system-type 'darwin)
  (use-package djr-osx))

(use-package djr-looks)
(use-package djr-spinner)
(use-package djr-hydra)
(use-package djr-vim)
(use-package djr-clean-emacs-directory)
(use-package djr-dired)
;; (use-package djr-paredit)
(use-package djr-lispy)
(use-package djr-clojure)
;; (use-package djr-blog)
(use-package djr-expand-region)
(use-package djr-multiple-cursors)
(use-package djr-magit)
(use-package djr-modeline)
(use-package djr-rainbow-delimiters)
(use-package djr-helm)
;; (use-package djr-counsel)
(use-package djr-projectile)
(use-package djr-guru-mode)
(use-package djr-eshell)
(use-package djr-midnight :disabled t)
(use-package djr-elisp)
(use-package djr-company)
(use-package djr-haskell)
(use-package djr-discover)
(use-package djr-markdown)
(use-package djr-spell)
(use-package djr-ggtags)
(use-package djr-flycheck)
(use-package djr-intellij)
(use-package djr-focus-mode)
(use-package djr-rcirc)
(use-package djr-typo)
(use-package djr-deft)

(use-package djr-smalls)

(when (equal system-type 'darwin)
  (use-package djr-org-mode)
  (use-package djr-password-manager)
  (use-package djr-really-autosave)
  (use-package djr-mu4e)
  (use-package djr-twitter)
  ;; (use-package djr-elfeed)
  (use-package djr-emms)
  (use-package djr-chrome-editboxes)
  (use-package djr-gmail-message-mode)
  (use-package djr-frequency))

(use-package djr-history)
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

(winner-mode 1)

(setq echo-keystrokes 0.1
      initial-major-mode 'text-mode ;; http://emacsredux.com/blog/2014/07/25/configure-the-scratch-buffers-mode/
      mouse-yank-at-point t
      truncate-partial-width-windows nil
      uniquify-buffer-name-style 'post-forward
      enable-recursive-minibuffers t ;; http://www.masteringemacs.org/articles/2011/10/19/executing-shell-commands-emacs
      minibuffer-depth-indicate-mode t
      redisplay-dont-pause t ;; http://www.masteringemacs.org/articles/2011/10/02/improving-performance-emacs-display-engine
      xterm-mouse-mode t
      gc-cons-threshold 20000000 ;; https://github.com/lewang/flx - GC opt
      gnutls-min-prime-bits nil)

(setq-default indent-tabs-mode nil)

(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)

(setq server-socket-dir "/tmp/danie-emacs-shared")

(server-start)
(when (equal system-type 'darwin)
  (use-package djr-mu4e-mbsync)
  (djr/sync-mail-and-update-mu4e))

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
