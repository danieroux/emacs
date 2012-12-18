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

(setq dotfiles-dir (file-name-directory load-file-name))
(add-to-list 'load-path dotfiles-dir)

;; Initialise ELPA with all three sources I know of
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")
                  ("gnu" . "http://elpa.gnu.org/packages/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(load-theme 'zenburn t)

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

(require 'ido)
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-use-filename-at-point 'guess)
(ido-mode 1)

(require 'paren)
(show-paren-mode 1)

(require 'recentf)
(recentf-mode 1)

(require 'saveplace)
(setq-default save-place t)

;; External packages
(require 'key-chord)
(key-chord-mode 1)

(require 'rainbow-delimiters)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(require 'smex)
(smex-initialize)

;; My packages
(require 'private)
(require 'my-defuns)
(require 'my-vim)
(require 'my-org-mode)
(require 'my-haskell)
(require 'my-autocompile)
(require 'my-really-autosave)
(require 'my-clean-emacs-directory)
(require 'my-calendar)
(require 'my-autocomplete)
; (require 'my-frequency)
(require 'my-dired-open)
(require 'my-guru-mode)
(require 'my-keybindings)
(require 'my-addressbook)
(require 'my-mu4e)
(require 'my-offlineimap)
(require 'my-twitter)
(require 'my-paredit)
(require 'my-clojure)
(require 'my-ledger)
(require 'djr-erc)

(setq browse-url-browser-function (quote browse-url-generic)
      browse-url-generic-program "open")

(mouse-avoidance-mode 'exile)
(turn-off-auto-fill)
(windmove-default-keybindings 'meta)

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

(defun djr/kitchen-sink ()
  "Open my Emacs kitchen sink"
  (interactive)

  (select-frame (make-frame))
  (org-agenda nil "H")
  (delete-other-windows)

  (select-frame (make-frame))
  (twit)

  (select-frame (make-frame))
  (mu4e)

  ;(select-frame (make-frame))
  ;(djr/irc)
)
