(use-package zenburn-theme
  :ensure t
  :init
  (load-theme 'zenburn t))

(when (equal system-type 'darwin)
  (add-hook 'window-setup-hook
	    (lambda nil
	      (set-face-attribute
	       'default nil
	       :family "Monaco"
	       :height 180
	       :weight 'normal))))

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
      font-lock-maximum-decoration t)

(require 'bzg-naked)

(defun djr/focus ()
  (interactive)
  (bzg-big-fringe-mode 1)
  (hidden-mode-line-mode 1)
  (djr/make-fullscreen))

(defun djr/break ()
  (interactive)
  (bzg-big-fringe-mode -1)
  (hidden-mode-line-mode -1)
  (toggle-frame-fullscreen))

(provide 'djr-looks)
