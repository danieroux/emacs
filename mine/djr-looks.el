(use-package zenburn-theme
  :ensure t
  :config
  (progn
    (load-theme 'zenburn t)

    (add-to-list 'default-frame-alist '(fullscreen . maximized))
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

(provide 'djr-looks)
