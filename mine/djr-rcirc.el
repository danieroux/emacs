(use-package rcirc
  :defer t
  :commands rcirc

  :init
  ;; rcirc-server-alist, rcirc-authinfo and rcirc-default-nick are defined in private.el.gpg
  (setq rcirc-prompt "%n %t> "
        rcirc-scroll-show-maximum-output nil
        rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY" "KEEPALIVE")
        rcirc-fill-flag nil
        rcirc-log-flag t
        rcirc-kill-channel-buffers t)

  :config
  (progn
    ;; Cheap hack to configure the QUIT and PART messages
    (setq rcirc-id-string "it has come to this.")

    (defadvice rcirc-format-response-string (after dim-entire-line)
      "Dim whole line for senders whose nick matches `rcirc-dim-nicks'."
      (when (and rcirc-dim-nicks sender
                 (string-match (regexp-opt rcirc-dim-nicks 'words) sender))
        (setq ad-return-value (rcirc-facify ad-return-value 'rcirc-dim-nick))))
    (ad-activate 'rcirc-format-response-string)

    (rcirc-track-minor-mode)

    ;; brew install terminal-notifier
    ;; https://github.com/nicferrier/rcirc-notify
    (use-package rcirc-notify
      :ensure t
      :config
      (rcirc-notify-add-hooks))

    (use-package rcirc-color
      :ensure t)

    (use-package rcirc-groups
      :ensure t)

    (set-face-foreground 'rcirc-dim-nick "grey" nil)

    (defun djr/rcirc-mode-setup ()
      (interactive)
      (setq rcirc-omit-mode nil)
      (rcirc-omit-mode)
      (set (make-local-variable 'scroll-conservatively) 8192))

    (flyspell-mode 1)

    (defun rcirc-handler-NOTICE (process sender args text))

    ;; http://www.emacswiki.org/emacs/rcircOccur
    (defun-rcirc-command occur (regexp)
      "Run `multi-occur' for all buffers in `rcirc-mode'."
      (interactive "sList lines matching regexp: ")
      (multi-occur (let (result)
                     (dolist (buf (buffer-list))
                       (with-current-buffer buf
                         (when (eq major-mode 'rcirc-mode)
                           (setq result (cons buf result)))))
                     result) regexp))

    (defun djr/rcirc-clear-screen ()
      (interactive)
      (goto-char (point-max))
      (set (make-local-variable 'recenter-positions) '(top))
      (recenter-top-bottom)
      (rcirc-clear-unread (current-buffer)))

    (bind-key "C-l" 'djr/rcirc-clear-screen rcirc-mode-map)
    (add-hook 'rcirc-mode-hook 'djr/rcirc-mode-setup)
    (rcirc-track-minor-mode)))

(provide 'djr-rcirc)
