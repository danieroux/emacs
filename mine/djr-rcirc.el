(use-package s)
(use-package dash)
(use-package ivy)

(use-package rcirc
  :defer t
  :commands rcirc

  :init
  ;; rcirc-server-alist, rcirc-authinfo and rcirc-default-nick are defined in private.el.gpg
  (setq rcirc-prompt "%n %t> "
        rcirc-scroll-show-maximum-output nil
        rcirc-omit-responses '("AWAY" "MODE")
        rcirc-omit-threshold 0
        rcirc-fill-flag nil
        rcirc-debug-flag t
        rcirc-log-flag t
        rcirc-trap-errors-flag nil
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

    (use-package rcirc-color :ensure t)
    (use-package djr-emoji)

    ;; (use-package rcirc-groups :ensure t)

    (set-face-foreground 'rcirc-dim-nick "grey" nil)

    (defun djr/general-to-less-general-slack-channel-name ()
      (interactive)
      (rename-buffer
       (djr/general-to-less-general-slack-channel-name-1 (buffer-name))))

    (defun djr//rcirc-is-server-buffer-p (buf)
      (with-current-buffer buf rcirc-buffer-alist))

    (defun djr//rcirc-chat-buffers ()
      (-filter
       (lambda (buf)
         (and
          (eq 'rcirc-mode (with-current-buffer buf major-mode))
          (not (djr//rcirc-is-server-buffer-p buf))))
       (buffer-list)))

    ;; Adapted from rcirc.el
    (defun djr//rcirc-chat-buffers-sorted-by-last-activity ()
      (sort (djr//rcirc-chat-buffers)
            (lambda (b1 b2)
              (let ((t1 (with-current-buffer b1 rcirc-last-post-time))
                    (t2 (with-current-buffer b2 rcirc-last-post-time)))
                (time-less-p t2 t1)))))

    (defun djr/ivy-rcirc-buffers ()
      (interactive)
      (ivy-read
       "IRC buffers: "
       (mapcar (lambda (buffer)
                 (cons (buffer-name buffer) buffer))
               (djr//rcirc-chat-buffers-sorted-by-last-activity))
       :action (lambda (buffer) (switch-to-buffer buffer))))

    (defun djr/general-to-less-general-slack-channel-name-1 (a-buffer-name)
      "Turns #general@znc-slackserver into #slackserver-g@znc-slackserver, or leaves buffer-name the same"
      (if (s-starts-with? "#general" a-buffer-name)
          (let* ((server (second (s-split "@znc-" a-buffer-name))))
            (format "#%s-g@znc-%s" server server))
        a-buffer-name))

    (defun djr/rcirc-mode-setup ()
      (interactive)
      (setq rcirc-omit-mode nil)
      (rcirc-omit-mode)
      (emojify-mode)
      (flyspell-mode 1)
      (djr/general-to-less-general-slack-channel-name)
      (set (make-local-variable 'scroll-conservatively) 8192))

    ;; Override
    (defun rcirc-short-buffer-name (buffer)
      "Override to ignore rcirc-short-buffer-name var - and to always use the real buffer-name"
      (with-current-buffer buffer
        (buffer-name)))

    ;; Swallow KEEPALIVE messages with a sledgehammer.
    ;; (defun rcirc-handler-NOTICE (process sender args text))

    ;; Slack does not set a topic, and breaks this handler. So stop handling topics as a quickfix.
    ;; (defun rcirc-handler-333 (process sender args _text))

    (defun-rcirc-command slack (arg)
      "Open slack in the browser for this IRC integration"
      (let* ((connected-host (process-name process))
             ;; I have my slack servers defined as znc-slackserver
             (slack-name (second (split-string connected-host "-")))
             (slack-server (format "https://%s.slack.com/messages/%s" slack-name target))
             (open-message (format "Opening %s" slack-server)))
        (rcirc-print process nil "SLACK" target open-message)
        (browse-url slack-server)))

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

    ;; http://endlessparentheses.com/marking-emacs-chat-buffers-as-read.html
    (defun endless/mark-read ()
      "Mark buffer as read up to current line."
      (let ((inhibit-read-only t))
        (put-text-property
         (point-min) (line-beginning-position)
         'face       'font-lock-comment-face)))

    (defun endless/bury-buffer ()
      "Bury buffer and maybe close its window."
      (interactive)
      (endless/mark-read)
      (bury-buffer)
      (when (cdr (window-list nil 'nomini))
        (delete-window)))

    (defun djr/rcirc-clear-screen-and-next-activity ()
      (interactive)
      (when (equal major-mode 'rcirc-mode)
        (djr/rcirc-clear-screen))
      (call-interactively 'rcirc-next-active-buffer))

    (defun djr/rcirc-clear-screen ()
      (interactive)
      (rcirc-clear-unread (current-buffer))
      (endless/bury-buffer))

    (defun djr/rcirc-fixup-modestring ()
      (interactive)
      (setq global-mode-string '(""))
      (setq global-mode-string (append global-mode-string '(rcirc-activity-string))))

    (bind-key "C-c C-@" 'djr/rcirc-clear-screen-and-next-activity rcirc-track-minor-mode-map)
    (bind-key "C-c C-SPC" 'djr/rcirc-clear-screen-and-next-activity rcirc-track-minor-mode-map)
    (bind-key "C-c SPC" 'djr/ivy-rcirc-buffers)

    (add-hook 'rcirc-mode-hook 'djr/rcirc-mode-setup)

    (rcirc-track-minor-mode)))

(provide 'djr-rcirc)
