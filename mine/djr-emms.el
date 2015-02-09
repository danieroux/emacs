(use-package emms-setup
  :load-path "external/emms/lisp"
  :config
  (progn
    (emms-all)
    (emms-default-players)
    (define-emms-simple-player afplay '(file)
      (regexp-opt '(".mp3" ".m4a" ".flv" ".aac"))
      "afplay")
    (setq emms-player-list `(,emms-player-afplay))
    (setq emms-source-file-default-directory "~/Music")))

(provide 'djr-emms)
