(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra")
      ispell-list-command "list")

(setq flyspell-issue-message-flag nil)

(dolist (hook '(markdown-mode-hook
		mu4e-compose-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; Disable for these modes
; (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
;  (add-hook hook (lambda () (flyspell-mode -1))))

(provide 'djr-spell)
