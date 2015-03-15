;; Start of some useful IntelliJ tricks I miss

;; Also, from evil.el:

;; Evil requires `goto-last-change' and `goto-last-change-reverse'
;; function for the corresponding motions g; g, as well as the
;; last-change-register `.'. One package providing these functions is
;; goto-chg.el:
(use-package goto-chg
  :pin "melpa"
  :commands goto-last-change
  :ensure t
  :bind* ("<C-S-backspace>" . goto-last-change))

;; Save on tab-out
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

(provide 'djr-intellij)
