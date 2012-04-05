(require 'auto-complete-config)
(ac-config-default)

;; http://www.masteringemacs.org/articles/2010/11/29/evaluating-elisp-emacs
(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers)))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)

(add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
(add-to-list 'ac-modes 'emacs-lisp-mode)
(auto-complete-mode 1)

(provide 'my-autocomplete)
