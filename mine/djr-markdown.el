; http://jblevins.org/projects/markdown-mode/

(use-package markdown-mode
  :ensure t
  :commands markdown-mode
  :pin "melpa"
  :mode ("\\.md\\'" . markdown-mode)
  :config

  (setq markdown-command "pandoc -s -f markdown -t html5")

  ;; (use-package markdown-preview-mode :ensure t)
  )


(defun djr/markdown-from (file)
  (interactive "FDocument to use:\n")
  (let ((document 
         (shell-command-to-string (format "pandoc -t markdown --no-wrap %s" file))))
    (newline)
    (insert "#+BEGIN_SRC markdown")
    (newline)
    (insert document)
    (newline)
    (insert "#+END_SRC")
    (newline)))

(provide 'djr-markdown)
