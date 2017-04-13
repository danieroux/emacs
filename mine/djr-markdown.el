; http://jblevins.org/projects/markdown-mode/

(use-package markdown-mode
  :ensure t
  :commands markdown-mode
  :pin "melpa"
  :mode ("\\.\\(md\\|\\|txt\\)$" . markdown-mode)
  :config

  (setq markdown-command "pandoc -s -f markdown -t html")
  (setq markdown-open-command "/usr/local/bin/pandoc -t latex -o /tmp/markdown.pdf")

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
