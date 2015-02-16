; http://jblevins.org/projects/markdown-mode/

(use-package markdown-mode
  :ensure t
  :pin melpa
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (progn
    ;; Use Pandoc to process Markdown
    (setq markdown-command "pandoc -s -f markdown -t html5")))

(provide 'djr-markdown)
