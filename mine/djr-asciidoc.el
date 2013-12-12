(djr/ensure-package 'adoc-mode)

(add-to-list 'auto-mode-alist (cons "\\.ad\\'" 'adoc-mode))

(provide 'djr-asciidoc)
