(djr/ensure-melpa-package 'scala-mode2)
(require 'scala-mode2)

(add-to-list 'load-path (concat external-dir "/ensime_2.10.0-RC3-0.9.8.2/elisp"))
(require 'ensime)

(djr/prepend-to-paths "/Users/danie/bin/scala-2.10.3/bin")
(djr/prepend-to-paths "/Users/danie/bin/sbt/bin")

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(provide 'djr-scala)
