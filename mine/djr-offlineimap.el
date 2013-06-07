(add-to-list 'load-path (concat external-dir "/offlineimap"))

(require 'offlineimap)

(setq offlineimap-command "/usr/pkg/bin/offlineimap -u MachineUI"
      offlineimap-mode-line-style 'symbol
      offlineimap-mode-line-symbol "O"
      offlineimap-enable-mode-line-p '(member
				       major-mode
				       '(offlineimap-mode
					 mu4e-main-mode)))

(provide 'djr-offlineimap)

