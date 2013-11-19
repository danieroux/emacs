(require 'midnight)

(midnight-delay-set 'midnight-delay "6:00am")

(defun djr/daily-run ()
  (interactive)
  (djr/pull)
  (djr/org-mode-ical-home)
  (djr/org-mode-ical-consulting))

(add-hook 'midnight-hook 'djr/daily-run)

(provide 'djr-midnight)
