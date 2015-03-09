(use-package org-drill
  :commands org-drill)

(setq org-drill-use-visible-cloze-face-p t
      org-drill-maximum-items-per-session 100
      org-drill-maximum-duration 30
      org-drill-scope 'agenda
      org-drill-add-random-noise-to-intervals-p t
      org-drill-adjust-intervals-for-early-and-late-repetitions-p t)

(provide 'djr-org-drill)
