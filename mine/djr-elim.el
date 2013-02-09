(add-to-list 'load-path (concat external-dir "/elim/elisp"))

(autoload 'garak "garak" nil t)

(garak-alert-when (quote (:new :hidden :chat)))
(garak-buddy-list-sort-type (quote garak-buddy-sort-by-label-predicate))
(garak-display-splash nil)
(garak-hide-offline-buddies t)

(provide 'djr-elim)
