(require 'eudcb-mab)

; http://news.ycombinator.com/item?id=1995542
(eudc-set-server "dummy" 'mab t)

(eval-after-load
    "message"
  '(define-key message-mode-map [(tab)] 'eudc-expand-inline))

(eval-after-load
    "sendmail"
  '(define-key mail-mode-map [(tab)] 'eudc-expand-inline))

(eudc-protocol-set 'eudc-inline-query-format
		   '((name)
		     (email))
		   'mab)

(eudc-protocol-set 'eudc-inline-expansion-format
		   '("%s <%s>" name email)
		   'mab)

(provide 'my-addressbook)
