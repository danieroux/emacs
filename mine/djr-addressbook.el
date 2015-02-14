(require 'eudcb-mab-sqlite)

; http://news.ycombinator.com/item?id=1995542
(eudc-set-server "dummy" 'mab t)

(setq eudc-contacts-file "~/Library/Application Support/AddressBook/Sources/12282D27-D3D9-47F5-AAE5-DC45C61D8FCF/AddressBook-v22.abcddb") 

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

(provide 'djr-addressbook)
