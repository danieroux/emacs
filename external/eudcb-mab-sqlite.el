;;; eudcb-mab-sqlite.el --- Emacs Unified Directory Client - AddressBook backend

;; Copyright (C) 2003-2015 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: comm
;; Package: eudc

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;    This library provides an interface to use the Mac's AddressBook,
;;    by querying the database using sqlite3. No additional software
;;    need to be installed.

;;; Code:

(require 'eudc)
(require 'executable)

;;{{{      Internal cooking

(defvar eudc-mab-conversion-alist nil)
(defvar eudc-buffer-time nil)
(defvar eudc-contacts-file
  "~/Library/Application Support/AddressBook/AddressBook-v22.abcddb"
  "This is the location for the 'Local' addressbook on Mavericks. If you have a remote source (like Google), find the right file in ~/Library/Application Support/AddressBook/Sources/")

(eudc-protocol-set 'eudc-query-function 'eudc-mab-query-internal 'mab)
(eudc-protocol-set 'eudc-list-attributes-function nil 'mab)
(eudc-protocol-set 'eudc-mab-conversion-alist nil 'mab)
(eudc-protocol-set 'eudc-protocol-has-default-query-attributes nil 'mab)

(defun sqlite3-dump-mac-addressbook ()
  "LastName:FirstName:Phone:Email"
  (interactive)
  (call-process (executable-find "sqlite3") nil t nil
		"-separator" ":"
		(expand-file-name eudc-contacts-file) 
		"select p.ZLASTNAME,p.ZFIRSTNAME,n.ZFULLNUMBER,e.ZADDRESSNORMALIZED from ZABCDRECORD as p,ZABCDEMAILADDRESS as e,ZABCDPHONENUMBER as n WHERE (e.ZOWNER = p.Z_PK) AND (n.ZOWNER = p.Z_PK);"))

(defun eudc-mab-query-internal (query &optional return-attrs)
  "Query MAB  with QUERY.
QUERY is a list of cons cells (ATTR . VALUE) where ATTRs should be valid
MAB attribute names.
RETURN-ATTRS is a list of attributes to return, defaulting to
`eudc-default-return-attributes'."

  (let ((mab-buffer (get-buffer-create " *mab contacts*"))
	(modified (nth 5 (file-attributes eudc-contacts-file)))
	result)
    (with-current-buffer mab-buffer
      (make-local-variable 'eudc-buffer-time)
      (goto-char (point-min))
      (when (or (eobp) (time-less-p eudc-buffer-time modified))
	(erase-buffer)
	(sqlite3-dump-mac-addressbook)
	(setq eudc-buffer-time modified))
      (goto-char (point-min))
      (while (not (eobp))
	(let* ((args (split-string (buffer-substring (point)
						     (line-end-position))
				   "\\s-*:\\s-*"))
	       (lastname (nth 0 args))
	       (firstname (nth 1 args))
	       (phone (nth 2 args))
	       (mail (nth 3 args))
	       (matched t))

	  (if (string-match "\\s-+\\'" mail)
	      (setq mail (replace-match "" nil nil mail)))

	  (dolist (term query)
	    (cond
	     ((eq (car term) 'name)
	      (unless (string-match (cdr term)
				    (concat firstname " " lastname))
		(setq matched nil)))
	     ((eq (car term) 'email)
	      (unless (string= (cdr term) mail)
		(setq matched nil)))
	     ((eq (car term) 'phone))))

	  (when matched
	    (setq result
		  (cons `((firstname . ,firstname)
			  (lastname . ,lastname)
			  (name . ,(concat firstname " " lastname))
			  (phone . ,phone)
			  (email . ,mail)) result))))
	(forward-line)))
    (if (null return-attrs)
	result
      (let (eudc-result)
	(dolist (entry result)
	  (let (entry-attrs abort)
	    (dolist (attr entry)
	      (when (memq (car attr) return-attrs)
		(if (= (length (cdr attr)) 0)
		    (setq abort t)
		  (setq entry-attrs
			(cons attr entry-attrs)))))
	    (if (and entry-attrs (not abort))
		(setq eudc-result
		      (cons entry-attrs eudc-result)))))
	eudc-result))))

;;}}}

;;{{{      High-level interfaces (interactive functions)

(defun eudc-mab-set-server (dummy)
  "Set the EUDC server to MAB."
  (interactive)
  (eudc-set-server dummy 'mab)
  (message "MAB server selected"))

;;}}}


(eudc-register-protocol 'mab)

(provide 'eudcb-mab-sqlite)
;;; eudcb-mab-sqlite.el ends here
