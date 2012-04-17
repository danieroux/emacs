;;; google-contacts-message.el --- Support for Google Contacts in message-mode

;; Copyright © 2011 Julien Danjou <julien@danjou.info>

;; Author: Julien Danjou <julien@danjou.info>
;; Keywords: comm

;; This file is NOT part of GNU Emacs.

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
;;
;; This allows you to press <TAB> in message-mode to get email address
;; completion.
;;

;;; Code:

(eval-and-compile
  (require 'google-contacts))

(defun google-contacts-message-complete-function ()
  "Function used in `completion-at-point-functions' in `message-mode'."
  (let ((mail-abbrev-mode-regexp
         "^\\(Resent-To\\|To\\|B?Cc\\|Reply-To\\|From\\|Mail-Followup-To\\|Mail-Copies-To\\|Disposition-Notification-To\\|Return-Receipt-To\\):"))
    (when (mail-abbrev-in-expansion-header-p)
      (google-contacts-complete-name))))

(defun google-contacts-complete-name ()
  "Complete text at START with a user name and email."
  (let ((end (point))
        (start (save-excursion
                 (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
                 (goto-char (match-end 0))
                 (point)))
        (contacts (xml-get-children (google-contacts-data) 'entry))
        choices)
    (dolist (contact contacts)
      (let* ((name-value (nth 0 (xml-get-children contact 'gd:name)))
             (fullname (xml-node-child-string (nth 0 (xml-get-children name-value 'gd:fullName))))
             (emails (google-contacts-build-node-list contact 'gd:email
                                                      (xml-get-attribute child 'address))))
        (dolist (email emails)
          (add-to-list 'choices
                       (if (string= fullname "")
                           (cdr email)
                         (concat fullname " <" (cdr email) ">"))))))
    (list start end choices)))

(add-hook 'message-mode-hook
          (lambda ()
            (add-to-list 'completion-at-point-functions
                         'google-contacts-message-complete-function)))

(provide 'google-contacts-message)
