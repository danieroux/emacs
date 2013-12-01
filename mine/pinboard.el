;;; pinboard.el --- Rudimentary http://pinboard.in integration

;; Copyright (C) 2013
;; Danie Roux <danie@danieroux.com>

;; Authors: Danie Roux <danie@danieroux.com>
;; Keywords: pinboard, WWW

;; This file is NOT part of Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This allows adding URLs to http://pinboard.in

;; Suggested integration with w3m:

;; (defun w3m-pinboard-add-current-buffer ()
;;  (pinboard-add-interactively w3m-current-url w3m-current-title))

;;; Code:

(defgroup pinboard nil
  "pinboard.in integration"
  :group 'comm)

(defcustom pinboard-api-token "username:hexstring"
  "Your pinboard.in API token that can be found at: https://pinboard.in/settings/password"
  :group 'pinboard
  :type 'string)

(defun string-set-p (str)
  "Returns true if str is not-nil and not-empty"
  (when str (not (string-equal "" str))))

(defun pinboard-build-add-request (url &optional description tags)
  (when (not (string-set-p url)) (error "For this to be useful, we need a URL"))
  (concat "add?url=" (url-hexify-string url)
	  (when (string-set-p description)
	    (concat "&description=" (url-hexify-string description)))
	  (when (string-set-p tags)
	    (concat "&tags=" (url-hexify-string tags)))))

(defun pinboard-response (buffer)
  "Returns the response string - 'done' if it worked, else the error"
  (unwind-protect
      (with-current-buffer buffer
        (save-excursion
          (goto-char url-http-end-of-headers)
          (let* ((response (car (xml-parse-region (point) (- (re-search-forward "<!--") 5)))))
	    ;; From - (result ((code . "done")))
	    (cdr (assoc 'code (plist-get response 'result))))))))

(defun pinboard-auth-request (request)
  ;; https://pinboard.in/settings/password
  (concat "https://api.pinboard.in/v1/posts/" request "&auth_token=" pinboard-api-token))

(defun pinboard-add (url &optional description tags)
  "Add the url to pinboard.in with optional details - will cause an error if it could not complete"
  (let ((response (pinboard-response
		   (url-retrieve-synchronously
		    (pinboard-auth-request
		     (pinboard-build-add-request url description tags))))))
    (when (not (string-equal "done" response))
      (error "pinboard.in - could not complete adding %s because: %s" url response))))

(defun pinboard-add-interactively (url &optional description tags)
  "Interactively add the url to pinboard.in with optional details - will cause an error if it could not complete"
  (interactive)
  (let ((a-url (read-from-minibuffer "URL to add to Pinboard? " url))
	(a-description (read-from-minibuffer "title? " description))
	(a-tags (read-from-minibuffer "tags? " tags)))
    (pinboard-add a-url a-description a-tags)))

(ert-deftest test-build-request ()
  (should (equal "add?url=http%3A%2F%2Fdanieroux.com&description=Home%20page&tags=personal%2Cblog" (pinboard-build-add-request "http://danieroux.com" "Home page" "personal,blog")))
  (should (equal "add?url=http%3A%2F%2Fdanieroux.com" (pinboard-build-add-request "http://danieroux.com" nil nil)))
  (should (equal "add?url=http%3A%2F%2Fdanieroux.com" (pinboard-build-add-request "http://danieroux.com" "" "")))
  (should (equal "add?url=http%3A%2F%2Fdanieroux.com" (pinboard-build-add-request "http://danieroux.com")))
  (should-error (pinboard-build-add-request nil)))

(provide 'pinboard)
