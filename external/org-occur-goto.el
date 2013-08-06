;;; org-occur-goto.el -- search open org buffers with an occur interface

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;; 
;;; 
;;; Usage: M-x oog, then start typing
;;; 
;;; select from the occur matches with up/down/pgup/pgdown and press enter
;;; (you can navigate the history with M-p/M-n) 
;;; 
;;; the search string must be at least 3 characters long (by default)
;;;
;;; Modified by Danie Roux to use its own overlay for results

(require 'cl)

(defvar oog-idle-delay 0.5)

(defvar oog-minimum-input-length 3)

(defvar oog-map 
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "C-j") 'oog-next-line)
    (define-key map (kbd "C-k") 'oog-previous-line)
    (define-key map (kbd "M-k") 'oog-previous-page)
    (define-key map (kbd "M-j") 'oog-next-page)
   map))

(defvar oog-history-list nil)

(defvar oog-highlight-overlay nil)

(defface oog-visible-mark
  '((((min-colors 88) (background dark))
     (:background "green1" :foreground "black"))
    (((background dark))
     (:background "green" :foreground "black"))
    (((background light)) :background "#d1f5ea")
    (((min-colors 88))
     (:background "green1"))
    (t (:background "green")))
  "Face for visible mark."
  :group 'oog)

(defun oog-previous-line ()
  (interactive)
  (oog-move-selection 'forward-line -1))
 
 
(defun oog-next-line ()
  (interactive)
  (oog-move-selection 'forward-line 1))
 
 
(defun oog-previous-page ()
  (interactive)
  (oog-move-selection 'scroll-down nil))
 
 
(defun oog-next-page ()
  (interactive)
  (oog-move-selection 'scroll-up nil))
 
(defun oog-highlight-clear ()
  (if oog-highlight-overlay 
      (delete-overlay oog-highlight-overlay)))

(defun oog-highlight-line ()
  (setq oog-highlight-overlay 
	(make-overlay (point-at-bol) (1+ (point-at-eol))))
  (overlay-put oog-highlight-overlay 'face 'oog-visible-mark))

(defun oog-move-selection (movefunc movearg)
  (let ((win (get-buffer-window "*Occur*")))
    (if win
        (with-selected-window win
          (condition-case nil
	      (progn
		(oog-highlight-clear)
		(funcall movefunc movearg)
		(oog-highlight-line))
            (beginning-of-buffer (goto-char (point-min)))
            (end-of-buffer (goto-char (point-max))))))))


(defun oog-check-input ()
  (when (sit-for oog-idle-delay)
    (unless (equal (minibuffer-contents) oog-current-input)
      (setq oog-current-input (minibuffer-contents))

      (if (< (length oog-current-input) oog-minimum-input-length)
          (let ((win (get-buffer-window "*Occur*")))
            (if win
              (with-selected-window win
                (setq buffer-read-only nil)
                (erase-buffer))))

        (save-excursion
          (flet ((message (&rest args) nil))  ;; suppress occur messages
            (multi-occur
             (remove nil (mapcar (lambda (buffer)
                                   (with-current-buffer buffer
                                     (if (eq major-mode 'org-mode)
                                         buffer)))
                                 (buffer-list)))
             oog-current-input))
          (if (get-buffer "*Occur*")
              ;; put cursor on first matching line for convenience
              (let ((win (get-buffer-window "*Occur*")))
                (if win
                    (with-selected-window win
                      (forward-line))))
            (message "No matches.")))))))



(defun oog ()
  (interactive)
  (let ((cursor-in-non-selected-windows 'box)
        marker)
    (save-window-excursion
      (add-hook 'post-command-hook 'oog-check-input)
      (setq oog-current-input nil)

      (unwind-protect
          (let ((minibuffer-local-map oog-map))
            (read-string "string: " nil 'oog-history-list))
 
        (remove-hook 'post-command-hook 'oog-check-input))

      (let ((buf (get-buffer "*Occur*")))
        (if buf
            (with-current-buffer buf
              (unless (= (buffer-size) 0)
		(hl-line-mode)
                (setq marker (occur-mode-find-occurrence)))))))

    (switch-to-buffer (marker-buffer marker))
    (goto-char marker)
    (when (outline-invisible-p)
      (save-excursion
        (outline-previous-visible-heading 1)
        (org-show-subtree)))))

(provide 'org-occur-goto)
