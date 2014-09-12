(defun djr/org-timer-start-at (offset-string)
  "Starts the timer at a specific time"
  (interactive "sPlease enter the start time for the relative timer(i.e. 9:30): ")
  (org-timer-start (concat offset-string ":00")))

(defun djr/org-timer-warp-minutes (delta-minutes)
  "Warps the current relative timer by the given amount of minutes. Assumes timer is running."
  (interactive "nHow many minutes do you want to warp? ")
  (setq org-timer-start-time
	(seconds-to-time
	 (- (org-float-time org-timer-start-time)
	    (* 60 delta-minutes)))))

(defun djr/org-timer-warp-one ()
  (interactive)
  (djr/org-timer-warp-minutes 1))

(defun djr/org-timer-warp-minus-one ()
  (interactive)
  (djr/org-timer-warp-minutes -1))

(defun djr/org-timer-set-minute (new-minute)
  "Observations jumps by minutes, so specifically specifying '22' to jump to the current hours 22 minute mark is easier"
  (interactive "sWhich minute in the hour do you want to jump to? ")
  (let* ((hour-by-crook (/ (floor (org-timer-seconds)) (* 60 60))) ;; Because timezones, org-timer-seconds and decode-time is an unhappy place
	 (new-time-string (format "%s:%s:00" hour-by-crook new-minute))) 
    (org-timer-start new-time-string)))

(define-minor-mode org-mode-observations
  "Toggle Org Mode Observations Mode

Convenient minor mode that allows you to control the relative timer with ease.
This is used to transcribe timestamped observations quickly.

M-/ Sets the minute in the hour to a prompted time
M-= Increments the relative time by one minute
M-- Decrements the relative time by one minute

by Danie Roux <danie@danieroux.com>
   "
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " Observations"
  ;; The minor mode bindings
  :keymap
  `((,(kbd "M-,") . djr/org-timer-warp-minus-one)
    (,(kbd "M-.") . djr/org-timer-warp-one)
    (,(kbd "M-/") . djr/org-timer-set-minute))
  :group 'org-observations)

(provide 'org-mode-observations)
