(defvar my-auto-clocking-out-expected-timeout nil
  "The expected, non-suspended timeout")
(defvar my-auto-clocking-out-overtime nil
  "The threshold at which this clock is considered overtime")
(defvar my-auto-clocking-out-window 10
  "How long a suspension needs to go on before we automatically clock out")
(defvar my-auto-clocking-out-timer nil
  "The timer that keeps all auto-clocking out going")

(defun my-auto-clock-message (the-current-time expected-timeout)
  (interactive "nCurrent time:\nnExpected Timeout:")

  (let* ((overtime-difference
	  (- the-current-time expected-timeout))
	 (overtime-difference-time (seconds-to-time overtime-difference))
	 (clockout-string (format-time-string "%d-%m-%y %H:%M:%S" (seconds-to-time expected-timeout)))
	 (overtime-string (format-time-string "%H:%M:%S" overtime-difference-time t)))
    (message "Host OS was suspended, clocked out org-mode at %s, %s ago." clockout-string overtime-string)
    (sit-for 3)))

(defun my-auto-clocking-out-check-overtime ()
  "Does the overtime check and actually clocks out org-mode. This is the part that could be more generic."
  (interactive)

  (when my-auto-clocking-out-expected-timeout
    (if (> (float-time)
	   my-auto-clocking-out-overtime)
	(progn
	  (org-clock-out t (seconds-to-time my-auto-clocking-out-expected-timeout))
	  (my-auto-clock-message (float-time) my-auto-clocking-out-expected-timeout)))))

(defun my-auto-clocking-out ()
  "Entry function to clock out if the host OS hibernates/suspends"
  (interactive)

  (when my-auto-clocking-out-timer
    (cancel-timer my-auto-clocking-out-timer))

  (my-auto-clocking-out-check-overtime)

  (setq my-auto-clocking-out-expected-timeout (+ my-auto-clocking-out-window (float-time))
	my-auto-clocking-out-overtime (+ 5 my-auto-clocking-out-expected-timeout)
	my-auto-clocking-out-timer (run-at-time my-auto-clocking-out-window my-auto-clocking-out-window 'my-auto-clocking-out)))

(defun my-auto-debug ()
  (my-print-seconds "now" (float-time))
  (my-print-seconds "expected" my-auto-clocking-out-expected-timeout)
  (my-print-seconds "overtime" my-auto-clocking-out-overtime))

(defun my-print-seconds (name the-seconds)
  (interactive "sName:\nnSeconds:")
  (let* ((my-time (seconds-to-time the-seconds))
	 (my-string (format-time-string "%H:%M:%S" my-time t)))
    (message "%s: %s is %s" name the-seconds my-string)))

(defun fake-hibernate ()
  (interactive)
  (setq my-auto-clocking-out-expected-timeout (- (float-time) 60)
	my-auto-clocking-out-overtime (- (float-time) 60))
  (my-auto-clocking-out))

(provide 'my-org-timeout)
