(defvar djr/auto-clocking-out-expected-timeout nil
  "The expected, non-suspended timeout")
(defvar djr/auto-clocking-out-overtime nil
  "The threshold at which this clock is considered overtime")
(defvar djr/auto-clocking-out-window 10
  "How long a suspension needs to go on before we automatically clock out")
(defvar djr/auto-clocking-out-timer nil
  "The timer that keeps all auto-clocking out going")

(defun djr/auto-clock-message (the-current-time expected-timeout)
  (interactive "nCurrent time:\nnExpected Timeout:")

  (let* ((overtime-difference
	  (- the-current-time expected-timeout))
	 (overtime-difference-time (seconds-to-time overtime-difference))
	 (clockout-string (format-time-string "%d-%m-%y %H:%M:%S" (seconds-to-time expected-timeout)))
	 (overtime-string (format-time-string "%H:%M:%S" overtime-difference-time t)))
    (message "Host OS was suspended, clocked out org-mode at %s, %s ago." clockout-string overtime-string)
    (sit-for 3)))

(defun djr/auto-clocking-out-check-overtime ()
  "Does the overtime check and actually clocks out org-mode. This is the part that could be more generic."
  (interactive)

  (when djr/auto-clocking-out-expected-timeout
    (if (> (float-time)
	   djr/auto-clocking-out-overtime)
	(progn
	  (org-clock-out t (seconds-to-time djr/auto-clocking-out-expected-timeout))
	  (djr/auto-clock-message (float-time) djr/auto-clocking-out-expected-timeout)))))

(defun djr/auto-clocking-out ()
  "Entry function to clock out if the host OS hibernates/suspends"
  (interactive)

  (when djr/auto-clocking-out-timer
    (cancel-timer djr/auto-clocking-out-timer))

  (djr/auto-clocking-out-check-overtime)

  (setq djr/auto-clocking-out-expected-timeout (+ djr/auto-clocking-out-window (float-time))
	djr/auto-clocking-out-overtime (+ 5 djr/auto-clocking-out-expected-timeout)
	djr/auto-clocking-out-timer (run-at-time djr/auto-clocking-out-window djr/auto-clocking-out-window 'djr/auto-clocking-out)))

(defun djr/auto-debug ()
  (djr/print-seconds "now" (float-time))
  (djr/print-seconds "expected" djr/auto-clocking-out-expected-timeout)
  (djr/print-seconds "overtime" djr/auto-clocking-out-overtime))

(defun djr/print-seconds (name the-seconds)
  (interactive "sName:\nnSeconds:")
  (let* ((time (seconds-to-time the-seconds))
	 (string (format-time-string "%H:%M:%S" time t)))
    (message "%s: %s is %s" name the-seconds string)))

(defun fake-hibernate ()
  (interactive)
  (setq djr/auto-clocking-out-expected-timeout (- (float-time) 60)
	djr/auto-clocking-out-overtime (- (float-time) 60))
  (djr/auto-clocking-out))

(provide 'djr-org-timeout)
