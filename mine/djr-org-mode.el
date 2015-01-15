;; -*- lexical-binding: t -*-
;; From the rich resource at http://doc.norang.ca/org-mode.html

; (package-install 'org-plus-contrib)

(djr/ensure-package 'org-plus-contrib)

(require 'org)

(require 'org-collector)
(require 'org-checklist)
(require 'org-mobile)

(require 'djr-org-timeout)
(require 'djr-org-drill)

(require 'org-mode-observations)
(setq org-modules (quote (org-habit)))

;; Files
(setq gtd-file "~/Dropbox/Documents/gtd/gtd.org.gpg"
      someday-file "~/Dropbox/Documents/gtd/someday_maybe.org"
      brain-file "~/Dropbox/Documents/brain/brain.org.gpg"
      period-log-file "~/Dropbox/Documents/journal/period.org.gpg")

(setq org-agenda-files `("~/Dropbox/Documents"
			 "~/Dropbox/Documents/gtd"
			 ,brain-file
			 ,gtd-file
			 "~/Dropbox/Documents/consulting/consulting.org.gpg"))

(setq org-directory "~/Dropbox/Documents/gtd")

(setq org-mobile-files (quote ("agendas.org"))
      org-mobile-directory "~/Dropbox/MobileOrg"
      org-mobile-agendas (quote ("O" "P" "e" "c" "b" "o" "n" "h" "A" "w" "E"))
      org-mobile-inbox-for-pull "~/Dropbox/Documents/gtd/inbox.org")

(defun djr/org-mobile-push-agendas-org-only ()
  "Replaces org-mobile-push"
  (interactive)
  (message "Creating agendas.org only")
  (org-mobile-create-sumo-agenda)
  (org-mobile-create-index-file)
  (org-mobile-write-checksums))

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(setq org-default-notes-file "~/Dropbox/Documents/gtd/notes.org")

(org-add-link-type
 "gmail"
 (lambda (link)
   (browse-url (concat "https://mail.google.com/mail/?shva=1#all/" link))))

(setq org-blank-before-new-entry nil
      org-enforce-todo-dependencies t
      org-fast-tag-selection-include-todo t
      org-fast-tag-selection-single-key t
      org-use-fast-todo-selection t
      org-treat-S-cursor-todo-selection-as-state-change nil
      org-hide-leading-stars t
      org-agenda-skip-additional-timestamps-same-entry nil
      org-id-method (quote uuidgen)
      org-id-link-to-org-use-id t)

;; Todo config
(setq org-todo-keywords (quote ((sequence "NEXT(n)" "MAYBE(m)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(z) | NOTE(t)"))))

(setq djr-single-task-header-id "C3478345-CEEF-497D-97EF-32AB278FBCF3")

(setq org-capture-templates `(("P" "New Project" entry (file "~/Dropbox/Documents/gtd/gtd.org.gpg") "* %^{Project name}
** NEXT %^{First task}%?")
			      ("b" "Brain" entry (file "~/Dropbox/Documents/brain/brain.org.gpg") "* %?
  %u

%a")
			      ("l" "liam" entry (file "~/Dropbox/Documents/liam.org") "* %?" :clock-in t :clock-resume t)
			      ("f" "Fieldstone" entry (file "~/Dropbox/Documents/gtd/stones.org") "* %?
  %u

%a")
			      ("i" "inbox" entry (file "~/Dropbox/Documents/gtd/inbox.org") "* NEXT %?
  %u

%a")
			      ("S" "Someday/Maybe" entry (file "~/Dropbox/Documents/gtd/someday_maybe.org") "* NEXT %?
  %u

%a")
			      ("I" "Pay Invoice" entry (id ,djr-single-task-header-id) "* NEXT %a :@banking:
  %u

%a")
			      ("R" "Read paper" entry (id "54B79B33-F158-4200-A317-83DE22D6E6B6") "* NEXT %a :@open:
  %u

%a")
			      ("s" "Single task" entry (id ,djr-single-task-header-id) "* NEXT %?
  %u

%a")
			      ("e" "Follow up email" entry (id ,djr-single-task-header-id) "* NEXT %? %a                     :@online:
  %u
  SCHEDULED: %^t

%a")
			      ("n" "note" entry (file "~/Dropbox/Documents/gtd/inbox.org") "* NOTE %?
	%u
%a")
			      
			      ("d" "daily" entry (file ,period-log-file) "* %U

%?")
			      ("D" "dream" entry (file "~/Dropbox/Documents/journal/dream.org.gpg") "* %U

%?")))

(setq org-stuck-projects
      '("+LEVEL=1+project-persistent/-DONE-CANCELLED" ("NEXT" "STARTED") ()))

(setq org-tag-alist (quote ((:startgroup)
                            ("@errands" . ?e)
                            ("@banking" . ?b)
                            ("@calls" . ?c)
                            ("@home" . ?h)
			    ("@open" . ?O)
			    ("@notebook" . ?a)
			    ("@cellphone" . ?m)
                            ("@online" . ?o)
                            ("@agenda" . ?A)
                            ("@watch" . ?W)
                            (:endgroup)
                            ("crypt" . ?s)
                            ("drill" . ?D)
                            ("project" . ?P))))

(setq org-agenda-tags-todo-honor-ignore-options t
      org-agenda-todo-ignore-scheduled 'future
      org-agenda-skip-function-global nil)

(setq org-agenda-sorting-strategy
      (quote ((agenda
	       habit-down
	       scheduled-up
	       deadline-up)
	      (todo priority-down category-keep)
	      (tags priority-down category-keep)
	      (search category-keep))))

(defun gtd-refile ()
  "Includes email and anything in an inbox"
  `(tags "refile" ((org-agenda-overriding-header "Inbox"))))

(defun gtd-agenda ()
  `(agenda "-MAYBE" ((org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo '("NEXT" "STARTED" "WAITING" "project"))))))

(defun gtd-context (heading query-string)
  `(tags-todo ,(concat query-string "/!-DONE-CANCELLED-WAITING")
	      ((org-agenda-overriding-header ,heading)
	       (org-tags-match-list-sublevels t)
	       (org-agenda-tags-todo-honor-ignore-options t)
	       (org-agenda-todo-ignore-scheduled 'future))))

(defun gtd-project-context (tag &optional if-mode)
  (gtd-context tag (concat "+project+" tag if-mode)))

(defun gtd-agenda-entry (key context-tag)
  `((,key ,context-tag ,@(gtd-project-context context-tag))))

(setq org-agenda-custom-commands `(("H" "@home"
                                    (,(gtd-agenda)
				     ,(gtd-refile)
				     ,(gtd-project-context "@banking" "-consulting")
				     ,(gtd-project-context "@home" "-consulting")
				     ,(gtd-project-context "@open" "-consulting")
				     ,(gtd-project-context "@online" "-consulting")
				     ,(gtd-project-context "@notebook" "-consulting")
				     ,(gtd-project-context "@calls" "-consulting")
				     ,(gtd-project-context "@watch")))
				   ("N" "@notebook"
                                    (,(gtd-agenda)
				     ,(gtd-refile)
				     ,(gtd-project-context "@banking")
				     ,(gtd-project-context "@notebook")
				     ,(gtd-project-context "@calls")
				     ,(gtd-project-context "@open")
				     ,(gtd-project-context "@online")
				     ,(gtd-project-context "@errands")
				     ,(gtd-project-context "@watch")))
                                   ,@(gtd-agenda-entry "n" "@notebook")
                                   ,@(gtd-agenda-entry "e" "@errands")
                                   ,@(gtd-agenda-entry "o" "@online")
                                   ,@(gtd-agenda-entry "O" "@open")
                                   ,@(gtd-agenda-entry "A" "@agenda")
                                   ,@(gtd-agenda-entry "h" "@home")
                                   ,@(gtd-agenda-entry "b" "@banking")
                                   ,@(gtd-agenda-entry "c" "@calls")
                                   ,@(gtd-agenda-entry "W" "@watch")
				   ("w" "Waiting" todo "WAITING" ((org-agenda-overriding-header "Waiting")))
                                   ("r" "refile" tags "refile" nil)
                                   ("p" "projects" tags "+LEVEL=1+project-persistent-@agenda-MAYBE/-CANCELLED-DONE" nil)
                                   ("E" "Todo items without context (in error)" 
                                    ((tags "+project+TODO=\"NEXT\"-{@.*}"))
				    ((org-agenda-overriding-header "context free")))))

;; Client specific agendas and settings
(require 'djr-org-mode-private)

(defun djr/org-mode-refile-current-task-as-single-task ()
  "Refiles the current task as a single task in gtd.org"
  (interactive)
  (let* ((org-refile-targets `((,gtd-file . (:tag . "single"))))
	 ;; Because the tag captures the top level to, grab the second entry
	 (rfloc (nth 1 (org-refile-get-targets))))
    (org-refile nil nil rfloc)))

(setq org-completion-use-ido t
      org-refile-targets `((,(remove brain-file org-agenda-files) :level . 1)
			   (,brain-file . (:level . 0))
			   (,period-log-file . (:level . 0))
			   (,someday-file . (:level . 0))
			   (nil . (:level . 1)))
      org-refile-use-outline-path (quote file)
      org-outline-path-complete-in-steps nil 
      org-refile-allow-creating-parent-nodes nil)

;; Clocking
(require 'bh-org-mode)  

(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 2 :fileskip0 t :compact t :narrow 80)))

(org-clock-persistence-insinuate)
(setq org-clock-into-drawer t)

;; Crypt
(require 'org-crypt)

(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt"))
      org-crypt-key "978D4E9F"
      org-crypt-disable-auto-save t)

(setq org-agenda-include-diary nil)
(setq org-agenda-todo-ignore-scheduled 'future)

(setq org-agenda-span 'day
      org-deadline-warning-days 5)

(run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))

(setq org-habit-following-days 0
      org-habit-show-habits t
      org-habit-preceding-days 30)

(setq org-cycle-separator-lines 0
      org-log-done 'time)

(setq org-src-fontify-natively t)

(setq org-ditaa-jar-path "~/Dropbox/java/ditaa0_90.jar"
      org-plantuml-jar-path "~/Dropbox/java/plantuml.jar")

;(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (ruby . t)
         (gnuplot . t)
         (clojure . t)
         (sh . t)
         (ledger . t)
         (org . t)
         (plantuml . t)
         (latex . t))))

(setq org-confirm-babel-evaluate nil)

; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

;; https://github.com/matburt/mobileorg-android/wiki/FAQ#wiki-How_do_I_get_orgmode_to_execute_orgmobilepush_automatically
(defvar org-mobile-push-timer nil
  "Timer that `org-mobile-push-timer' used to reschedule itself, or nil.")

(defun org-mobile-push-with-delay (secs)
  (when org-mobile-push-timer
    (cancel-timer org-mobile-push-timer))
  (setq org-mobile-push-timer
        (run-with-idle-timer
         (* 60 secs) nil 'djr/org-mobile-push-agendas-org-only)))

(add-hook 'after-save-hook
 (lambda ()
   (when (eq major-mode 'org-mode)
     (dolist (file (org-mobile-files-alist))
       (if (string= (expand-file-name (car file)) (buffer-file-name))
           (org-mobile-push-with-delay 30))))))

(run-at-time "00:05" 86400 '(lambda () (org-mobile-push-with-delay 1))) ;; refreshes agenda file each day

;; I prefer to keep archived entries within the original file
(setq org-archive-default-command (quote org-archive-set-tag))

(djr/auto-clocking-out)

;; Dim blocked tasks
(setq org-agenda-dim-blocked-tasks 'invisible)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

(setq org-agenda-sticky nil)

(setq org-enforce-todo-checkbox-dependencies t
      org-enforce-todo-dependencies t)

(setq org-use-speed-commands t
      org-speed-commands-user (quote (("0" . ignore)
				      ("1" . ignore) ;; For org-drill
				      ("2" . ignore)
				      ("3" . ignore)
				      ("4" . ignore)
				      ("5" . ignore)
				      ("d" . org-decrypt-entry)
				      ("j" . ignore)
				      ("J" . org-clock-goto)
				      ("k" . ignore)
				      ("K" . org-cut-special)
				      ("q" . bh/show-org-agenda)
				      ("S" . widen))))

(setq org-agenda-persistent-filter t)

(setq org-agenda-clock-consistency-checks
      (quote (:max-duration "4:00"
              :min-duration 0
              :max-gap 0
              :gap-ok-around ("4:00"))))

(setq org-agenda-clockreport-parameter-plist
      (quote (:link nil :maxlevel 10 :fileskip0 t :compact t :narrow 150)))

(setq org-icalendar-use-scheduled '(event-if-todo)
      org-icalendar-use-deadline '(event-if-todo)
      org-icalendar-categories '(all-tags todo-state category))

(defun djr/org-mode-ical-home ()
  (interactive)
  (djr/org-mode-ical-export
   '("~/Dropbox/Documents/gtd/gtd.org.gpg")
   "GTD Home"
   "gtd-calendar-home.ics"))

(defun djr/org-mode-ical-export (agenda-files calendar-name ics-name)
  (let* ((org-agenda-files agenda-files)
	 (org-icalendar-combined-name calendar-name)
	 (org-icalendar-combined-agenda-file (expand-file-name ics-name djr/org-icalendar-directory)))
    (org-icalendar-combine-agenda-files)))

;; http://comments.gmane.org/gmane.emacs.orgmode/68791
(defvar bh/insert-inactive-timestamp t)

(defun bh/toggle-insert-inactive-timestamp ()
  (interactive)
  (setq bh/insert-inactive-timestamp (not bh/insert-inactive-timestamp))
  (message "Heading timestamps are %s"
           (if bh/insert-inactive-timestamp "ON" "OFF")))

(defun bh/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun bh/insert-heading-inactive-timestamp ()
  (save-excursion
    (when bh/insert-inactive-timestamp
      (org-return)
      (org-cycle)
      (bh/insert-inactive-timestamp))))

(defun bh/show-org-agenda ()
  (interactive)
  (if org-agenda-sticky
      (switch-to-buffer "*Org Agenda( )*")
    (switch-to-buffer "*Org Agenda*"))
  (delete-other-windows))

(defun djr/agenda-home ()
  (interactive)
  (djr~agenda "H"))

(defun djr/agenda-notebook ()
  (interactive)
  (djr~agenda "N"))

(defun djr~agenda (org-agenda-shortcut)
  (interactive)
  (djr/mu4e-to-org djr-mu4e-combined-inbox-bookmark
		   (lambda ()
		     (org-mobile-pull)
		     (org-agenda nil org-agenda-shortcut)
		     (delete-other-windows))))

(run-with-idle-timer 300 t 'bh/show-org-agenda)

(add-hook 'org-insert-heading-hook
	  'bh/insert-heading-inactive-timestamp 'append)

;; More control on output format
(setq org-export-preserve-breaks t)

(provide 'djr-org-mode)
