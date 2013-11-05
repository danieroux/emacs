;; From the rich resource at http://doc.norang.ca/org-mode.html

(djr/ensure-package 'org-plus-contrib)

(require 'org)

(require 'org-collector)
(require 'org-checklist)
(require 'org-mobile)

(require 'djr-org-timeout)
(require 'djr-org-drill)

(setq org-modules (quote (org-habit)))

;; Files
(setq org-agenda-files (quote ("~/Dropbox/Documents"
			       "~/Dropbox/Documents/gtd"
			       "~/Dropbox/Documents/brain/brain.org")))

(setq org-directory "~/Dropbox/Documents/gtd")

(setq org-mobile-files (quote ("agendas.org"))
      org-mobile-directory "~/Dropbox/MobileOrg"
      org-mobile-agendas (quote ("P" "e" "c" "b" "o" "n" "h" "A" "w" "E"))
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

(setq org-capture-templates (quote (("p" "New Project" entry (file "~/Dropbox/Documents/gtd/gtd.org") "* %^{Project name}
** NEXT %^{First task}%?")
                                    ("b" "Brain" entry (file "~/Dropbox/Documents/brain/brain.org") "* %?
  %u")
                                    ("l" "liam" entry (file "~/Dropbox/Documents/liam.org") "* %?" :clock-in t :clock-resume t)
                                    ("i" "inbox" entry (file "~/Dropbox/Documents/gtd/inbox.org") "* NEXT %?
  %u

%a")
                                    ("e" "Follow up email" entry (id "4566445f-82d5-47c7-86c7-dee29ef82112") "* NEXT %?                      :@online:
  %u
  SCHEDULED: %^t

%a")
                                    ("n" "note" entry (file "~/Dropbox/Documents/gtd/inbox.org") "* NOTE %?
	%u
%a"))))

(setq org-stuck-projects
      '("+LEVEL=1+project-persistent/-DONE-CANCELLED" ("NEXT" "STARTED") ()))

(setq org-tag-alist (quote ((:startgroup)
                            ("@errands" . ?e)
                            ("@banking" . ?b)
                            ("@calls" . ?c)
                            ("@home" . ?h)
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
                                    (,(gtd-refile)
				     ,(gtd-agenda)
				     ,(gtd-project-context "@home" "-consulting")
				     ,(gtd-project-context "@banking" "-consulting")
				     ,(gtd-project-context "@online" "-consulting")
				     ,(gtd-project-context "@notebook" "-consulting")
				     ,(gtd-project-context "@calls" "-consulting")
				     ,(gtd-project-context "@watch")))
				   ("N" "@notebook"
                                    (,(gtd-refile)
				     ,(gtd-agenda)
				     ,(gtd-project-context "@online")
				     ,(gtd-project-context "@notebook")
				     ,(gtd-project-context "@errands")
				     ,(gtd-project-context "@calls")
				     ,(gtd-project-context "@banking")))
                                   ,@(gtd-agenda-entry "n" "@notebook")
                                   ,@(gtd-agenda-entry "e" "@errands")
                                   ,@(gtd-agenda-entry "o" "@online")
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

;; Refile
(setq org-completion-use-ido t
      org-refile-targets (quote ((org-agenda-files :maxlevel . 1) (nil :maxlevel . 1)))
      org-refile-use-outline-path (quote file)
      org-outline-path-complete-in-steps nil 
      org-refile-allow-creating-parent-nodes (quote confirm))

;; Clocking
(require 'bh-org-mode)  

(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

(org-clock-persistence-insinuate)
(setq org-clock-into-drawer t)

;; Crypt
(require 'org-crypt)

(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt"))
      org-crypt-key "978D4E9F"
      org-crypt-disable-auto-save t)

(setq org-agenda-include-diary nil)
;; Keep tasks with dates on the global todo lists
;(setq org-agenda-todo-ignore-with-date nil)

;; Keep tasks with deadlines on the global todo lists
;; (setq org-agenda-todo-ignore-deadlines t)

(setq org-agenda-todo-ignore-scheduled 'future)

;; Keep tasks with timestamps on the global todo lists
;(setq org-agenda-todo-ignore-timestamp nil)

;; Remove completed deadline tasks from the agenda view
;(setq org-agenda-skip-deadline-if-done t)

;; Remove completed scheduled tasks from the agenda view
;; (setq org-agenda-skip-scheduled-if-done t)

;; Remove completed items from search results
;(setq org-agenda-skip-timestamp-if-done t)

(setq org-agenda-span 'day
      org-deadline-warning-days 5)

(run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))
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
   '("~/Dropbox/Documents/gtd/gtd.org")
   "GTD Home"
   "gtd-calendar-home.ics"))

(defun djr/org-mode-ical-export (agenda-files calendar-name ics-name)
  (let* ((org-agenda-files agenda-files)
	 (org-icalendar-combined-name calendar-name)
	 (org-icalendar-combined-agenda-file (expand-file-name ics-name djr/org-icalendar-directory)))
    (org-icalendar-combine-agenda-files t)))

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

(add-hook 'org-insert-heading-hook
	  'bh/insert-heading-inactive-timestamp 'append)

(provide 'djr-org-mode)
