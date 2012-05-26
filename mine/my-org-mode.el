;; From the rich resource at http://doc.norang.ca/org-mode.html

(add-to-list 'load-path (concat external-dir "/org-mode/lisp"))
(add-to-list 'load-path (concat external-dir "/org-mode/contrib/lisp"))

(require 'org-install)
(require 'org)

(require 'org-collector)
(require 'org-checklist)
(require 'org-velocity)

(setq org-modules (quote (org-habit)))

;; Files
(setq org-agenda-files (quote ("~/Dropbox/Documents" "~/Dropbox/Documents/gtd")))
(setq org-directory "~/Dropbox/Documents/gtd")

(setq org-mobile-files (quote ("~/Dropbox/Documents/gtd/gtd.org"))
      org-mobile-directory "~/Dropbox/MobileOrg"
      org-mobile-agendas (quote ("e" "c" "n" "H" "E" "w"))
      org-mobile-inbox-for-pull "~/Dropbox/Documents/gtd/inbox.org")

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(setq org-default-notes-file "~/Dropbox/Documents/gtd/notes.org")

(org-add-link-type
 "gmail"
 (lambda (link)
   (browse-url (concat "https://mail.google.com/mail/?shva=1#all/" link))))

(setq org-blank-before-new-entry nil)
(setq org-enforce-todo-dependencies t)
(setq org-fast-tag-selection-include-todo t)
(setq org-fast-tag-selection-single-key t)
(setq org-use-fast-todo-selection t)
(setq org-agenda-skip-additional-timestamps-same-entry nil)

(setq org-hide-leading-stars t)
;; Todo config
(setq org-todo-keywords (quote ((sequence "NEXT(n)" "MAYBE(m)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(z) | NOTE(t)"))))
(setq org-use-fast-todo-selection t)

(setq org-capture-templates (quote (("p" "New Project" entry (file "~/Dropbox/Documents/gtd/gtd.org") "* %^{Project name}
** NEXT %^{First task}%?" :clock-in t :clock-resume t)
                                    ("j" "Journal" entry (file "~/Dropbox/Documents/gtd/journal.org") "* %?" :clock-in t :clock-resume t)
                                    ("l" "liam" entry (file "~/Dropbox/Documents/liam.org") "* %?" :clock-in t :clock-resume t)
                                    ("i" "inbox" entry (file "~/Dropbox/Documents/gtd/inbox.org") "* NEXT %?
	%u %a")
                                    ("n" "note" entry (file "~/Dropbox/Documents/gtd/inbox.org") "* NOTE %?
	%u %a"))))

;; Mirror the inbox above
(setq org-velocity-capture-templates (quote (("v" "From velocity inbox" entry (file "~/Dropbox/Documents/gtd/inbox.org") "* NEXT %:search
	%u %a"))))

(setq org-velocity-bucket "~/Dropbox/Documents/gtd/inbox.org")
;; (setq org-velocity-force-new t)
(setq org-velocity-exit-on-match nil)
;; Agenda
(setq org-stuck-projects
      '("+LEVEL=1+project/-done" ("NEXT" "STARTED") ()))

(setq org-tag-alist (quote ((:startgroup)
                            ("@errands" . ?e)
                            ("@calls" . ?c)
                            ("@home" . ?h)
			    ("@notebook" . ?a)
			    ("@cellphone" . ?p)
                            ("@online" . ?o)
                            (:endgroup)
                            ("crypt" . ?s)
                            ("project" . ?P))))

(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-agenda-todo-ignore-scheduled 'future)

(setq org-agenda-custom-commands (quote
                                  (
                                   ("W" "@work"
                                    ((tags-todo "+project+lautus"
                                                ((org-agenda-overriding-header "Next Task")
                                                 (org-agenda-tags-todo-honor-ignore-options )
                                                 (org-agenda-todo-ignore-scheduled 'future)
                                                 (org-tags-match-list-sublevels t)
                                                 (org-agenda-sorting-strategy
                                                  '(todo-state-down effort-up category-keep))))
                                     (agenda "" nil)
                                     (tags "@calls-TODO=\"DONE\""
                                           ((org-agenda-overriding-header "Calls")
                                            (org-agenda-tags-todo-honor-ignore-options )
                                            (org-agenda-todo-ignore-scheduled 'future)
                                            (org-tags-match-list-sublevels t)))
                                     (tags-todo "@errands-TODO=\"DONE\"" ((org-agenda-overriding-header "Errands")))))
                                   ("H" "@home"
                                    (
				     (tags-todo "+project+@home"
                                                ((org-agenda-overriding-header "Home")
                                                 (org-agenda-tags-todo-honor-ignore-options t)
                                                 (org-agenda-todo-ignore-scheduled 'future)
                                                 (org-tags-match-list-sublevels t)
                                                 (org-agenda-sorting-strategy
                                                  '(todo-state-down effort-up category-keep))))
				     (tags-todo "+project+@online"
                                                ((org-agenda-overriding-header "Online")
                                                 (org-agenda-tags-todo-honor-ignore-options t)
                                                 (org-agenda-todo-ignore-scheduled 'future)
                                                 (org-tags-match-list-sublevels t)
                                                 (org-agenda-sorting-strategy
                                                  '(todo-state-down effort-up category-keep))))
				     (tags-todo "+project+@notebook"
                                                ((org-agenda-overriding-header "Notebook")
                                                 (org-agenda-tags-todo-honor-ignore-options t)
                                                 (org-agenda-todo-ignore-scheduled 'future)
                                                 (org-tags-match-list-sublevels t)
                                                 (org-agenda-sorting-strategy
                                                  '(todo-state-down effort-up category-keep))))
                                     (agenda "" nil)
				     (tags "@calls-TODO=\"DONE\""
					   ((org-agenda-overriding-header "Calls")
					    (org-agenda-tags-todo-honor-ignore-options )
					    (org-agenda-todo-ignore-scheduled 'future)
					    (org-tags-match-list-sublevels t)))
                                     (tags-todo "@errands-TODO=\"DONE\"" ((org-agenda-overriding-header "Errands")))))
                                   ("n" "Next and Started tasks" tags-todo "+project-lautus-WAITING-CANCELLED/!NEXT|STARTED"
                                    ((org-agenda-overriding-header "Next Personal Tasks")
                                     (org-agenda-tags-todo-honor-ignore-options t)
                                     (org-agenda-todo-ignore-scheduled 'future)))
                                   ("e" "Errands" tags-todo "@errands-TODO=\"DONE\"" ((org-agenda-overriding-header "@errands")))
                                   ("c" "Calls" tags "@calls-TODO=\"DONE\""
				    ((org-agenda-overriding-header "@calls")
				     (org-agenda-tags-todo-honor-ignore-options )
				     (org-agenda-todo-ignore-scheduled 'future)))
                                   ("w" "Waiting" tags "TODO=\"WAITING\"" ((org-agenda-overriding-header "@waiting")))
                                   ("r" "Refile" tags "refile" nil)
                                   ("p" "Projects" tags "+LEVEL=1+project" nil)
                                   ("s" "Maybe" tags "+LEVEL=1+MAYBE" nil)
                                   ("E" "Todo items without context (in error)" 
                                    ((tags "+project+TODO=\"NEXT\"-{@.*}"))
				    ((org-agenda-overriding-header "Context free todos"))))))

;; Refile
(setq org-completion-use-ido t)
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 1) (nil :maxlevel . 1))))
(setq org-refile-use-outline-path (quote file))
(setq org-outline-path-complete-in-steps t)
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Clocking
(org-clock-persistence-insinuate)
(setq org-clock-history-length 28)
(setq org-clock-in-resume t)
(setq org-clock-in-switch-to-state (quote life/clock-in-to-started))
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
(setq org-clock-into-drawer t)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-out-when-done t)
(setq org-clock-persist (quote history))
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
(setq org-clock-report-include-clocking-task t)

(defun life/clock-in-to-started (kw)
  (if (member (org-get-todo-state) (list "INBOX" "NEXT"))
      "STARTED"))
  
;; Crypt
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key "978D4E9F")
(setq org-crypt-disable-auto-save 'ask)

(setq org-agenda-include-diary t)
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

(setq org-agenda-span 'day)

(setq org-deadline-warning-days 5)

(run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))
(setq org-cycle-separator-lines 0)
(setq org-log-done 'time)

(setq org-src-fontify-natively t)

; 
; (setq org-ditaa-jar-path "~/java/ditaa0_6b.jar")
(setq org-plantuml-jar-path "~/Dropbox/java/plantuml.jar")
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

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
         (* 1 secs) nil 'org-mobile-push)))

(add-hook 'after-save-hook
 (lambda ()
   (when (eq major-mode 'org-mode)
     (dolist (file (org-mobile-files-alist))
       (if (string= (expand-file-name (car file)) (buffer-file-name))
           (org-mobile-push-with-delay 30))))))

(run-at-time "00:05" 86400 '(lambda () (org-mobile-push-with-delay 1))) ;; refreshes agenda file each day

(provide 'my-org-mode)
