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
(setq org-agenda-files (quote ("~/Dropbox/Documents" "~/Dropbox/Documents/gtd")))
(setq org-directory "~/Dropbox/Documents/gtd")

(setq org-mobile-files (quote ("~/Dropbox/Documents/gtd/gtd.org" "~/Dropbox/Documents/gtd/consulting.org" "~/Dropbox/Documents/gtd/agenda.org"))
      org-mobile-directory "~/Dropbox/MobileOrg"
      org-mobile-agendas (quote ("P" "e" "c" "b" "o" "n" "h" "A" "w" "E"))
      org-mobile-inbox-for-pull "~/Dropbox/Documents/gtd/inbox.org")

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
      org-hide-leading-stars t
      org-agenda-skip-additional-timestamps-same-entry nil)

;; Todo config
(setq org-todo-keywords (quote ((sequence "NEXT(n)" "MAYBE(m)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(z) | NOTE(t)"))))

(setq org-capture-templates (quote (("p" "New Project" entry (file "~/Dropbox/Documents/gtd/gtd.org") "* %^{Project name}
** NEXT %^{First task}%?")
                                    ("j" "Journal" entry (file "~/Dropbox/Documents/gtd/journal.org") "* %?" :clock-in t :clock-resume t)
                                    ("l" "liam" entry (file "~/Dropbox/Documents/liam.org") "* %?" :clock-in t :clock-resume t)
                                    ("i" "inbox" entry (file "~/Dropbox/Documents/gtd/inbox.org") "* NEXT %?
	%u %a")
                                    ("e" "Follow up email" entry (id "4566445f-82d5-47c7-86c7-dee29ef82112") "* NEXT %?                      :@online:
  SCHEDULED: %^t

  %a")
                                    ("n" "note" entry (file "~/Dropbox/Documents/gtd/inbox.org") "* NOTE %?
	%u %a"))))

(setq org-stuck-projects
      '("+LEVEL=1+project/-DONE-CANCELLED" ("NEXT" "STARTED") ()))

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

(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-agenda-todo-ignore-scheduled 'future)

(setq org-agenda-skip-function-global nil)

(setq org-agenda-custom-commands (quote
                                  (
                                   ("H" "@home"
                                    ((tags "refile"
                                                ((org-agenda-overriding-header "Inbox")))
				     (agenda "-MAYBE" ((org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo '("NEXT" "STARTED" "WAITING" "project")))))
				     (tags-todo "+project+@home/!-WAITING"
                                                ((org-agenda-overriding-header "Home")
                                                 (org-agenda-tags-todo-honor-ignore-options t)
                                                 (org-agenda-todo-ignore-scheduled 'future)
                                                 (org-tags-match-list-sublevels t)
                                                 (org-agenda-sorting-strategy
                                                  '(todo-state-down effort-up category-keep))))
				     (tags-todo "+project+@banking/!-WAITING"
                                                ((org-agenda-overriding-header "Banking")
                                                 (org-agenda-tags-todo-honor-ignore-options t)
                                                 (org-agenda-todo-ignore-scheduled 'future)
                                                 (org-tags-match-list-sublevels t)
                                                 (org-agenda-sorting-strategy
                                                  '(todo-state-down effort-up category-keep))))
				     (tags-todo "+project+@online/!-WAITING"
                                                ((org-agenda-overriding-header "Online")
                                                 (org-agenda-tags-todo-honor-ignore-options t)
                                                 (org-agenda-todo-ignore-scheduled 'future)
                                                 (org-tags-match-list-sublevels t)
                                                 (org-agenda-sorting-strategy
                                                  '(todo-state-down effort-up category-keep))))
				     (tags-todo "+project+@notebook/!-WAITING"
                                                ((org-agenda-overriding-header "@notebook")
                                                 (org-agenda-tags-todo-honor-ignore-options t)
                                                 (org-agenda-todo-ignore-scheduled 'future)
                                                 (org-tags-match-list-sublevels t)
                                                 (org-agenda-sorting-strategy
                                                  '(todo-state-down effort-up category-keep))))
				     (tags-todo "@calls/!-CANCELLED-WAITING"
					   ((org-agenda-overriding-header "Calls")
					    (org-agenda-tags-todo-honor-ignore-options t)
					    (org-agenda-todo-ignore-scheduled 'future)
					    (org-tags-match-list-sublevels t)))
				     (tags-todo "@watch/!-CANCELLED-WAITING"
					   ((org-agenda-overriding-header "Watch")
					    (org-agenda-tags-todo-honor-ignore-options t)
					    (org-agenda-todo-ignore-scheduled 'future)
					    (org-tags-match-list-sublevels t)))
				     )
				    )
                                   ("n" "Notebook tasks" tags-todo "+project+@notebook-WAITING-CANCELLED/!NEXT|STARTED"
                                    ((org-agenda-overriding-header "@notebook")
                                     (org-agenda-tags-todo-honor-ignore-options t)
                                     (org-agenda-todo-ignore-scheduled 'future)))
                                   ("e" "Errands" tags-todo "@errands-TODO=\"DONE\"" ((org-agenda-overriding-header "@errands")))
                                   ("o" "Online" tags-todo "@online-MAYBE/!-CANCELLED-WAITING" ((org-agenda-overriding-header "@online")))
                                   ("A" "Agenda items" tags-todo "@agenda-TODO=\"DONE\"" ((org-agenda-overriding-header "@agenda")))
                                   ("h" "@home" tags-todo "+project+@home"
				    ((org-agenda-overriding-header "@home")
				     (org-agenda-tags-todo-honor-ignore-options 't)
				     (org-agenda-todo-ignore-scheduled 'future)))
                                   ("P" "Phone" tags-todo "@cellphone-DONE-CANCELLED"
				    ((org-agenda-overriding-header "@cellphone")
				     (org-agenda-tags-todo-honor-ignore-options 't)
				     (org-agenda-todo-ignore-scheduled 'future)))
                                   ("b" "Banking" tags-todo "@banking-DONE-CANCELLED"
				    ((org-agenda-overriding-header "@banking")
				     (org-agenda-tags-todo-honor-ignore-options 't)
				     (org-agenda-todo-ignore-scheduled 'future)))
                                   ("c" "Calls" tags-todo "@calls/!-DONE-CANCELLED-WAITING"
				    ((org-agenda-overriding-header "@calls")
				     (org-agenda-tags-todo-honor-ignore-options 't)
				     (org-agenda-todo-ignore-scheduled 'future)))
                                   ("w" "Waiting" tags "TODO=\"WAITING\"" ((org-agenda-overriding-header "@waiting")))
                                   ("r" "Refile" tags "refile" nil)
                                   ("p" "Projects" tags "+LEVEL=1+project-persistent-MAYBE/-CANCELLED-DONE" nil)
                                   ("s" "Maybe" tags "+LEVEL=1+MAYBE" nil)
                                   ("E" "Todo items without context (in error)" 
                                    ((tags "+project+TODO=\"NEXT\"-{@.*}"))
				    ((org-agenda-overriding-header "context free"))))))

;; Refile
(setq org-completion-use-ido t
      org-refile-targets (quote ((org-agenda-files :maxlevel . 1) (nil :maxlevel . 1)))
      org-refile-use-outline-path (quote file)
      org-outline-path-complete-in-steps t
      org-refile-allow-creating-parent-nodes (quote confirm))

;; Clocking
(require 'bh-org-mode)  

(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

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

(setq org-agenda-span 'day)

(setq org-deadline-warning-days 5)

(run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))
(setq org-cycle-separator-lines 0)
(setq org-log-done 'time)

(setq org-src-fontify-natively t)

(setq org-ditaa-jar-path "~/Dropbox/java/ditaa0_90.jar")
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
         (* 60 secs) nil 'org-mobile-push)))

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

(setq org-enforce-todo-checkbox-dependencies t)
(setq org-enforce-todo-dependencies t)

(setq org-use-speed-commands t
      org-speed-commands-user (quote (("0" . ignore)
				     ("w" . org-refile)
				     )))

(setq org-agenda-persistent-filter t)

(setq org-agenda-clock-consistency-checks
      (quote (:max-duration "4:00"
              :min-duration 0
              :max-gap 0
              :gap-ok-around ("4:00"))))

(setq org-agenda-clockreport-parameter-plist
      (quote (:link nil :maxlevel 10 :fileskip0 t :compact t :narrow 150)))
  
(provide 'djr-org-mode)
