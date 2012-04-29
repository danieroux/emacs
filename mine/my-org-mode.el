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
(setq org-todo-keywords (quote ((sequence "next(n)" "maybe(m)" "started(s)"  "waiting(w)" "|" "done(d)" "cancelled(z) | note(t)"))))
(setq org-use-fast-todo-selection t)

(setq org-capture-templates (quote (("p" "New Project" entry (file "~/Dropbox/Documents/gtd/gtd.org") "* %^{Project name}
** next %^{First task}%?" :clock-in t :clock-resume t)
                                    ("j" "Journal" entry (file "~/Dropbox/Documents/gtd/journal.org") "* %?" :clock-in t :clock-resume t)
                                    ("l" "liam" entry (file "~/Dropbox/Documents/liam.org") "* %?" :clock-in t :clock-resume t)
                                    ("i" "inbox" entry (file "~/Dropbox/Documents/gtd/inbox.org") "* next %?
	%u %a")
                                    ("n" "note" entry (file "~/Dropbox/Documents/gtd/inbox.org") "* note %?
	%u %a"))))

;; Mirror the inbox above
(setq org-velocity-capture-templates (quote (("v" "From velocity inbox" entry (file "~/Dropbox/Documents/gtd/inbox.org") "* next %:search
	%u %a"))))

(setq org-velocity-bucket "~/Dropbox/Documents/gtd/inbox.org")
;; (setq org-velocity-force-new t)
(setq org-velocity-exit-on-match nil)
;; Agenda
(setq org-stuck-projects
      '("+LEVEL=1+project/-done" ("next" "started") ()))

(setq org-tag-alist (quote ((:startgroup)
                            ("@errands" . ?e)
                            ("@calls" . ?c)
                            ("@home" . ?h)
                            ("@online" . ?o)
                            (:endgroup)
                            ("crypt" . ?s)
                            )))

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
                                     (tags "@calls-TODO=\"done\""
                                           ((org-agenda-overriding-header "Calls")
                                            (org-agenda-tags-todo-honor-ignore-options )
                                            (org-agenda-todo-ignore-scheduled 'future)
                                            (org-tags-match-list-sublevels t)))
                                     (tags-todo "@errands-TODO=\"done\"" ((org-agenda-overriding-header "Errands")))))

                                   ("H" "@home"
                                    ((tags-todo "+project+@home"
                                                ((org-agenda-overriding-header "Next Task")
                                                 (org-agenda-tags-todo-honor-ignore-options t)
                                                 (org-agenda-todo-ignore-scheduled 'future)
                                                 (org-tags-match-list-sublevels t)
                                                 (org-agenda-sorting-strategy
                                                  '(todo-state-down effort-up category-keep))))
                                     (agenda "" nil)
                                     (tags "@calls-TODO=\"done\""
                                           ((org-agenda-overriding-header "Calls")
                                            (org-agenda-tags-todo-honor-ignore-options )
                                            (org-agenda-todo-ignore-scheduled 'future)
                                            (org-tags-match-list-sublevels t)))
                                     (tags-todo "@errands-TODO=\"done\"" ((org-agenda-overriding-header "Errands")))))

                                   ("n" "Next and Started tasks" tags-todo "+project-lautus-waiting-cancelled/!next|started"
                                    ((org-agenda-overriding-header "Next Personal Tasks")
                                     (org-agenda-tags-todo-honor-ignore-options t)
                                     (org-agenda-todo-ignore-scheduled 'future)))
                                   ("N" "Next and Started tasks" tags-todo "+project+lautus-waiting-cancelled/!next|started"
                                    ((org-agenda-overriding-header "Next Lautus Tasks ")
                                     (org-agenda-tags-todo-honor-ignore-options t)
                                     (org-agenda-todo-ignore-scheduled 'future)))
                                   ("e" tags-todo "@errands-TODO=\"done\"" nil)
                                   ("c" tags "@calls-TODO=\"done\"" nil)
                                   ("w" tags "TODO=\"waiting\"" nil)
                                   ("r" tags "refile" nil)
                                   ("p" tags "+LEVEL=1+project" nil)
                                   ("s" tags "+LEVEL=1+maybe" nil)
                                   ("E" "Todo items without context (in error)" 
                                    ((tags "+project+TODO=\"next\"-{@.*}"))))))

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
  (if (member (org-get-todo-state) (list "inbox" "next"))
      "started"))
  
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

(provide 'my-org-mode)
