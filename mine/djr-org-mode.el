;; -*- lexical-binding: t -*-
;; From the rich resource at http://doc.norang.ca/org-mode.html

(use-package org
  :commands org-mode
  :pin "manual"
  :demand t
  :load-path ("external/org-mode/lisp" "external/org-mode/contrib/lisp")
  :mode ("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode)

  :init

  (setq org-modules (quote (org-habit)))

  :config
  (progn
    (when (equal system-type 'darwin)
      (use-package org-mac-link
        :load-path ("/Users/danie/dotfiles/emacs.d/external/org-mode/contrib/lisp"))

      (autoload 'omlg-grab-link "org-mac-link"))

    (org-clock-persistence-insinuate)

    ;; Crypt
    (use-package org-crypt)
    (org-crypt-use-before-save-magic)

    (add-hook 'org-agenda-mode-hook
              (lambda ()
                (define-key org-agenda-mode-map "q" 'bury-buffer)
                (define-key org-agenda-mode-map "P" 'org-pomodoro))
              'append)

    (org-add-link-type
     "gmail"
     (lambda (link)
       (browse-url (concat "https://mail.google.com/mail/?shva=1#all/" link))))

    (use-package org-mobile)

    (use-package djr-org-drill)
    ;(use-package djr-org-mu4e)

    (use-package org-pomodoro
      :pin "melpa"
      :ensure t
      :commands org-pomodoro
      :init
      (progn
        (setq org-pomodoro-keep-killed-pomodoro-time t
              org-pomodoro-sound (expand-file-name "~/Dropbox/Audio/wav/13699__harri__a.mp3")
              org-pomodoro-short-break-sound (expand-file-name "~/Dropbox/Audio/wav/13699__harri__a.mp3")
              org-pomodoro-long-break-sound (expand-file-name "~/Dropbox/Audio/wav/13699__harri__a.mp3")
              org-pomodoro-start-sound (expand-file-name "~/Dropbox/Audio/wav/big-singing-bowl.wav")
              org-pomodoro-killed-sound (expand-file-name "~/Dropbox/Audio/wav/jf-glass-breaking.wav")
              org-pomodoro-start-sound-p t
              org-pomodoro-time-format "%.2m")))

    (use-package org-mode-observations
      :commands org-mode-observations)

    ;; Files
    (setq gtd-file "~/Dropbox/Documents/gtd/gtd.org.gpg"
          consulting-file "~/Dropbox/Documents/consulting/consulting.org.gpg"
          inbox-file "~/Dropbox/Documents/gtd/inbox.org"
          someday-file "~/Dropbox/Documents/gtd/someday_maybe.org.gpg"
          brain-file "~/Dropbox/Documents/brain/brain.org.gpg"
          conversations-file "~/Dropbox/Documents/gtd/conversations.org"
          period-log-file "~/Dropbox/Documents/journal/period.org.gpg"
          daily-log-file "~/Dropbox/Documents/journal/daily.org.gpg"
          matter-log-file "~/Dropbox/Documents/matter/matter-log.org.gpg"
          blog-ideas-file "~/Dropbox/Documents/gtd/blog_ideas.org.gpg")

    (setq org-agenda-files `(,gtd-file ,consulting-file))

    (setq org-directory "~/Dropbox/Documents/gtd")

    (setq org-mobile-files (quote ("agendas.org" "~/Dropbox/Documents/gtd/conversations.org"))
          org-mobile-directory "~/Dropbox/MobileOrg"
          org-mobile-agendas (quote ("M"))
          org-mobile-inbox-for-pull inbox-file)

    (defun djr/org-mobile-push-agendas-org-only ()
      "Replaces org-mobile-push. My files are .gpg encrypted, and this causes issues with MobileOrg"
      (interactive)
      (org-mobile-create-sumo-agenda)
      (org-mobile-copy-agenda-files)
      (org-mobile-create-index-file)
      (org-mobile-write-checksums))

    (setq org-default-notes-file "~/Dropbox/Documents/gtd/notes.org")

    (setq org-blank-before-new-entry nil
          org-enforce-todo-dependencies t
          org-fast-tag-selection-include-todo t
          org-fast-tag-selection-single-key t
          org-use-fast-todo-selection t
          org-treat-S-cursor-todo-selection-as-state-change nil
          org-hide-leading-stars t
          org-agenda-skip-additional-timestamps-same-entry nil
          org-id-method (quote uuidgen)
                                        ; Make clocktable indents pretty-ish
          org-pretty-entities t
          org-id-link-to-org-use-id t)

    ;; Todo config
    (setq org-todo-keywords (quote ((sequence "NEXT(n)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(z) | NOTE(t)"))))

    (setq djr-single-task-header-id "ADB306DE-7FDA-4F6F-826A-569E3D6CD2F3")

    (setq org-capture-templates `(("P" "New Project" entry (file ,gtd-file) "* %^{Project name}
** NEXT %^{First task}%?")
                                  ("b" "Brain" entry (file ,brain-file) "* %?
  %u

%a")
                                  ("l" "liam" entry (file "~/Dropbox/Documents/liam.org") "* %?" :clock-in t :clock-resume t)
                                  ("f" "Fieldstone" entry (file "~/Dropbox/Documents/gtd/stones.org") "* %?
  %u

%a")
                                  ("i" "inbox" entry (file ,inbox-file) "* NEXT %?
  %u

%a")
                                  ("S" "Someday/Maybe" entry (file ,someday-file) "* NEXT %?
  %u

%a")
                                  ("I" "Pay Invoice" entry (id ,djr-single-task-header-id) "* NEXT %a :@banking:
  %u

%a")
                                  ("R" "Read paper" entry (id "54B79B33-F158-4200-A317-83DE22D6E6B6") "* NEXT %a :@open:
  %u

%a")
                                  ("s" "Single task" entry (id ,djr-single-task-header-id) "* NEXT %? %^g
  %u

%a")
                                  ("e" "Follow up email" entry (id ,djr-single-task-header-id) "* NEXT %? %a                     :@online:
  %u
  SCHEDULED: %^t

%a")
                                  ("n" "note" entry (file ,inbox-file) "* NOTE %?
  %u
%a")

                                  ("p" "period" entry (file ,period-log-file) "* %U

%?")
                                  ("d" "daily" entry (file ,daily-log-file) "* %U

- %?")
                                  ("m" "Matter Log Entry" entry (file ,matter-log-file) "* %U

- %?")
                                  ;; https://www.farnamstreetblog.com/2014/02/decision-journal
                                  ("D" "Decision" entry (file "~/Dropbox/Documents/journal/decisions.org.gpg") "* %U %?
** Situation or context
** Problem statement / frame
** Variables that govern the situation
** Complications or complexity as I see it
** Alternatives that were seriously considered and why they were not chosen
** A paragraph explaining the range of outcomes
** A paragraph explaining what you expect to happen and the reasoning and actual probabilities you assign to each projected outcome
** The time of day you're making the decision and how you feel physically and mentally
%?")
                                  ("c" "The current Chrome tab" entry (file ,inbox-file) "* NEXT %? %(org-mac-chrome-get-frontmost-url)  :@online:
  %u

%a")))

    (setq org-stuck-projects
          '("+LEVEL=1+project-persistent/-DONE-CANCELLED" ("NEXT" "STARTED") ()))

    (setq org-tag-alist (quote ((:startgroup)
                                ("@errands" . ?e)
                                ("@banking" . ?b)
                                ("@calls" . ?c)
                                ("@home" . ?h)
                                ("@open" . ?O)
                                ("@notebook" . ?a)
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
          '(time-up category-keep priority-down))

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
                                         ,(gtd-project-context "@banking" "-consulting")
                                         ,(gtd-project-context "@home" "-consulting")
                                         ,(gtd-project-context "@open" "-consulting")
                                         ,(gtd-project-context "@online" "-consulting")
                                         ,(gtd-project-context "@notebook" "-consulting")
                                         ,(gtd-project-context "@calls" "-consulting")
                                         ,(gtd-project-context "@watch")
                                         ,(gtd-refile)))
                                       ("N" "@notebook"
                                        (,(gtd-agenda)
                                         ,(gtd-project-context "@banking")
                                         ,(gtd-project-context "@notebook")
                                         ,(gtd-project-context "@calls")
                                         ,(gtd-project-context "@open")
                                         ,(gtd-project-context "@online")
                                         ,(gtd-project-context "@errands")
                                         ,(gtd-project-context "@watch")
                                         ,(gtd-refile)))
                                       ("M" "Mobile"
                                        (,(gtd-agenda)
                                         ,(gtd-project-context "@open")
                                         ,(gtd-project-context "@calls")
                                         ,(gtd-project-context "@errands")
                                         ,(gtd-project-context "@online")
                                         ,(gtd-project-context "@banking")))
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
    (use-package djr-org-mode-private)

    (defun djr/org-mode-refile-current-task-as-single-task ()
      "Refiles the current task as a single task in gtd.org"
      (interactive)
      (let* ((org-refile-targets `((,gtd-file . (:tag . "single"))))
             ;; Because the tag captures the top level to, grab the second entry
             (rfloc (nth 1 (org-refile-get-targets))))
        (org-refile nil nil rfloc)))

    (defun djr/eval-sexp-on-line ()
      "On the current org header line, find the sexp and evaluate it"
      (interactive)
      (org-end-of-line)
      (search-backward ")")
      (forward-char)
      (eval-last-sexp nil))

    ;; S is too dangerous as it is, in VI mode, happy to repurpose it
    (define-key evil-normal-state-map "S" 'djr/eval-sexp-on-line)

    (setq org-completion-use-ido t
          org-refile-targets `((,(remove brain-file org-agenda-files) :level . 1)
                               (,brain-file . (:level . 0))
                               (,period-log-file . (:level . 0))
                               (,someday-file . (:level . 0))
                               (,blog-ideas-file . (:level . 0))
                               (,conversations-file . (:level . 1))
                               (nil . (:level . 1)))
          org-refile-use-outline-path (quote file)
          org-outline-path-complete-in-steps nil
          org-refile-allow-creating-parent-nodes nil)

    (setq org-use-sub-superscripts nil)

    ;; Clocking
    (use-package bh-org-mode)

    (setq org-agenda-clockreport-parameter-plist
          (quote (:link t :maxlevel 2 :fileskip0 t :compact t :narrow 80)))

    (setq org-clock-into-drawer t
          org-clock-persist 'history)

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

    ;; (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

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
             (shell . t)
             (ledger . t)
             (org . t)
             (plantuml . t)
             (latex . t))))

    (setq org-confirm-babel-evaluate nil)

                                        ; Use fundamental mode when editing plantuml blocks with C-c '
    (add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

    ;; org-todotxt

    ;; (use-package org-todotxt)

    (setq org-agenda-sticky nil)

    ;; (defun djr/org-todotxt-auto-push-all-agendas ()
    ;;   (interactive)
    ;;   (org-todotxt-sync "~/Dropbox/Apps/Simpletask App Folder/todo.txt")
    ;;   (org-todotxt-push "~/Dropbox/todo/clockwork-todo.txt"))

    ;; (setq org-todotxt-auto-push-function 'djr/org-todotxt-auto-push-all-agendas
    ;;       org-todotxt-auto-push-file-list `(,gtd-file)
    ;;       org-todotxt-auto-push-delay 1
    ;;       org-todotxt-inbox-for-pull inbox-file
    ;;       org-todotxt-enable-sync t)

    ;(when *my-primary-emacs-instance*
    ;  (org-todotxt-install-after-save-hook))

    ;; Archive

    (setq org-archive-default-command (quote org-archive-to-archive-sibling)
          org-archive-location "%s_archive.gpg::")

    ;; Dim blocked tasks
    (setq org-agenda-dim-blocked-tasks 'invisible)

    ;; Compact the block agenda view
    (setq org-agenda-compact-blocks t)

    (setq org-enforce-todo-checkbox-dependencies t
          org-enforce-todo-dependencies t)

    (setq org-use-speed-commands t
          ;; Also see org-speed-commands-default
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
                                          ("N" . org-narrow-to-subtree)
                                          ("P" . org-pomodoro)
                                          ("q" . djr/show-org-agenda-refreshing-if-empty)
                                          ("s" . djr/org-mode-refile-current-task-as-single-task)
                                          ("S" . djr/eval-sexp-on-line)
                                          ("z" . org-add-note)
                                          ("W" . widen))))

    (setq org-agenda-persistent-filter t)

    (when *my-primary-emacs-instance*
      (setq org-clock-idle-time 5))

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
      (let* ((org-agenda-files agenda-files))
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

    (defun djr/show-org-agenda-refreshing-if-empty ()
      "If the Org Agenda buffer has been drawn, show it. Else refresh and show."
      (interactive)
      (if (or (not (get-buffer "*Org Agenda*"))
              (= 0 (buffer-size (get-buffer "*Org Agenda*"))))
          (djr/agenda-notebook))
      (switch-to-buffer "*Org Agenda*")
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

    ;; (when *my-primary-emacs-instance*
    ;;   (run-with-idle-timer 300 t 'djr/show-org-agenda-refreshing-if-empty))

    (add-hook 'org-insert-heading-hook
              'bh/insert-heading-inactive-timestamp 'append)

    ;; More control on output format
    (setq org-export-preserve-breaks t)

    ;; Turn a org-mobile entry into a period.org.gpg entry
    (fset 'djr/make-period-entry
          (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([47 92 91 return 68 63 92 42 return 108 108 80 108 100 119 105 return escape 106 tab 86 106 106 120 107 107] 0 "%d")) arg)))

    (global-unset-key [(control c) (control l)])
    (global-unset-key [(control c) (l)])
    (bind-key* "C-c C-l" 'org-store-link)
    (bind-key* "C-c l" 'org-insert-link)
    (bind-key* "C-c g" 'org-clock-goto)
    (global-set-key (kbd "S-<f1>") 'org-capture)
    (global-set-key (kbd "C-c c") 'org-capture)
    (global-set-key (kbd "C-c a") 'djr/show-org-agenda-refreshing-if-empty)
    (global-set-key (kbd "C-c A") 'org-agenda)
    (global-set-key (kbd "<f12>") 'djr/agenda-notebook)
    (global-set-key (kbd "S-<f12>") 'djr/agenda-home)
    ;; https://www.reddit.com/r/emacs/comments/3tn1ba/orgmode_homerowbindings_for_changing_level_etc/cx7r2l4

    ;;(global-set-key (kbd "C-c N") 'djr/agenda-notebook)

    (defun djr/enter-org-speedmode ()
      "Moves to start of heading where org-use-speed-commands starts to work. Switch to insert for it to take effect."
      (interactive)
      (org-back-to-heading)
      (evil-change-state 'insert))

    (defun djr/org-insert-new-heading ()
      (interactive)
      (goto-char (point-max))
      (org-insert-heading-respect-content)
      (evil-change-state 'insert))))

(provide 'djr-org-mode)
