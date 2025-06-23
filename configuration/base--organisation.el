(use-package
  org

  :mode
  ("\\.org\\'" . org-mode)

  :commands
  (org-agenda
   org-capture-todo
   org-capture-todo-context
   org-capture-journal
   org-capture-calendar)

  :init
  (setq-default
   org-cycle-include-plain-lists 'integrate
   org-startup-indented t
   org-startup-folded 'content
   org-M-RET-may-split-line nil
   org-outline-path-complete-in-steps nil
   org-refile-use-outline-path t
   org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))
   org-ellipsis "--more"
   org-default-priority 65
   org-todo-keywords
   '((sequence "TODO(t)" "STARTED(s)" "BLOCKED(b)" "|" "DONE(d)" "DELEGATED(g)" "CANCELED(c)"))
   org-refile-targets '((org-agenda-files :maxlevel . 4))

   org-capture-templates
   '(("t" "Todo (without deadline)" entry (file+headline org-gtd-todos-file "Inbox")
      "* TODO %? %i %^g\n:PROPERTIES:\n:ADDED: %U\n:END:")
     ("T" "Todo (with deadline)" entry (file+headline org-gtd-todos-file "Inbox")
      "* TODO %? %i %^g\nDEADLINE: %^t\n:PROPERTIES:\n:ADDED: %U\n:END:")
     ("j" "Journal" entry (file+headline org-gtd-journal-file "Journal")
      "* %? %i %^g\n:PROPERTIES:\n:ADDED: %U\n:END:")
     ("c" "Calendar" entry (file+headline org-gtd-todos-file "Calendar")
      "* TODO %? %i %^g\nSCHEDULED: %^T\n:PROPERTIES:\n:ADDED: %U\n:END:")))

  (defun org-capture-todo ()
    (interactive)
    (org-capture nil "t"))

  (defun org-capture-todo-deadline ()
    (interactive)
    (org-capture nil "T"))

  (defun org-capture-journal ()
    (interactive)
    (org-capture nil "j"))

  (defun org-capture-calendar ()
    (interactive)
    (org-capture nil "c"))

  (defun org-insert-jira-issue (&optional jira-issue)
    (interactive "JIRA Issue: ")
    (let* ((jira-issue
            (upcase jira-issue))

           (jira-issue-link
            (concat env-jira-host-url "browse/" jira-issue)))

      (org-insert-link nil jira-issue-link jira-issue)))

  :bind*
  (:map org-mode-map
        ("M-i l" . org-insert-link)
        ("M-i j" . org-insert-jira-issue)
        ("M-i d" . org-deadline)

        ("M-j j" . org-open-at-point)

        ("C-<up>" . org-up-element)
        ("C-<down>" . org-down-element)

        ("C-<right>" . forward-word)
        ("C-<left>" . backward-word)

        ("C-M-<up>" . org-drag-element-backward)
        ("C-M-<down>" . org-drag-element-forward)

        ("C-k" .   org-kill-item)
        ("C-S-k" . org-kill-line))

  :config
  (setq org-agenda-files '("~/Documents/org/agenda")
        org-agenda-start-on-weekday 1
        org-agenda-span 'week
        org-agenda-show-all-dates t
        org-agenda-skip-deadline-prewarning-if-scheduled t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t)

  (defun org-backward-element-with-beginning ()
    (interactive)
    (let ((cursor (point)))
      (org-beginning-of-line)
      (when (= cursor (point))
        (org-backward-element))))

  (defun org-forward-element-with-end ()
    (interactive)
    (let ((cursor (point)))
      (org-end-of-line)
      (when (= cursor (point))
        (org-forward-element)
        (org-end-of-line))))

  (defun org-kill-list-item ()
    (interactive)
    (beginning-of-line)
    (org-kill-line)
    (org-kill-line))

  (defun org-kill-item ()
    (interactive)
    (let ((e (save-excursion (beginning-of-line) (org-element-at-point))))
      (if (memq (org-element-type e) '(item plain-list))
          (org-kill-list-item)
        (org-cut-special)))))

(use-package denote
  :ensure t

  :init
  (unbind-key "C-M--")

  :custom
  (expand-file-name "~/Documents/org/jira")
  (denote-known-keywords '("daily" "ticket"))

  :bind*
  (("C-M-- n" . denote)
   ("C-M-- r" . denote-region)
   ("C-M-- z" . denote-signature)
   ("C-M-- l" . denote-link)
   ("C-M-- f" . denote-link)

   ("C-M-- j" . org-agenda)
   ("C-M-- ." . my/org-capture-todo-for-date)
   ("C-M-- <return>" . org-capture))

  :config
  (setq org-capture-templates
        '(("j" "Jira Ticket" entry
           (file+headline (lambda ()
                            (let ((ticket-id (read-string "JIRA-ID: ")))
                              (expand-file-name (concat ticket-id ".org")
                                                "~/Documents/org/jira")))
                          "Tasks")
           "* TODO %^{Summary}\n  CREATED: %U\n  %?"
           :empty-lines 1)
          ("d" "TODO for date" entry
           (file (lambda ()
                   (let ((date (org-read-date nil t nil "Select a date")))
                     (expand-file-name
                      (format-time-string "%Y-%m-%d.org" date)
                      "~/Documents/org/agenda/"))))
           "* TODO Agenda\n** TODO %?"
           :empty-lines 1))))

(use-package org-jira
  :ensure t

  :custom
  (jiralib-update-issue-fields-exclude-list '(reporter))
  (org-jira-reverse-comment-order t)
  (org-jira-default-jql
   "assignee = currentUser()
      and resolution = unresolved
      and sprint in openSprints()

      ORDER BY project DESC")
  (org-jira-custom nil)

  :bind
  ((:map org-jira-entry-mode-map
         ("C-c o"  . org-jira-todo-to-jira)

         ("C-c tt" . my/create-issue-from-template)
         ("C-c tj" . my/create-default-issue)

         ("C-c ij" . my/get-issues-overview)
         ("C-c im" . my/org-jira-mention-user)))

  :config
  (require 'authentication)

  (defvar-local my/issue-template-dir
      "~/.org-jira/templates/"
    "Path to my JIRA issue templates.")

  (defun my/get-issue-template (&optional template)
    (with-temp-buffer
      (insert-file-contents (s-concat my/issue-template-dir (or template "default") ".org"))
      (buffer-string)))

  (defun my/read-issue-templates ()
    "Read issue templates."
    (let ((templates
           (->> my/issue-template-dir
                (directory-files)
                (remove-if 'file-readable-p)
                (mapcar 'file-name-sans-extension))))

      (completing-read "Template: " templates nil t nil "default")))

  (defun my/create-issue-from-template (project type summary description)
    "docstring"
    (interactive
     (let* ((project (org-jira-read-project))
            (type (org-jira-read-issue-type project))
            (summary (read-string "Summary: "))
            (template (my/read-issue-templates))
            (description (my/get-issue-template template)))
       (list project type summary description)))
    (if (or (equal project "")
            (equal type "")
            (equal summary ""))
        (error "Must provide all information!"))
    (let* ((parent-id nil)
           (ticket-struct (org-jira-get-issue-struct project type summary description)))
      (org-jira-get-issues (list (jiralib-create-issue ticket-struct)))))

  (defun my/create-default-issue (project type summary)
    ""
    (interactive)
    (my/create-issue-from-template project type summary))

  (defun my/get-issues-overview ()
    "Fetches my assigned issues in current sprint, which are unresolved"
    (interactive)
    (org-jira-get-issues-from-custom-jql
     `((:jql ,org-jira-default-jql
             :filename "Open Issues Overview"))))

  (defun my/org-jira-decoded-users (project)
    (mapcar (lambda (user)
              (cons (org-jira-decode (cdr (assoc 'displayName user)))
                    (org-jira-decode (cdr (assoc 'accountId user)))))
            (jiralib-get-users project)))

  (defun my/org-jira-mention-user ()
    "Prompts for an user and inserts a mention at point."
    (interactive)
    (let ((issue-id (org-jira-parse-issue-id)))
      (if issue-id
          (let* ((project
                  (replace-regexp-in-string "-[0-9]+" "" issue-id))

                 (jira-users
                  (my/org-jira-decoded-users project))

                 (jira-user
                  (completing-read "Jira User: " jira-users))

                 (user-id
                  (cdr (assoc jira-user jira-users))))

            (insert (format "[~accountid:%s]" user-id)))
        (error "Not on an issue")))))

(use-package jira
  :ensure t

  :custom
  (jira-token-is-personal-access-token nil)
  (jira-api-version 3)
  (jira-issues-max-results 100)
  (jira-statuses-todo '("To Do"))
  (jira-statuses-progress '("In Progress" "im Test"))
  (jira-statuses-done '("Done" "Abgenommen" "Nicht umgesetzt"))
  (jira-issues-table-fields
   '(:key :issue-type-name :priority-name :status-name :story-points :assignee-name :summary))

  :bind
  (("C-x j" . my/jira-overview)
   (:map jira-issues-mode-map
         ("j" . my/org-jira-open-issue)))

  :config
  (require 'org-jira)

  (setq jira-issues-fields
        '((:key . ((:path . (key))
                   (:columns . 12)
                   (:name . "Key")
                   (:formatter . jira-fmt-issue-key)))
          (:priority-name . ((:path . (fields priority name))
                             (:columns . 10)
                             (:name . "Priority")))
          (:priority-icon .  ((:path . (fields priority iconUrl))
                              (:columns . 10)
                              (:name . "Priority")))
          (:labels . ((:path . (fields labels))
                      (:columns . 10)
                      (:name . "Labels")))
          (:work-ratio . ((:path . (fields workratio))
                          (:columns . 6)
                          (:name . "WR")
                          (:formatter . jira-fmt-issue-progress)))
          (:original-estimate . ((:path . (fields aggregatetimeoriginalestimate))
                                 (:columns . 10)
                                 (:name . "Estimate")
                                 (:formatter . jira-fmt-time-from-secs)))
          (:remaining-time . ((:path . (fields timeestimate))
                              (:columns . 10)
                              (:name . "Remaining")
                              (:formatter . jira-fmt-time-from-secs)))
          (:assignee-name . ((:path . (fields assignee displayName))
                             (:columns . 14)
                             (:name . "Assignee")))
          (:reporter-name . ((:path . (fields reporter displayName))
                             (:columns . 14)
                             (:name . "Reporter")))
          (:components . ((:path . (fields components))
                          (:columns . 10)
                          (:name . "Components")
                          (:formatter . jira-fmt-issue-components)))
          (:fix-versions . ((:path . (fields fixVersions))
                            (:columns . 10)
                            (:name . "Fix Versions")
                            (:formatter . jira-fmt-issue-fix-versions)))
          (:status-name . ((:path . (fields status name))
                           (:columns . 12)
                           (:name . "Status")
                           (:formatter . jira-fmt-issue-status)))
          (:status-category-name . ((:path . (fields status statusCategory name))
                                    (:columns . 10)
                                    (:name . "Status Category")))
          (:creator-name . ((:path (fields creator  displayName))
                            (:columns . 10)
                            (:name . "Creator")))
          (:issue-type-name . ((:path . (fields issuetype name))
                               (:columns . 7)
                               (:name . "Type")
                               (:formatter . jira-fmt-issue-type-name)))
          (:issue-type-icon . ((:path . (fields issuetype iconUrl))
                               (:columns .  10)
                               (:name . "Type")))
          (:project-key . ((:path . (fields project key))
                           (:columns . 10)
                           (:name . "Project")))
          (:project-name .  ((:path . (fields project name))
                             (:columns . 10)
                             (:name . "Project")))
          (:parent-type-name . ((:path . (fields parent fields issuetype name))
                                (:columns . 10)
                                (:name . "Parent Type")
                                (:formatter . jira-fmt-issue-type-name)))
          (:parent-status . ((:path . (fields parent fields status name))
                             (:columns . 10)
                             (:name . "Parent Status")
                             (:formatter . jira-fmt-issue-status)))
          (:parent-key . ((:path . (fields parent key))
                          (:columns . 10)
                          (:name . "Parent Key")
                          (:formatter . jira-fmt-issue-key)))
          (:created . ((:path . (fields created))
                       (:columns . 10)
                       (:name . "Created")))
          (:updated . ((:path . (fields updated))
                       (:columns . 10)
                       (:name . "Updated")))
          (:description . ((:path . (fields description))
                           (:columns . 10)
                           (:name . "Description")))
          (:summary . ((:path . (fields summary))
                       (:columns . 10)
                       (:name . "Summary")))
          (:due-date . ((:path . (fields duedate))
                        (:columns . 10)
                        (:name . "Due Date")
                        (:formatter . jira-fmt-date)))
          (:sprints . ((:path . (fields (custom "Sprint")))
                       (:columns . 10)
                       (:name . "Sprints")
                       (:formatter . jira-fmt-issue-sprints)))
          (:line . ((:path . (fields (custom "Business line")))
                    (:columns . 10)
                    (:name . "Business Line")
                    (:formatter . jira-fmt-business-line)))
          (:story-points . ((:path . (fields customfield_10004))
                            (:columns . 12)
                            (:name . "Story Points")))
          (:cost-center . ((:path . (fields (custom "Cost center")))
                           (:columns . 10)
                           (:name . "Const Center")
                           (:formatter . jira-fmt-cost-center)))
          (:resolution . ((:path . (fields resolution name))
                          (:columns . 10)
                          (:name . "Resolution")))))

  (defun my/org-jira-open-issue ()
    (interactive)
    (let ((issue-key (jira-utils-marked-item)))
      (when issue-key
        (org-jira-get-issue issue-key)
        (org-jira-mode 1))))

  (defun my/jira-login ()
    (interactive)
    (setq jiralib-url (my/jira-auth-info "url")
          jira-base-url (my/jira-auth-info "url")
          jira-username (my/jira-auth-info "email")
          jira-token    (my/jira-auth-info 'secret))
    (jiralib-login jira-username jira-token))

  (defun my/jira-overview ()
    (interactive)
    (my/jira-login)
    (jira-issues)))


(provide 'base--organisation)
