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
  (("C-x j" . my/jira-login)
   (:map org-jira-entry-mode-map
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

  (defun my/create-issue-from-template (project type summary &optional template)
    "docstring"
    (org-jira-create-issue project type summary (my/get-issue-template template)))

  (defun my/create-default-issue (project type summary)
    ""
    (interactive)
    (create-issue-from-template project type summary))

  (defun my/get-issues-overview ()
    "Fetches my assigned issues in current sprint, which are unresolved"
    (interactive)
    (my/org-jira-get-issues-from-custom-jql
     `((:jql ,org-jira-default-jql
             :filename "Open Issues Overview"
             :group-by status))))

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
        (error "Not on an issue"))))

  (defun my/jira-login ()
    (interactive)
    (setq jiralib-url (my/jira-auth-info "url"))
    (jiralib-login (my/jira-auth-info "email") (my/jira-auth-info 'secret))
    (org-jira-mode 1)))

(provide 'base--organisation)
