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
   org-refile-targets '((org-agenda-files :maxlevel . 4)))

  (defun org-insert-jira-issue (&optional jira-issue)
    (interactive "JIRA Issue: ")
    (let* ((jira-issue
            (upcase jira-issue))

           (jira-issue-link
            (concat env-jira-host-url "browse/" jira-issue)))

      (org-insert-link nil jira-issue-link jira-issue)))

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
        (org-cut-special))))

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
        org-agenda-skip-deadline-if-done t))

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
           "* TODO %?\n"
           :empty-lines 1))))

(provide 'base--organisation)
