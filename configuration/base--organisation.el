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
   org-gtd-directory "~/.gtd/"
   ;; org-gtd-todos-file (expand-file-name "todos.org" org-gtd-directory)
   ;; org-gtd-journal-file (expand-file-name "journal.org" org-gtd-directory)
   org-gtd-todos-file (expand-file-name "gtd.org" org-gtd-directory)
   org-gtd-journal-file (expand-file-name "gtd.org" org-gtd-directory)

   org-cycle-include-plain-lists 'integrate
   org-startup-indented t
   org-startup-folded 'content
   org-M-RET-may-split-line nil
   org-default-notes-file org-gtd-todos-file
   org-outline-path-complete-in-steps nil
   org-refile-use-outline-path t
   org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))
   org-agenda-files (list org-gtd-todos-file)
   org-ellipsis "--more"
   org-default-priority 65

   org-agenda-prefix-format
   '((agenda . " %i %-30:(org-format-outline-path (org-get-outline-path))%?-12t %s")
     (todo . " %i %-30:(org-format-outline-path (org-get-outline-path))")
     (tags . " %i %-30:(org-format-outline-path (org-get-outline-path))")
     (search . " %i %-30:(org-format-outline-path (org-get-outline-path))"))

   org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "BLOCKED(b)" "|" "DONE(d)" "DELEGATED(g)" "CANCELED(c)"))

   org-tag-alist
   '(;; For exclusiv groups
     ;; (:startgroup . nil)
     ;; ("@work" . ?w) ("@home" . ?h)
     ;; ("@tennisclub" . ?t)
     ;; (:endgroup . nil)
     ("GENERAL" . ?g) ("EMACS" . ?e) ("DAILY" . ?d)
     ("RETRO" . ?r))

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
  (;;("C-M-- <RET>" . org-capture)
   ;;("C-M-- t" . org-capture-todo)
   ("C-M-- T" . org-capture-todo-deadline)
   ;;("C-M-- j" . org-capture-journal)
   ("C-M-- c" . org-capture-calendar)
   ("C-M-- a" . org-agenda)



   :map org-mode-map
   ("C--" . org-narrow-to-subtree)
   ("M-i l" . org-insert-link)
   ("M-i j" . org-insert-jira-issue)
   ("M-i d" . org-deadline)

   ("M-j j" . org-open-at-point)

   ("C-<up>" . org-up-element)
   ("C-<down>" . org-down-element)
   ("C-<left>" . org-backward-element-with-beginning)
   ("C-<right>" . org-forward-element-with-end)

   ("C-M-<up>" . org-drag-element-backward)
   ("C-M-<down>" . org-drag-element-forward)

   ("C-k" . org-kill-line)
   ("C-S-k" . org-kill-item)))


(use-package
  org-roam
  :ensure t
  :init (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "/home/fendt/Documents/org")
  (org-roam-dailies-directory "/home/fendt/Documents/dailies")
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  ;;(setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  ;;ex-config
  ;; (org-roam-capture-templates
  ;;  ("d" "default" plain "%?"
  ;;   :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
  ;;                      "#+title: ${title}\n")
  ;;   :unnarrowed t))
  (org-roam-capture-ref-templates
   '(("r" "ref" plain ""
      :target (file+head "${slug}.org"
                         "#+title: ${title}")
      :unnarrowed t
      :immediate-finish t)
     ;; ("w" "Website" plain "%?"
     ;;  :target (file+olp "${slug}.org" "Web")
     ;;  "* %c :website:\n%U %?%:initial"
     ;;  :unarrowed t)
     ("c" "content" plain "%?"
      :target (file+head "${slug}.org"
                         "#+title: ${title}\n${body}\n")
      :unnarrowed t
      :immediate-finish t)))

  :config
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol)

  ;;(org-roam-completion-everywhere t)
  (defun org-roam-node-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (cons arg args))
          (org-roam-capture-templates
           (list (append (car org-roam-capture-templates)
                         '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))


  :bind*
  (("C-M-- <RET>" . org-roam-capture)
   ("C-M-- s" . org-roam-node-find)
   ("C-M-- r" . org-roam-ref-find)

   ;; Dailies
   ("C-M-- j" . org-roam-dailies-capture-today)
   ("C-M-- t" . org-roam-dailies-capture-tomorrow)
   ("C-M-- d" . org-roam-dailies-capture-date)

   ("C-M-- g j" . org-roam-dailies-goto-today)
   ("C-M-- g t" . org-roam-dailies-goto-tomorrow)
   ("C-M-- g d" . org-roam-dailies-goto-date)
   ("C-M-- g y" . org-roam-dailies-goto-yesterday)


   :map org-mode-map
   ;; ("M-j g" . org-roam-graph)
   ("M-j o" . org-roam-node-insert)
   ("M-j i" . org-roam-node-insert-immediate)))

(provide 'base--organisation)
