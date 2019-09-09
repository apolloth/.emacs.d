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

  :config
  (setq
   org-gtd-directory "~/.gtd/"
   ;; org-gtd-todos-file (expand-file-name "todos.org" org-gtd-directory)
   ;; org-gtd-journal-file (expand-file-name "journal.org" org-gtd-directory)
   org-gtd-todos-file (expand-file-name "gtd.org" org-gtd-directory)
   org-gtd-journal-file (expand-file-name "gtd.org" org-gtd-directory))
  
  (setq
   org-startup-indented t
   org-M-RET-may-split-line t
   org-default-notes-file org-gtd-todos-file
   org-outline-path-complete-in-steps nil
   org-refile-use-outline-path t
   org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))
   show-week-agenda-p t
   org-agenda-files (list org-gtd-todos-file)
   org-todo-keywords '((sequence "TODO(t)" "BLOCKED(b)" "|" "DONE(d)" "DELEGATED(g)" "CANCELED(c)"))
   org-tag-alist '(;; For exclusiv groups
                   ;; (:startgroup . nil)
                   ;; ("@work" . ?w) ("@home" . ?h)
                   ;; ("@tennisclub" . ?t)
                   ;; (:endgroup . nil)
                   ("GENERAL" . ?g) ("EMACS" . ?e)))

  (setq
   org-capture-templates
   '(("t" "Todo (without context)" entry (file+headline org-gtd-todos-file "Inbox")
      "* TODO %? %i %^g\n:PROPERTIES:\n:ADDED: %U\n:CONTEXT:\n:DEADLINE: %^t\n:END:")
     ("T" "Todo (with context)" entry (file+headline org-gtd-todos-file "Inbox")
      "* TODO %? %i %^g\n:PROPERTIES:\n:ADDED: %U\n:CONTEXT: %a\n:DEADLINE: %^t\n:END:")
     ("j" "Journal" entry (file+headline org-gtd-journal-file "Journal")
      "* %? %i %^g\n:PROPERTIES:\n:ADDED: %U\n:CONTEXT: %a\n:END:")
     ("c" "Calendar" entry (file+headline org-gtd-todos-file "Calendar")
      "* TODO %? %i %^g\n:PROPERTIES:\n:ADDED: %U\n:CONTEXT: %a\n:SCHEDULED: %^T\n:DEADLINE: %^t\n:END:")))

  (defun org-capture-todo ()
    (interactive)
    (org-capture nil "t"))

  (defun org-capture-todo-context ()
    (interactive)
    (org-capture nil "T"))

  (defun org-capture-journal ()
    (interactive)
    (org-capture nil "t"))

  (defun org-capture-calendar ()
    (interactive)
    (org-capture nil "c"))
  
  :bind* 
  (("C-M-- <RET>" . org-capture)
   ("C-M-- t" . org-capture-todo)
   ("C-M-- T" . org-capture-todo-context)
   ("C-M-- j" . org-capture-journal)
   ("C-M-- c" . org-capture-calendar)
   ("C-M-- a" . org-agenda)))

(provide 'base--organisation)
