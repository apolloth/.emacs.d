(use-package
  dashboard
  :demand t
  :diminish page-break-lines-mode

  :custom
  ((dashboard-startup-banner "~/.emacs.d/configuration/dashboard/emacs-mascot.png")
   (dashboard-items '((recents  .  10)
                      (projects .   5)
                      (agenda   .   5)))
   (dashboard-icon-type 'nerd-icons)
   (dashboard-set-file-icons t)
   (dashboard-set-heading-icons t)
   (dashboard-center-content t)
   (dashboard-filter-agenda-entry 'dashboard-filter-agenda-by-todo)

   (dashboard-startupify-list
    '(dashboard-insert-banner
      dashboard-insert-banner-title
      dashboard-insert-newline
      dashboard-insert-newline
      dashboard-insert-items
      dashboard-insert-newline
      dashboard-insert-newline
      dashboard-insert-newline
      dashboard-insert-newline
      dashboard-insert-newline
      dashboard-insert-newline
      dashboard-insert-newline
      dashboard-insert-footer)))

  :config
  (dashboard-setup-startup-hook))


(provide 'base--startup)
