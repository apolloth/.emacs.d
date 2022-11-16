(use-package
  dashboard
  :demand t
  :diminish page-break-lines-mode
  :config
  (setq
   dashboard-startup-banner "~/.emacs.d/configuration/dashboard/emacs-mascot.png"
   dashboard-items
   '((recents . 10)
     (projects . 5)
     (agenda . 5)))
  (dashboard-setup-startup-hook))

(provide 'base--startup)
