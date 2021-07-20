(use-package
  dashboard
  :demand t
  :diminish page-break-lines-mode
  :config
  (setq
   dashboard-startup-banner "~/.emacs.d/configuration/dashboard/emacs-logo.txt" 
   dashboard-items
   '((recents . 10)
     (projects . 5)))
  (dashboard-setup-startup-hook))

(provide 'base--startup)
