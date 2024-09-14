;;; authentication.el --- Provides Packages and API functions to authenticate to external services

(use-package auth-source-pass
  :custom
  (auth-source-pass-extra-query-keywords t)

  :config
  (auth-source-pass-enable))

(defun my/jira-auth-info (key)
  (auth-source-pass-get key "jira-access"))

(provide 'authentication)


;;; authentication.el ends here
