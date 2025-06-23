;;; authentication.el --- Provides Packages and API functions to authenticate to external services

(use-package auth-source-pass
  :custom
  (auth-source-pass-extra-query-keywords t)

  :config
  (auth-source-pass-enable))

(provide 'authentication)


;;; authentication.el ends here
