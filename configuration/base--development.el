(use-package treesit-auto
  :ensure t

  :functions
  (treesit-auto-add-to-auto-mode-alist
   global-treesit-auto-mode)

  :custom
  (treesit-auto-install 'prompt)

  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode 1))

;; TODO Add Wrappers to 'repl-driven-development'
;;      Call Wrapped functions in own language modes
(use-package repl-driven-development
  :ensure t

  ;;:init
  ;; (unbind-key "C-r")
  ;; :config
  ;; (repl-driven-development [C-r j] java :blink nil)
  ;; (repl-driven-development [C-r p] python :blink nil)
  ;; (repl-driven-development [C-r c] "lein repl" :prompt "user=>" :blink nil)
  ;; (repl-driven-development [C-r t] terminal :blink nil)

  ;;:config
  ;; (defun my-wrap-repl-fn (keys cli
  ;;                              &key name )
  ;;   "docstring"
  ;;   (lambda ()
  ;;     (interactive)
  ;;     ))
  )

(use-package smartparens
  :diminish smartparens-mode

  :config
  ;; TODO: not here!
  (require 'smartparens-html)
  (require 'smartparens-javascript)

  (defun sp-forward-transpose-sexp ()
    (interactive)
    (sp-next-sexp)
    (sp-transpose-sexp)
    (sp-backward-sexp))

  (defun sp-backward-transpose-sexp ()
    (interactive)
    (sp-transpose-sexp)
    (sp-backward-sexp)
    (sp-backward-sexp))

  (defun sp-kill-list ()
    (interactive)
    (save-excursion
      (sp-backward-up-sexp)
      (sp-kill-sexp)))

  (defun sp-copy-list ()
    (interactive )
    (save-excursion
      (sp-backward-up-sexp)
      (sp-copy-sexp)))

  (defun sp-trim-whitespace-of-sexp ()
    (interactive)
    (set-mark (point))
    (sp-backward-whitespace)
    (delete-active-region))

  (defun sp-wrap-doublequote ()
    (interactive)
    (sp-prefix-save-excursion
     (sp-wrap-round)
     (sp-rewrap-sexp "\"")))

  :hook
  ((prog-mode . smartparens-mode)
   (cider-repl-mode . smartparens-mode)))

(use-package newcomment
  :ensure nil
  :bind
  (("C-;" . comment-dwim)))

(use-package simple
  :ensure nil
  :hook
  ((before-save . delete-trailing-whitespace)))

(use-package devdocs
  :ensure t)

(use-package aggressive-indent

  :diminish
  aggressive-indent-mode

  :hook
  (prog-mode . aggressive-indent-global-mode)

  :config
  (add-to-list 'aggressive-indent-excluded-modes 'sass-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'cider-repl-mode))

(use-package magit
  :requires (fullframe)
  :bind*
  (("C-x g" . magit)
   ("M-v g" . magit-status)
   ("M-v l" . magit-log-current)
   ("M-v L" . magit-log-all)
   ("M-v b" . magit-blame)
   ("M-v r" . magit-list-repositories))
  :init
  (defun magit-push-to-gerrit-local-branch-or-commit (source)
    "Push an arbitrary branch or commit to gerrit. The source is read in the minibuffer."
    (interactive
     (let ((source (magit-read-local-branch-or-commit "Push")))
       (list source)))
    (magit-git-command-topdir (concat "git push origin " source (concat ":refs/for/" source))))

  (defun magit-push-to-gerrit-current-branch ()
    "Push an arbitrary branch or commit to gerrit. The source is read in the minibuffer."
    (interactive)
    (magit-push-to-gerrit-local-branch-or-commit (magit-get-current-branch)))

  :config
  ;;FIXME: Use corfu/orderless instead of ivy
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq magit-repository-directories '(("/~" . 1)))
  (transient-append-suffix 'magit-push "e"
    '("P" "Push current branch to Gerrit" magit-push-to-gerrit-current-branch))
  (transient-append-suffix 'magit-push "e"
    '("U" "Push branch / commit to Gerrit" magit-push-to-gerrit-local-branch-or-commit))
  (fullframe magit-status magit-mode-quit-window)
  (fullframe magit-log-all magit-mode-quit-window)
  (fullframe magit-log-current magit-mode-quit-window))

(use-package flycheck
  :diminish
  flycheck-mode

  :hook
  (prog-mode . flycheck-mode-on-safe)

  :config
  (set-default 'flycheck-disabled-checkers '(sass)))

(use-package yasnippet-snippets
  :defer t)

(use-package yasnippet
  :diminish yas-minor-mode
  :hook
  ((prog-mode . yas-minor-mode-on)
   (cider-repl-mode . yas-minor-mode-on))

  :config
  (require 'yasnippet-snippets)
  (yas-reload-all)

  :bind*
  (:map yas-minor-mode-map
        ("C-c" . nil)))

(use-package prog-mode
  :ensure nil

  :config
  (defun enable-prettify-symbols ()
    (interactive)
    (setq prettify-symbols-alist
          '(("lambda" . 955) ; λ
            ("fn" . 955)))
    (prettify-symbols-mode))

  :hook
  ((prog-mode . enable-prettify-symbols)
   (cider-repl-mode . enable-prettify-symbols)))

(use-package highlight-symbol
  :demand t
  :diminish highlight-symbol-mode

  :init
  (defvar highlight-symbol-mode)

  :commands
  (highlight-symbol)

  :hook
  ((prog-mode . highlight-symbol-mode)
   (cider-repl-mode . highlight-symbol-mode))

  :bind
  ((:map highlight-symbol-nav-mode-map
         ("m" . highlight-symbol)
         ("n" . highlight-symbol-next)
         ("p" . highlight-symbol-prev)
         ("r" . highlight-symbol-query-replace)
         ("q" . highlight-symbol-remove-all)))

  :config
  (setq highlight-symbol-idle-delay 1.5)
  (bind-key "M-m" highlight-symbol-nav-mode-map))

(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode))

(use-package sql
  :commands (sql-mysql)
  :config
  (when (equal system-type 'windows-nt)
    (setq sql-mysql-options '("-C" "-t" "-f" "-n"))))

(use-package simple-httpd

  :config
  (defun httpd/start (&optional root port)
    "Starts the server with given `root-directory' at given `port-number'"
    (interactive "Dhttp-root: \nnhttp-port: \n")
    (setf httpd-root root
          httpd-port port)
    (httpd-start)
    (message "Started httpd on %s:%d, serving: %s"
             (cl-case httpd-host
               ((nil) "0.0.0.0")
               ((local) "localhost")
               (otherwise httpd-host))
             port root))

  (defalias 'httpd/stop 'httpd-stop)

  (defun projectile-httpd/start ()
    (interactive)
    (httpd/start (projectile-project-root) 8080))

  (defalias 'projectile-httpd/stop 'httpd/stop)

  :commands
  (httpd/start
   httpd/stop
   projectile-httpd/start
   projectile-httpd/stop))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package pkg--ide
  :load-path "configuration/"
  :config
  (ide/configure-frame-layout ide/default-layout)

  (defun ide/setup ()
    (interactive)
    (ide/open-buffers ide/default-buffers)))

(setq eval-sexp-fu-flash-face 'success-face
      eval-sexp-fu-flash-duration 0.3
      eval-sexp-fu-flash-error-face 'error-face
      eval-sexp-fu-flash-error-duration 0.5

      eldoc-echo-area-use-multiline-p t)

(provide 'base--development)
