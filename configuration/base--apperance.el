(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)

  :custom
  (doom-modeline-support-imenu t))

(use-package display-line-numbers
  :diminish display-line-numbers
  :init
  (unbind-key "C-M-,")

  ;;WATCHOUT This works because display-line-numbers is initialized with t.
  ;;         Other values ('visual or 'relative) are also excepted, but are
  ;;         not currently needed by me.
  (defun my-toggle-line-numbers ()
    (interactive)
    (setq display-line-numbers (not display-line-numbers)))

  :custom
  (display-line-numbers-grow-only t)

  :hook
  ((prog-mode . display-line-numbers-mode))

  :bind
  (("C-M-, t" . my-toggle-line-numbers)))

(use-package rainbow-mode
  :diminish rainbow-mode

  :hook
  ((prog-mode . rainbow-mode)
   (cider-repl-mode . rainbow-mode)))

(use-package rainbow-delimiters
  :hook
  ((prog-mode . rainbow-delimiters-mode)
   (cider-repl-mode . rainbow-delimiters-mode)))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package nerd-icons
  :ensure t)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-treemacs-theme "doom-colors"
        doom-dracula-brighter-modeline t)
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(defface error-face
  '((t (:foreground "#CC5353"))) "Red Highlight")

(defface success-face
  '((t (:foreground "yellow" :background "#555577"))) "Green Highlight")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(let ((n 0))
  (setq scroll-step 1)
  (setq scroll-conservatively n)
  (setq scroll-margin n))

(custom-set-variables
 '(inhibit-startup-screen t)
 '(inhibit-startup-message t)
 '(inhibit-startup-echo-area-message t)
 '(truncate-partial-width-windows nil)
 '(echo-keystrokes 0.02)
 '(query-replace-highlight t)
 '(next-error-highlight t)
 '(next-error-highlight-no-select t))

(set-default 'truncate-lines nil)
(set-default 'indent-tabs-mode nil)

(show-paren-mode 1)
(custom-set-variables '(show-paren-delay 0))

(custom-set-variables
 '(frame-title-format
   (quote
    ((:eval
      (when (fboundp 'projectile-project-root)
        (when (projectile-project-root)
          (concat (projectile-project-name) " - "))))
     "%b"))))

(global-hl-line-mode 1)

(add-hook
 'prog-mode-hook
 (lambda ()
   (font-lock-add-keywords
    nil
    '(("\\<\\(HACK\\|NOTE\\|FIXME\\|TODO\\|TEST\\|WATCHOUT\\|REVIEW\\|BUG\\)"
       1 font-lock-warning-face t)))))

(toggle-frame-maximized)

(provide 'base--apperance)
