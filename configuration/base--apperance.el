(use-package doom-modeline
  :ensure t

  :init
  (line-number-mode -1)
  (column-number-mode 1)
  (doom-modeline-mode 1)

  :custom
  (doom-modeline-support-imenu t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-column-zero-based nil))

(use-package display-line-numbers
  :diminish display-line-numbers

  :custom
  (display-line-numbers-grow-only t)

  :hook
  ((prog-mode . display-line-numbers-mode))

  :bind*
  (("C-M-, c" . my-toggle-fill-column-indicator)
   ("C-M-, l" . my-toggle-line-numbers))

  :config
  (defun my-toggle-line-numbers ()
    "Toggle display of line numbers in prog-mode."
    (interactive)
    (if (bound-and-true-p display-line-numbers-mode)
        (display-line-numbers-mode -1)
      (when (derived-mode-p 'prog-mode)
        (display-line-numbers-mode 1))))

  (defun my-toggle-fill-column-indicator ()
    "Toggle display of line numbers in prog-mode."
    (interactive)
    (if (bound-and-true-p global-display-fill-column-indicator-mode)
        (global-display-fill-column-indicator-mode -1)
      (when (derived-mode-p 'prog-mode)
        (global-display-fill-column-indicator-mode 1)))))

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

(use-package nerd-icons-corfu
  :ensure t)

(use-package kaolin-themes
  :config
  (load-theme 'kaolin-ocean t)
  (kaolin-treemacs-theme))

(defface error-face
  '((t (:foreground "#CC5353"))) "Red Highlight")

(defface success-face
  '((t (:foreground "yellow" :background "#555577"))) "Green Highlight")


(setopt display-fill-column-indicator-column 80)

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
