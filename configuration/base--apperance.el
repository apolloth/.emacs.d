(use-package
  spaceline
  :demand t
  :config
  (require 'spaceline-segments)
  (setq powerline-default-separator 'wave)

  (spaceline-define-segment ide-window
    "IDE-Window marker."
    (let* ((window
            (selected-window))

           (ide-window?
            (window-parameter window 'ide)))

      (when ide-window?
        "IDE")))

  (spaceline-compile
    '((buffer-id
       :priority 99
       :when (and active buffer-file-name (buffer-modified-p))
       :face highlight-face)
      ((buffer-id "~")
       :priority 99
       :when (and (not active) buffer-file-name (buffer-modified-p))
       :face highlight-face)
      ((buffer-id)
       :priority 99
       :when (not (and buffer-file-name (buffer-modified-p))))
      (projectile-root
       :priority 80)
      (major-mode
       :priority 70)
      ((flycheck-error flycheck-warning flycheck-info)
       :priority 80
       :when active)
      (version-control
       :priority 60)
      (buffer-encoding
       :when (and active vc-mode)
       :priority 50))

    '((process
       :priority 60)
      (line-column
       :separator " | "
       :priority 50
       :when active)
      (buffer-position
       :priority 40
       :when active)
      (hud
       :priority 40)))

  (setq-default
   mode-line-format
   '("%e" (:eval (spaceline-ml-main)))))

(use-package
  linum
  :hook (prog-mode . linum-mode)
  :config (setq linum-format "%3d"))

(use-package
  rainbow-mode
  :diminish rainbow-mode

  :hook
  ((prog-mode . rainbow-mode)
   (cider-repl-mode . rainbow-mode)))

(use-package
  rainbow-delimiters
  :hook
  ((prog-mode . rainbow-delimiters-mode)
   (cider-repl-mode . rainbow-delimiters-mode)))

(use-package all-the-icons
  :if (display-graphic-p))

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

(custom-set-variables '(frame-title-format (quote ((:eval (when (fboundp 'projectile-project-root)
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

;; (load-theme 'spacemacs-dark t)
;; (load-theme 'doom-one t)

;; (load-theme 'doom-dracula t)

(toggle-frame-maximized)

(provide 'base--apperance)
