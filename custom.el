(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-inhibited t t)
 '(compilation-ask-about-save nil)
 '(confirm-nonexistent-file-or-buffer nil)
 '(delete-by-moving-to-trash nil)
 '(echo-keystrokes 0.02)
 '(frame-title-format
   '((:eval
      (when (fboundp 'projectile-project-root)
        (when (projectile-project-root)
          (concat (projectile-project-name) " - "))))
     "%b") t)
 '(global-auto-revert-non-file-buffers t)
 '(highlight-nonselected-windows t)
 '(inhibit-startup-echo-area-message t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice "*new*")
 '(initial-major-mode 'fundamental-mode)
 '(next-error-highlight t)
 '(next-error-highlight-no-select t)
 '(org-fold-core-style 'overlays)
 '(package-selected-packages
   '(a ac-emacs-eclim ace-mc aggressive-indent all-the-icons buffer-move
       cape clj-refactor consult-denote consult-projectile
       consult-todo consult-yasnippet corfu csv-mode dashboard
       diminish doom-modeline doom-themes elpy elscreen emacsql-sqlite
       embark-consult esh-autosuggest eshell-up exec-path-from-shell
       expand-region flycheck-clj-kondo fullframe git-auto-commit-mode
       gnu-elpa-keyring-update golden-ratio highlight-symbol hyperbole
       impatient-mode indium kaolin-themes marginalia markdown-mode
       mc-extras nerd-icons-corfu npm-mode orderless org-roam
       platformio-mode quickrun rainbow-delimiters rainbow-mode
       repl-driven-development restclient sass-mode scss-mode
       simple-bookmarks smartparens smartrep spaceline tide
       transpose-frame treemacs-icons-dired treemacs-magit
       treemacs-projectile treesit-auto typescript-mode undo-tree
       vega-view vertico web-mode-edit-element wgrep xref-js2
       yasnippet-snippets ztree))
 '(query-replace-highlight t)
 '(require-final-newline t)
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values
   '((eval font-lock-add-keywords nil
           `
           ((,(concat "("
                      (regexp-opt
                       '("sp-do-move-op" "sp-do-move-cl"
                         "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op"
                         "sp-do-del-cl")
                       t)
                      "\\_>")
             1 'font-lock-variable-name-face)))))
 '(select-enable-clipboard t)
 '(shift-select-mode t)
 '(show-paren-delay 0)
 '(transient-mark-mode t)
 '(truncate-partial-width-windows nil)
 '(uniquify-after-kill-buffer-p t)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(uniquify-ignore-buffers-re "^\\*")
 '(uniquify-separator "/"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
