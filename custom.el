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
 '(package-selected-packages
   '(ac-emacs-eclim ace-mc aggressive-indent all-the-icons buffer-move
                    cape clj-refactor company-emacs-eclim
                    company-quickhelp consult-denote
                    consult-projectile consult-todo consult-yasnippet
                    corfu counsel-projectile csv-mode dashboard
                    devdocs diminish doom-themes elpy elscreen
                    embark-consult esh-autosuggest eshell-up
                    exec-path-from-shell expand-region
                    flycheck-clj-kondo fullframe git-auto-commit-mode
                    gnu-elpa-keyring-update golden-ratio helm-lsp
                    highlight-symbol hyperbole impatient-mode indium
                    lsp-ivy lsp-java lsp-ui marginalia mc-extras
                    nerd-icons npm-mode orderless org-roam
                    platformio-mode quickrun rainbow-delimiters
                    rainbow-mode restclient sass-mode scss-mode
                    simple-bookmarks smartparens smartrep spaceline
                    tide transpose-frame treemacs-icons-dired
                    treemacs-magit treemacs-projectile treesit-auto
                    undo-tree use-package vega-view vertico
                    web-mode-edit-element which-key xref-js2
                    yasnippet-snippets ztree))
 '(query-replace-highlight t)
 '(require-final-newline t)
 '(ring-bell-function 'ignore)
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
