(use-package kmacro
  :ensure nil

  :bind*
  (("M-! m s" . start-kbd-macro)
   ("M-! m S" . kmacro-end-macro)
   ("M-! m m" . kmacro-end-and-call-macro)))

(use-package vertico
  :ensure t
  :requires (vertico-auto-loads embark)

  :init
  (vertico-mode 1)
  (vertico-multiform-mode 1)

  :custom
  (vertico-resize t)
  (vertico-cycle t)
  (vertico-multiform-commands
   '((execute-extended-command buffer)
     (devdocs-lookup buffer)
     (my-consult-line buffer)
     (consult-buffer buffer)
     (consult-recent-file buffer)
     (consult-mode-command buffer)
     (consult-complex-command buffer)
     (embark-bindings buffer)
     (embark-act buffer)
     (consult-locate buffer)
     (consult-projectile buffer)
     (my-consult-git-grep buffer)
     (my-consult-find buffer)
     (my-consult-find-home-dir buffer)
     (consult-locate buffer)
     (consult-ripgrep buffer)
     (imenu buffer)

     (t reverse)))

  :bind*
  ((:map vertico-map
         ("M-n" . vertico-scroll-up)
         ("M-p" . vertico-scroll-down)
         ("M-j" . vertico-quick-exit)

         ("C-," . embark-act)
         ("M-," . vertico-quick-embark)))

  :config
  (defun vertico-quick-embark ()
    (interactive)
    (vertico-quick-jump)
    (embark-act)))

(use-package consult
  :ensure t
  :init
  (defvar consult-keymap* (make-sparse-keymap))

  :functions
  (region-to-text
   wrap-region-text
   my-consult-git-grep
   my-consult-line
   my-consult-line-multi
   my-consult-outline
   my-consult-find
   my-consult-find-in-home-dir)

  :config
  (defun region-to-text ()
    (if mark-active
        (buffer-substring-no-properties (region-beginning) (region-end))))

  (defun wrap-region-text (func)
    (interactive)
    (let ((text (region-to-text)))
      (deactivate-mark)
      (funcall func text)))

  (defun my-consult-git-grep ()
    (interactive)
    (wrap-region-text (apply-partially 'consult-git-grep "./")))

  (defun my-consult-line ()
    (interactive)
    (wrap-region-text 'consult-line))

  (defun my-consult-line-multi ()
    (interactive)
    (wrap-region-text 'consult-line))

  (defun my-consult-outline ()
    (interactive)
    (wrap-region-text 'consult-outline))

  (defun my-consult-find (&optional dir)
    (interactive)
    (wrap-region-text (apply-partially 'consult-find dir)))

  (defun my-consult-find-in-home-dir ()
    (interactive)
    (my-consult-find "~/"))

  :bind-keymap*
  ("C-s" . consult-keymap*)

  :bind
  (:map consult-keymap*
        ("h"   . my-consult-find-in-home-dir)
        ("g"   . my-consult-git-grep)
        ("s"   . my-consult-line)
        ("m"   . my-consult-line-multi)
        ("o"   . my-consult-outline)

        ("b"   . consult-buffer)
        ("cc"  . consult-complex-command)
        ("l"   . consult-goto-line)
        ("f"   . my-consult-find)
        ("F"   . consult-locate)
        ("cm"  . consult-mode-command)
        ("p"   . consult-projectile)
        ("r"   . consult-ripgrep)

        ("t"   . consult-todo-dir)
        ("ys" .  consult-yasnippet)
        ("yf" .  consult-yasnippet-visit-snippet-file)
        ("n"   . consult-denote-find)

        ("d"   . devdocs-lookup)))

(use-package consult-projectile
  :ensure t

  :config
  (setq
   projectile-completion-system 'auto
   projectile-file-exists-remote-cache-expire 300
   projectile-file-exists-local-cache-expire nil
   projectile-enable-idle-timer t
   projectile-mode-line-prefix " ")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "dist")
  (add-to-list 'projectile-globally-ignored-directories "elpa")
  (add-to-list 'projectile-globally-ignored-directories "cache")
  (add-to-list 'projectile-globally-ignored-directories ".cache")
  (add-to-list 'projectile-globally-ignored-directories "devdocs")

  (defun save-all-buffers ()
    (interactive)
    (save-some-buffers t))

  :bind*
  (("C-x C-S-s" . save-all-buffers)))

(use-package consult-yasnippet
  :ensure t)

(use-package consult-todo
  :ensure t
  :custom
  (consult-todo-only-comment t))

(use-package consult-denote
  :ensure t)

(use-package dabbrev
  :ensure t)

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)

  :custom
  ((corfu-separator ?\s)
   (corfu-cycle t)
   (global-corfu-minibuffer nil)
   (tab-always-indent 'complete))

  :config
  (delete-selection-mode t)

  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-j" . corfu-insert)
        ("C--" . corfu-expand)))

(use-package cape
  :ensure t
  :after corfu

  :hook
  ((completion-at-point-functions cape-dabbrev)
   (completion-at-point-functions cape-file))

  :bind
  ("C-ö" . completion-at-point)
  ("C-S-ö" . cape-prefix-map))

(use-package orderless
  :ensure t

  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :custom
  (marginalia-separator "| ")
  (marginalia-prompt-categories
   '(("\\<customize group\\>" . customize-group)
     ("\\<M-x\\>" . command)
     ("\\<package\\>" . package)
     ("\\<bookmark\\>" . bookmark)
     ("\\<color\\>" . color)
     ("\\<face\\>" . face)
     ("\\<environment variable\\>" . environment-variable)
     ("\\<function\\|hook to remove\\>" . function)
     ("\\<variable\\>" . variable)
     ("\\<input method\\>" . input-method)
     ("\\<charset\\>" . charset)
     ("\\<coding system\\>" . coding-system)
     ("\\<minor mode\\>" . minor-mode)
     ("\\<kill-ring\\>" . kill-ring)
     ("\\<tab by name\\>" . tab)
     ("\\<library\\>" . library)
     ("\\<theme\\>" . theme)
     ("\\<Find\\>" . file)
     ("\\<Act\\>" . embark-keybinding)))

  (marginalia-command-categories
   '((imenu . imenu)
     (recentf-open . file)
     (where-is . command)

     (consult-buffer . buffer)
     (consult-theme . theme)
     (consult-projectile . project-file)))

  :config
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-,"   .   embark-act)
   ("M-#"   .   embark-dwim)
   ("C-h , ," . embark-bindings-at-point)
   ("C-h , b" . embark-bindings))

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;TODO:  use this in tiling.el
(use-package winner
  :ensure nil
  :demand t
  :init
  :config
  (winner-mode)
  :bind*
  (("C-M-#" . toggle-fullframe-window)))

(use-package golden-ratio
  :ensure t
  :config
  (golden-ratio-mode 1))

(use-package transpose-frame
  :init
  (defvar transpose-frame-keymap* (make-sparse-keymap))

  :bind-keymap*
  ("C-M-." . transpose-frame-keymap*)

  :bind
  (:map transpose-frame-keymap*
        ("t"       . transpose-frame)
        ("j"       . flop-frame)
        ("k"       . flip-frame)
        ("<down>"  . rotate-frame)
        ("<right>" . rotate-frame-clockwise)
        ("<left>"  . rotate-frame-anti-clockwise)))

(use-package fullframe
  :config
  (setq fullframe/advice-generic-quit-commands nil))

(use-package buffer-move
  :config (setq buffer-move-stay-after-swap nil)
  :bind
  (("C-M-' <up>" . buf-move-up)
   ("C-M-' <down>" . buf-move-down)
   ("C-M-' <left>" . buf-move-left)
   ("C-M-' <right>" . buf-move-right)))

(use-package windmove
  :ensure t
  :bind*
  (("<S-up>"    . windmove-up)
   ("<S-down>"  . windmove-down)
   ("<S-left>"  . windmove-left)
   ("<S-right>" . windmove-right)))

(use-package elscreen

  :commands
  (elscreen-create)

  :bind
  (("M-n c" . elscreen-create)
   ("M-n C" . elscreen-clone)

   :map elscreen-map
   ("c" . elscreen-create)
   ("C" . elscreen-clone)
   ("k" . elscreen-kill)
   ("K" . elscreen-kill-screen-and-buffers)
   ("p" . elscreen-previous)
   ("n" . elscreen-next)
   ("N" . elscreen-toggle)
   ("t" . elscreen-screen-nickname)
   ("f" . elscreen-select-and-goto))

  :config
  (defun with-elscreen (open-command close-command)
    (advice-add
     open-command
     :before
     (lambda (&rest args)
       (call-interactively 'elscreen-create))
     '((name . "elscreen-open-command")))

    (advice-add
     close-command
     :after
     (lambda (&rest args)
       (call-interactively 'elscreen-kill))
     '((name . "elscreen-close-command"))))

  (setq
   elscreen-prefix-key "\M-n"
   elscreen-display-screen-number nil
   elscreen-display-tab nil)

  (elscreen-start))

(use-package ediff
  :ensure nil

  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally)
  (with-elscreen #'ediff-buffers #'ediff-quit)
  (with-elscreen #'ediff-files #'ediff-quit)
  (with-elscreen #'ediff-directories #'ediff-quit)

  :commands
  (ediff-buffers
   ediff-files
   ediff))

(use-package ztree
  :demand t

  :commands
  (ztree-diff)

  :init
  (defalias 'ediff-trees 'ztree-diff)

  (setq ztree-diff-filter-list '("^\\.$" "^\\.\\.$" "^\\.git$" "^\\.DS_Store$" "^node_modules$")
        ztree-draw-unicode-lines t))

(use-package tiling
  :load-path "configuration/")

(use-package ace-jump-mode
  :ensure t

  :bind
  (("M-j" . ace-jump-char-mode)
   ("M-b" . ace-jump-mode-pop-mark)
   ("M-l" . ace-jump-line-mode)))

(use-package ace-window
  :ensure t
  :after (tiling pkg--zoom consult)
  :init
  (defvar window-management-keymap*  (make-sparse-keymap))

  (defun make-new-frame ()
    "Create a new frame and make it fullscreen."
    (interactive)
    (let ((new-frame (make-frame)))
      (select-frame-set-input-focus new-frame)
      (toggle-frame-maximized new-frame)
      (zoom/default)))

  (defun my-push-global-mark ()
    "Create a global mark and deactivate deactivates region"
    (interactive)
    (push-mark))

  :config
  (setq
   aw-scope 'global
   aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
   aw-dispatch-always t)

  :bind-keymap*
  ("C-o" . window-management-keymap*)

  :bind*
  (("M-o" . ace-window)

   :map window-management-keymap*
   ("f" . toggle-frame-fullscreen)
   ("m" . make-new-frame)
   ("k" . delete-other-windows)
   ("." . xref-find-definitions-other-window)

   ("o"   . create-tiling-window)
   ("x"   . delete-tiling-window)
   ("SPC" . toggle-tiling)
   ("j"   . tile-layout-master-left)
   ("t"   . tile-layout-master-top)
   ("v"   . tile-layout-even-vertical)
   ("b"   . tile-layout-even-horizontal)

   ("A" . my-push-global-mark)
   ("a" . pop-global-mark)))

(use-package expand-region
  :bind*
  (("C-." . er/expand-region)
   ("C-:" . er/contract-region)))

(use-package which-key
  :demand t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))

(use-package undo-tree
  :demand t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))


(use-package multiple-cursors
  :ensure t
  :bind*
  (("M-c c" . mc/mark-more-like-this-extended)
   ("M-c n" . mc/mark-next-like-this)
   ("M-c p" . mc/mark-previous-like-this)

   ("M-c r" . mc/mark-all-in-region)
   ("M-c d" . mc/mark-all-like-this-in-defun)
   ("M-c x" . mc/mark-all-like-this-dwim)
   ("M-c a" . mc/mark-all-like-this)
   ("M-c l" . mc/edit-lines)

   ("M-c i n" . mc/insert-numbers)
   ("M-c i l" . mc/insert-letters)
   ("M-c i s" . mc/sort-regions)
   ("M-c i r" . mc/reverse-regions)))

(use-package mc-extras
  :ensure t

  :requires (multiple-cursors-core ace-jump-mode)

  :bind*
  ("M-c RET" . add-n-fake-cursors-newline)
  ("M-c SPC" . mc/remove-current-cursor)
  ("M-c <right>" . mc/mark-next-sexps)
  ("M-c <left>" . mc/mark-previous-sexps)
  ("M-c <up>" . mc/mark-all-above)
  ("M-c <down>" . mc/mark-all-below)
  ("M-c f" . freeze-fake-cursors-and-point)
  ("M-c F" . unfreeze-fake-cursors-without-point)

  :functions
  (mc/create-fake-cursor-at-point
   mc/maybe-multiple-cursors-mode
   add-fake-cursor-newline)

  :config
  (defun freeze-fake-cursors-and-point ()
    (interactive)
    (mc/create-fake-cursor-at-point)
    (mc/freeze-fake-cursors))

  (defun unfreeze-fake-cursors-without-point ()
    (interactive)
    (mc/unfreeze-fake-cursors)
    (mc/remove-current-cursor))

  (defun add-fake-cursor-newline ()
    (interactive)
    (mc/create-fake-cursor-at-point)
    (newline)
    (mc/maybe-multiple-cursors-mode))

  (defun add-n-fake-cursors-newline ()
    (interactive)
    (dotimes (_ (read-number "How many? "))
      (add-fake-cursor-newline))))

(use-package
  ace-mc
  :ensure t

  :requires
  (multiple-cursors
   multiple-cursors-core
   mc-extras)

  :bind*
  ("M-c j" . ace-mc-add-multiple-cursors)
  ("M-c y" . add-multiple-cursors-line-mode)

  :functions
  (mc-add-fake-cursors-between)

  :config
  (defun add-multiple-cursors-line-mode ()
    (interactive)
    (ace-mc-add-multiple-cursors 16)))

(use-package
  pkg--zoom
  :demand t
  :load-path "configuration/"

  :init
  (defvar zoom-keymap*  (make-sparse-keymap))

  :bind-keymap*
  ("M-+" . zoom-keymap*)

  :bind
  ((:map zoom-keymap*
         ("#" . zoom/default)
         ("+" . zoom/inc)
         ("-" . zoom/dec)
         ("1" . zoom/custom-1)
         ("2" . zoom/custom-2)
         ("3" . zoom/custom-3))))

(use-package
  pkg--window-mirror
  :load-path "configuration/"
  :commands window-mirror/start-mirroring)

(custom-set-variables
 '(transient-mark-mode t)
 '(highlight-nonselected-windows t)
 '(shift-select-mode t)
 '(x-select-enable-clipboard t)
 '(initial-major-mode (quote fundamental-mode))
 '(initial-buffer-choice "*new*")
 '(ring-bell-function (quote ignore)))

(defalias 'yes-or-no-p 'y-or-n-p)

(when (eq system-type 'darwin)
  (setq
   ns-alternate-modifier nil
   ns-right-alternate-modifier nil
   mac-command-modifier 'meta))

;; Scroll with n Lines lookahead
(let ((n 5))
  (setq scroll-step 1)
  (setq scroll-conservatively n)
  (setq scroll-margin n))

(provide 'base--interface)
