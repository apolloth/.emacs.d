(use-package kmacro
  :ensure nil

  :bind*
  (("M-! m s" . start-kbd-macro)
   ("M-! m S" . kmacro-end-macro)
   ("M-! m m" . kmacro-end-and-call-macro)))

(use-package embark
  :ensure t

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :bind
  (("C-,"   .   embark-act)
   ("M-#"   .   embark-dwim)
   ("C-h , ," . embark-bindings-at-point)
   ("C-h , b" . embark-bindings))

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package vertico
  :ensure t
  :init
  (require 'vertico-autoloads)
  (vertico-mode 1)
  (vertico-multiform-mode 1)

  :custom
  (vertico-resize t)
  (vertico-cycle t)
  (vertico-multiform-commands
   '((embark-act grid)
     (my-consult-outline reverse)
     (consult-goto-line reverse)
     (t buffer)))

  :bind*
  (:map vertico-map
        ("M-n" . vertico-scroll-up)
        ("M-p" . vertico-scroll-down)
        ("M-j" . vertico-quick-exit)

        ("C-," . embark-act)
        ("M-," . vertico-quick-embark))

  :config
  (defun vertico-quick-embark ()
    (interactive)
    (vertico-quick-jump)
    (embark-act)))

(use-package consult
  :ensure t
  :init
  (defvar consult-keymap* (make-sparse-keymap "consult"))

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
    (wrap-region-text (apply-partially 'consult-git-grep ".")))

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

  (defvar-keymap my/consult-keymap
    :doc "My custom Consult keymap."
    "h"   #'my-consult-find-in-home-dir
    "g"   #'my-consult-git-grep
    "s"   #'my-consult-line
    "m"   #'my-consult-line-multi
    "o"   #'my-consult-outline
    "b"   #'consult-buffer
    "c c" #'consult-complex-command
    "l"   #'consult-goto-line
    "f"   #'my-consult-find
    "F"   #'consult-locate
    "c m" #'consult-mode-command
    "p"   #'consult-projectile
    "P"   #'consult-projectile-switch-project
    "r"   #'consult-ripgrep
    "t"   #'consult-todo-dir
    "y s" #'consult-yasnippet
    "y f" #'consult-yasnippet-visit-snippet-file
    "n"   #'consult-denote-find
    "d"   #'devdocs-lookup)

  :bind-keymap*
  ("C-s" . my/consult-keymap))

(use-package projectile
  :ensure t
  :custom
  (projectile-project-search-path '("~/projects"))

  :config
  (projectile-discover-projects-in-search-path))

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
  :requires nerd-icons-corfu

  :init
  (global-corfu-mode)

  :custom
  ((corfu-separator ?\s)
   (corfu-cycle t)
   (tab-always-indent 'complete))

  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("M-h" . corfu-popupinfo-toggle)
        ("C-j" . corfu-insert)
        ("M-j" . corfu-quick-insert))

  :config
  (require 'corfu-popupinfo)

  (advice-add #'corfu-quit :after #'corfu-popupinfo--hide)
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :ensure t

  :hook
  ((completion-at-point-functions cape-dabbrev)
   (completion-at-point-functions cape-file))

  :bind*
  ("C-ö" . cape-prefix-map)

  :config
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

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

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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
  (defvar transpose-frame-keymap* (make-sparse-keymap "transpose-frame"))

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

(use-package avy
  :ensure t

  :custom
  (avy-dispatch-alist
   '((?k . avy-action-kill-move)
     (?K . avy-action-kill-stay)
     (?t . avy-action-teleport)
     (?m . avy-action-mark)
     (?w . avy-action-copy)
     (?y . avy-action-yank)
     (?Y . avy-action-yank-line)
     (?z . avy-action-zap-to-char)
     (?x . avy-action-run-command)))

  :bind
  (("M-j" . avy-goto-char-timer))

  :config
  (defun avy-action-run-command (pt)
    (save-excursion
      (goto-char pt)
      (let ((command (read-key-sequence "Press keybinding: ")))
        (command-execute (key-binding command))))
    nil))

(use-package ace-window
  :ensure t
  :after (tiling pkg--zoom consult)

  :bind-keymap*
  ("C-o" . my/window-management-keymap)

  :bind*
  (("M-o" . ace-window)
   ("M-O" . aw-flip-window))

  :config
  (require 'cl-lib)

  (defvar-keymap my/window-management-keymap
    :doc "My custom keymap for ace, tiling and other window adjustments."
    "f"   #'toggle-frame-fullscreen
    "m"   #'my-make-new-frame
    "k"   #'delete-other-windows
    "."   #'xref-find-definitions-other-window

    "o"   #'create-tiling-window
    "x"   #'delete-tiling-window
    "SPC" #'toggle-tiling
    "j"   #'tile-layout-master-left
    "t"   #'tile-layout-master-top
    "v"   #'tile-layout-even-vertical
    "b"   #'tile-layout-even-horizontal

    "A"   #'my-push-global-mark
    "a"   #'pop-global-mark)

  (defun my-make-new-frame ()
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

  (defun my-aw--execute-command-other-window (command window)
    "Execute COMMAND in WINDOW."
    (aw-switch-to-window window)
    (unwind-protect
        (call-interactively command)
      (aw-flip-window)))

  (defun my-aw-add-custom-dispatches (dispatch-list)
    "Adds every defined dispatch in DISPATCH-LIST to `aw-dispatch-alist'"
    (dolist (dispatch dispatch-list)
      (cl-destructuring-bind
          (key command description) dispatch
        (add-to-list
         'aw-dispatch-alist
         (list key
               (apply-partially #'my-aw--execute-command-other-window command)
               description)))))


  (my-aw-add-custom-dispatches
   '((?p consult-projectile "Consult Projectile")
     (?l consult-line       "Consult Line")))

  (setq
   aw-scope 'global
   aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
   aw-dispatch-always t))

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
  :custom
  (undo-tree-auto-save-history nil)

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

  :init
  (require 'multiple-cursors-core)
  (require 'ace-jump-mode)

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

  :init
  (require 'multiple-cursors)
  (require 'multiple-cursors-core)
  (require 'mc-extras)

  :bind*
  ("M-c j" . ace-mc-add-multiple-cursors)
  ("M-c y" . add-multiple-cursors-and-add-cursors-in-between)

  :functions
  (add-multiple-cursors-line-mode--wrap-hook)

  :config
  ;;FIXME: Does not work at all ...
  (defun region-between-cursors ()
    "Return the region (start and end positions) between the first and last cursor."
    (let* ((cursors (sort (cons (point) (mapcar #'overlay-start (mc/all-fake-cursors))) '<))
           (first-cursor-pos (nth 0 cursors))
           (last-cursor-pos (nth (1- (length cursors)) cursors)))
      (cons first-cursor-pos last-cursor-pos)))

  (defun add-cursors-in-region (region)
    "Add a fake cursor on each line within the REGION, excluding the first and last lines."
    (let ((start (car region))
          (end (cdr region))
          (col (save-excursion
                 (goto-char (car region))
                 (current-column))))
      (save-excursion
        (goto-char start)
        (forward-line 1)
        (while (< (point) end)
          (move-to-column col)
          (unless (or (mc/fake-cursor-at-point) (eq (point) (car region)) (eq (point) (cdr region)))
            (mc/create-fake-cursor-at-point))
          (forward-line 1))
        (mc/maybe-multiple-cursors-mode))))

  (defun add-multiple-cursors-and-add-cursors-in-between ()
    "Add multiple cursors, then add fake cursors between the first and last cursor."
    (interactive)
    ;; Add the initial multiple cursors
    (ace-mc-add-multiple-cursors 16 t)
    ;; Get the region between the first and last cursors
    (let ((region (region-between-cursors)))
      ;; Add cursors in the region
      (add-cursors-in-region region))))

(use-package
  pkg--zoom
  :demand t
  :load-path "configuration/"

  :init
  (defvar zoom-keymap* (make-sparse-keymap "zoom"))

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
 '(delete-selection-mode t)
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
