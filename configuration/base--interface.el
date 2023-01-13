(use-package
  kmacro
  :ensure nil

  :bind*
  (("M-! m s" . start-kbd-macro)
   ("M-! m S" . kmacro-end-macro)
   ("M-! m m" . kmacro-end-and-call-macro)))

(use-package
  ivy
  :demand t
  :diminish ivy-mode

  :config
  (setq
   enable-recursive-minibuffers t
   ivy-use-selectable-prompt t
   ivy-height 15
   ivy-wrap t
   ivy-use-virtual-buffers t
   ivy-initial-inputs-alist nil)
  (ivy-mode 1)

  :bind
  (("C-x C-b" . ivy-switch-buffer)
   :map ivy-minibuffer-map
   ("C-o" . ivy-occur)))

(use-package
  counsel
  :demand t
  :diminish counsel-mode
  :config
  (counsel-mode 1)
  (delete-selection-mode t)
  :bind*
  (("C-S-Y" . counsel-yank-pop)))

(use-package
  swiper
  :requires counsel
  :init
  ;;TODO: Refactor to something like 'apply-region-text'
  (defun my-region-text ()
    (if mark-active
        (buffer-substring-no-properties (region-beginning) (region-end))))

  (defun my-git-grep ()
    (interactive)
    (let ((text (my-region-text)))
      (deactivate-mark)
      (counsel-git-grep text)))

  (defun my-swiper ()
    (interactive)
    (let ((text (my-region-text)))
      (deactivate-mark)
      (swiper text)))

  :bind
  (("C-s" . my-swiper)
   ("C-S-s" . my-git-grep)

   ("C-r" . swiper)
   ("C-S-r" . counsel-git-grep-query-replace)

   :map swiper-map
   ("C-r" . swiper-query-replace)))

(use-package
  winner
  :ensure nil
  :demand t
  :init
  (defun toggle-fullframe-window ()
    (interactive)
    (if (> (length (window-list)) 1)
        (delete-other-windows)
      (winner-undo)))
  :config
  (winner-mode)
  :bind*
  (("C-M-#" . toggle-fullframe-window)))

(use-package
  golden-ratio
  :config
  (golden-ratio-mode 1)
  (setq golden-ratio-auto-scale t))

(use-package transpose-frame
  :init
  (unbind-key "C-M-.")
  :bind
  (("C-M-. t"   . transpose-frame)
   ("C-M-. j"   . flop-frame)
   ("C-M-. k"   . flip-frame)
   ("C-M-. <down>"   . rotate-frame)
   ("C-M-. <right>"   . rotate-frame-clockwise)
   ("C-M-. <left>"   . rotate-frame-anti-clockwise)))

(use-package
  tiling
  :demand t
  :load-path "configuration/"

  :bind
  (("C-M-, ,"   . create-tiling-window)
   ("C-M-, x"   . delete-tiling-window)

   ("C-M-, j"   . tile-layout-master-left)
   ("C-M-, t"   . tile-layout-master-top)
   ("C-M-, v"   . tile-layout-even-vertical)
   ("C-M-, b"   . tile-layout-even-horizontal)
   ("C-M-, f"   . tile-layout-tiling-tile-4)

   ("C-M-, <left>"    . windmove-left)
   ("C-M-, <right>"   . windmove-right)
   ("C-M-, <up>"      . windmove-up)
   ("C-M-, <down>"    . windmove-down)))

(use-package
  fullframe
  :config
  (setq fullframe/advice-generic-quit-commands nil))

(use-package
  buffer-move
  :config (setq buffer-move-stay-after-swap nil)
  :bind
  (("C-M-' <up>" . buf-move-up)
   ("C-M-' <down>" . buf-move-down)
   ("C-M-' <left>" . buf-move-left)
   ("C-M-' <right>" . buf-move-right)))

(use-package
  windmove
  :bind
  (("<S-up>"    . windmove-up)
   ("<S-down>"  . windmove-down)
   ("<S-left>"  . windmove-left)
   ("<S-right>" . windmove-right)))

(use-package
  elscreen

  :commands
  (elscreen-create)

  :bind*
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

  :init
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

  :config
  (setq
   elscreen-prefix-key "\M-n"
   elscreen-display-screen-number nil
   elscreen-display-tab nil)

  (elscreen-start))

(use-package
  ediff
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

(use-package
  ztree
  :demand t

  :commands
  (ztree-diff)

  :init
  (defalias 'ediff-trees 'ztree-diff)

  (setq ztree-diff-filter-list '("^\\.$" "^\\.\\.$" "^\\.git$" "^\\.DS_Store$" "^node_modules$")
        ztree-draw-unicode-lines t))

(use-package
  ace-jump-mode
  :bind*
  (("M-j" . ace-jump-char-mode)
   ("M-b" . ace-jump-mode-pop-mark)
   ("M-l" . ace-jump-line-mode)))

(use-package
  ace-window
  :bind*
  (("M-#" . ace-window)
   ("C-#" . aw-flip-window))

  :config
  (setq
   aw-scope 'frame
   aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
   aw-dispatch-always t))

(use-package
  expand-region
  :bind*
  (("C-." . er/expand-region)
   ("C-:" . er/contract-region)))

;; (use-package
;;   pkg--narrow
;;   :load-path "configuration/"
;;   :commands narrow/narrow-dwim
;;   :bind*
;;   (("C--" . narrow/narrow-dwim)
;;    ("C-+" . narrow/widen)))

(use-package
  which-key
  :demand t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))

(use-package
  undo-tree
  :demand t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package
  multiple-cursors

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
   ("M-c i r" . mc/reverse-regions)

   :map mc/keymap
   ("<tab>" . mc-hide-unmatched-lines-mode)))

(use-package
  mc-extras
  :config

  (require 'multiple-cursors-core)
  (require 'ace-jump-mode)

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
      (add-fake-cursor-newline)))

  :bind*
  ("M-c RET" . add-n-fake-cursors-newline)
  ("M-c SPC" . mc/remove-current-cursor)
  ("M-c <right>" . mc/mark-next-sexps)
  ("M-c <left>" . mc/mark-previous-sexps)
  ("M-c <up>" . mc/mark-all-above)
  ("M-c <down>" . mc/mark-all-below)
  ("M-c f" . freeze-fake-cursors-and-point)
  ("M-c F" . unfreeze-fake-cursors-without-point))

(use-package
  ace-mc
  :config

  (require 'multiple-cursors-core)
  (require 'mc-extras)

  (defun add-multiple-cursors-line-mode ()
    (interactive)
    (ace-mc-add-multiple-cursors 16))

  :bind*
  ("M-c j" . ace-mc-add-multiple-cursors)
  ("M-c y" . add-multiple-cursors-line-mode))

(use-package
  pkg--zoom
  :demand t
  :load-path "configuration/"
  :requires smartrep
  :config
  (smartrep-define-key
      global-map
      "M-+"
    '(("#" . zoom/default)
      ("+" . zoom/inc)
      ("-" . zoom/dec)
      ("1" . zoom/custom-1)
      ("2" . zoom/custom-2)
      ("3" . zoom/custom-3))))

(use-package
  pkg--window-mirror
  :load-path "configuration/"
  :commands window-mirror/start-mirroring)

;; (use-package
;;   google-translate
;;   ())

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
