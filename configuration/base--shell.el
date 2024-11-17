(require 'tiling)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package vterm
  :ensure t
  :hook
  ((vterm-mode .
               (lambda ()
                 (set (make-local-variable 'buffer-face-mode-face) '(:family "MesloLGS NF"))
                 (buffer-face-mode t))))

  :bind*
  (("C-<return>" . my/toggle-vterm)

   (:map vterm-mode-map
         ("C-j" . vterm-copy-mode))

   (:map vterm-copy-mode-map
         ("C-j" . vterm-copy-mode)))

  :config
  (defun my/toggle-vterm ()
    "Toggles vterm in a new window.

     Runs vterm on project root, if necessary. Or at current directory, when not inside a project.
     Opens the vterm buffer to the according running vterm proccess, in another window.
     Or buries the currently opened vterm buffer instead."
    (interactive)
    (let ((shell-buffer-name
           (projectile-generate-process-name "vterm" nil)))
      (if (get-buffer-window shell-buffer-name)
          (progn
            (bury-buffer shell-buffer-name)
            (delete-window (get-buffer-window shell-buffer-name)))
        (let ((shell-buffer (get-buffer shell-buffer-name)))
          (if shell-buffer
              (pop-to-buffer shell-buffer)
            (projectile-run-vterm-other-window)))))))


(provide 'base--shell)
