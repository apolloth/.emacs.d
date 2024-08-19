(require 'tiling)


(defun my-toggle-zshell ()
  "Toggle a zshell buffer in a new window.
   If the zsh buffer is visible in the current frame, bury it and delete
   the window.  Otherwise, create a new window and switch to the zsh
   buffer."
  (interactive)
  (let ((shell-buffer-name "*zshell*"))
    (if (get-buffer-window shell-buffer-name)
        (progn
          (bury-buffer shell-buffer-name)
          (delete-window (get-buffer-window shell-buffer-name)))
      (let ((shell-buffer (get-buffer shell-buffer-name)))
        (if shell-buffer
            (pop-to-buffer shell-buffer)
          (progn
            (create-tiling-window)
            (other-window 1)
            (zshell)))))))

(global-set-key (kbd "C-<return>") 'my-toggle-zshell)

(provide 'base--shell)
