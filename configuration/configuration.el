(require 'package)

(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/"))

      package-enable-at-startup nil)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'diminish)
  (package-install 'smartrep)
  (package-install 'use-package)
  ;; (package-install 'spacemacs-theme)
  ;; (package-install 'doom-themes)
  (package-install 'exec-path-from-shell))

(defun normalized-shell-command-to-string (shell-command)
  "Trims newlines of the output of SHELL-COMMAND."
  (thread-last shell-command
               (shell-command-to-string)
               (replace-regexp-in-string "\n\\'" "")))

(when (memq window-system '(mac ns x))

  (let* ((default-shell-path
          (normalized-shell-command-to-string "echo $SHELL"))

         (default-shell
          (thread-last default-shell-path
                       (replace-regexp-in-string ".*/" "")))

         ;;TODO: Implement general solution for default-shell
         (path
          (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))

    (setenv "PATH" path)
    (setq exec-path (split-string-and-unquote path ":")
          shell-file-name default-shell-path)))

(setq custom-file "~/.emacs.d/custom.el")

(require 'smartrep)
(require 'use-package)

;; (global-so-long-mode 1)

(setq
 use-package-verbose t
 use-package-always-ensure t

 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil

 warning-minimum-level :error)

(require 'base--apperance)
(require 'base--interface)
(require 'base--filemanagement)
(require 'base--organisation)
(require 'base--startup)
(require 'base--development)
(require 'base--shell)

(require 'file--csv)

(require 'lang--elisp)
(require 'lang--typescript)
(require 'lang--javascript)
;; (require 'lang--php)
(require 'lang--python)
(require 'lang--html)
;; (require 'lang--xml)
(require 'lang--css)
(require 'lang--clojure)
(require 'lang--basic)
(require 'lang--c)

(defun dump (data)
  (setq dump-buffer (get-buffer-create "*dump*"))
  (print data dump-buffer))

(defun replace-keymap-bindings (from to)
  (map-keymap
   (lambda (k f)
     (define-key to (vector k) f))
   from))

(provide 'configuration)
