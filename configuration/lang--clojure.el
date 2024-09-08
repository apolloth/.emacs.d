(use-package clojure-mode
  :after smartparens

  :mode
  (("\\.clj\\'" . clojure-mode)
   ("\\.cljs\\'" . clojurescript-mode))

  :config
  (sp-with-modes sp-clojure-modes
    (sp-local-pair "'" "'"
                   :when '(sp-in-string-p
                           sp-in-comment-p))

    (sp-local-pair "`" "`"
                   :when '(sp-in-string-p
                           sp-in-comment-p)))

  :bind*
  (:map clojure-mode-map
        ("C-<left>" . sp-backward-sexp)
        ("C-<right>" . sp-forward-sexp)

        ("C-<up>" . sp-backward-up-sexp)
        ("C-<down>" . sp-down-sexp)

        ("M-<left>" . sp-forward-barf-sexp)
        ("M-<right>" . sp-forward-slurp-sexp)

        ("C-c M-<left>" . sp-add-to-next-sexp)
        ("C-c M-<right>" . sp-add-to-previous-sexp)

        ("C-M-<up>" . sp-backward-transpose-sexp)
        ("C-M-<down>" . sp-forward-transpose-sexp)

        ("M-\"" . sp-wrap-doublequote)

        ("C-c j" . sp-wrap-round)
        ("C-c k" . sp-wrap-curly)
        ("C-c l" . sp-wrap-square)

        ("C-k" . sp-kill-sexp)
        ("C-S-K" . sp-unwrap-sexp)

        ("M-k" . sp-splice-sexp)
        ("M-K" . sp-splice-sexp-killing-backward)
        ("C-M-k" . sp-splice-sexp-killing-around)

        ("C--" . sp-copy-sexp)
        ("C-<" . sp-forward-transpose-sexp)
        ("C-c ." . sp-trim-whitespace-of-sexp)
        ("C-;" . comment-dwim)))

(use-package flycheck-clj-kondo
  :after clojure-mode)

(use-package clj-refactor
  :after clojure-mode

  :bind
  (:map clojure-mode-map
        ("C-c t n" . clojure-thread)
        ("C-c t u" . clojure-unwind)
        ("C-c t f" . clojure-thread-first-all)
        ("C-c t l" . clojure-thread-last-all)
        ("C-c c p" . clojure-cycle-privacy)
        ("C-c c i" . clojure-cycle-if)
        ("C-c c w" . clojure-cycle-when)
        ("C-c c o" . clojure-cycle-not)
        ("C-c c t" . cljr-cycle-thread)
        ("C-c J"   . clojure-convert-collection-to-list)
        ("C-c K"   . clojure-convert-collection-to-map)
        ("C-c L"   . clojure-convert-collection-to-vector)
        ("C-c '"   . clojure-convert-collection-to-quoted-list)
        ("C-c #"   . clojure-convert-collection-to-set)
        ("C-c n n"   . clojure-insert-ns-form)
        ("C-c n ."   . clojure-insert-ns-form-at-point)
        ("C-c n u"   . clojure-update-ns)
        ("C-c n s"   . clojure-sort-ns)
        ("C-c n m"   . cljr-require-macro)
        ("C-c n a i" . cljr-add-import-to-ns)
        ("C-c n a l" . cljr-add-missing-libspec)
        ("C-c n a r" . cljr-add-require-to-ns)
        ("C-c n a u" . cljr-add-use-to-ns)
        ("C-c n c"   . cljr-clean-ns)
        ("C-c n f"   . cljr-move-form)
        ("C-c p d" . cljr-add-project-dependency)
        ("C-c p s" . cljr-sort-project-dependencies)
        ("C-c p u" . cljr-update-project-dependencies)
        ("C-c p c" . cljr-project-clean)
        ("C-c b i" . clojure-introduce-let)
        ("C-c b m" . clojure-move-to-let)
        ("C-c b n" . clojure-let-forward-slurp-sexp)
        ("C-c b p" . clojure-let-backward-slurp-sexp)
        ("C-c b e" . cljr-expand-let)
        ("C-c b r" . cljr-remove-let)
        ("C-c -"   . clojure-toggle-ignore)
        ("C-c _"   . clojure-toggle-ignore-surrounding-form)
        ("C-c f p" . cljr-promote-function)
        ("C-c f s" . cljr-change-function-signature)
        ("C-c f a" . clojure-add-arity)
        ("C-c f e" . cljr-create-fn-from-example)
        ("C-c e c" . cljr-extract-constant)
        ("C-c e d" . cljr-extract-def)
        ("C-c e f" . cljr-extract-function)
        ("C-c r n" . clojure-rename-ns-alias)
        ("C-c r f" . cljr-rename-file-or-dir)
        ("C-c r s" . cljr-rename-symbol)
        ("C-c s"   . cljr-find-usages)
        ("C-c d"   . cljr-destructure-keys)
        ("C-c h d" . cljr-hotload-dependency)
        ("C-c i s" . cljr-inline-symbol)
        ("C-c v r" . cljr-stop-referring)
        ("C-c ?"   . cljr-describe-refactoring))

  :config
  (setq cljr-warn-on-eval nil)
  (clj-refactor-mode 1))

(use-package parseedn)

(use-package cider

  :commands
  (cider-jack-in-clj
   cider-connect-clj
   cider-jack-in-cljs
   cider-connect-cljs
   cider-jack-in-clj&cljs
   cider-connect-clj&cljs
   cider-jack-in-with-args
   cider-jack-in-with-profile
   cider-jack-in-dwim)

  :config
  (require 'parseedn)
  (require 'projectile)

  (defun nil-blank-string (s)
    (when  s
      (unless (string-blank-p s)
        s)))

  (defun get-file-content (filePath)
    "Return filePath's file content."
    (with-temp-buffer
      (insert-file-contents filePath)
      (buffer-string)))

  (defun lein-project-clj-filepath ()
    (thread-first
      (projectile-project-root)
      (concat "project.clj")))

  (defun lein-project-clj-content (filepath)
    (let* ((data
            (thread-first
              filepath
              (get-file-content)
              (parseedn-read-str)))

           (name
            (nth 1 data))

           (version
            (nth 2 data))

           (rest-root-level-values
            (seq-drop data 3))

           (result
            (make-hash-table :test 'equal)))

      (puthash :name name result)
      (puthash :version version result)

      (mapc (lambda (pair) (puthash (car pair) (car (cdr pair)) result))
            (seq-partition rest-root-level-values 2))

      result))

  (defun lein-project-clj-profiles (filepath)
    (let* ((profiles
            (thread-last filepath
                         (lein-project-clj-content)
                         (gethash :profiles))))

      (when profiles
        (thread-last profiles
                     (hash-table-keys)
                     (mapcar 'symbol-name)
                     (mapcar (lambda (profile) (substring profile 1)))))))

  (defun my-cider-switch-to-repl-buffer (&optional set-namespace)
    (interactive "P")
    (cider--switch-to-repl-buffer
     (if (and (equal (cider-repl-type-for-buffer) 'cljs)
              (not (cider-repls 'cljs nil)))
         (cider-current-repl 'clj t)
       (cider-current-repl (cider-repl-type-for-buffer) t))
     set-namespace))

  (defun cider-jack-in-with-args (args)
    (interactive "sjack-in repl with args: ")
    (let ((cider-lein-parameters
           args))
      (cider-jack-in nil)))

  (defun cider-jack-in-with-profile (profile)
    (interactive "sjack-in repl with profile: ")
    (cider-jack-in-with-args (concat "with-profile " profile " repl")))

  (defun lein-project-clj-jack-in-profiles ()
    (thread-last
      (lein-project-clj-filepath)
      (lein-project-clj-profiles)
      (seq-filter (lambda (profile) (not (member profile '("dev" "repl" "provided" "uberjar")))))
      (seq-map (lambda (profile) (list profile (concat "+" profile))))
      (apply 'append)))

  (defun cider-jack-in-dwim ()
    (interactive)
    (let* ((profiles
            (lein-project-clj-jack-in-profiles))

           (profile
            (unless (seq-empty-p profiles)
              (nil-blank-string
               (completing-read "jack-in repl with profile: "
                                profiles nil nil nil nil "")))))

      (if profile
          (cider-jack-in-with-profile profile)
        (cider-jack-in nil))))

  (defun cider-jack-in-cljs-dwim ()
    (interactive)
    (let ((cider-lein-parameters
           (concat "with-profile -dev,+dev-desktop repl")))
      (cider-jack-in-cljs nil)))

  (defun cider-eval-dwim ()
    (interactive)
    (save-excursion
      (let ((cursor nil)
            (sexp nil))
        (ignore-errors
          (while (not (string-match-p "^(comment.*" (or sexp "")))
            (setq cursor (point))
            (paredit-backward-up)
            (setq sexp (cider-sexp-at-point))))
        (goto-char cursor)
        (unless (cider-sexp-at-point)
          (paredit-backward-up))
        (cider-eval-sexp-at-point)
        (when (string-match-p "^(\\([^\/]*\/\\)?deftest" (cider-sexp-at-point))
          (cider-test-run-test)))))

  (defun cider-repl-user-system-start ()
    (interactive)
    (cider-interactive-eval
     "(if-let [system-go (resolve 'user/system-go!)]
         (if (nil? (resolve 'user/emacs-system-go-executed))
           (do
             (intern 'user 'emacs-system-go-executed)
             (system-go))
           (user/system-restart!))
         (user/system-restart!))")
    (setq my/user-system-running-p t))

  (defun cider-repl-user-system-restart ()
    (interactive)
    (cider-interactive-eval
     "(user/system-restart!)")
    (setq my/user-system-running-p t))

  ;; (defun cider-repl-user-system-start ()
  ;;   (interactive)
  ;;   (cider-interactive-eval
  ;;    "(user/system-go!)"))

  (defun cider-repl-user-system-stop ()
    (interactive)
    (cider-interactive-eval
     "(user/system-stop!)")
    (setq my/user-system-running-p nil))

  (defun cider-figwheel-main-profiles ()
    (thread-last
      (projectile-project-root)
      (directory-files)
      (seq-filter (lambda (filename) (s-ends-with? ".cljs.edn" filename)))
      (seq-map (lambda (build-filename) (substring build-filename 0 (- 0 (length ".cljs.edn")))))))

  (defun cider-repl-user-fig-init ()
    (interactive)
    (let* ((profiles
            (cider-figwheel-main-profiles))

           (build
            (unless (seq-empty-p profiles)
              (nil-blank-string
               (completing-read "jack-in cljs-repl with build: "
                                profiles nil nil nil nil "")))))

      (cider-interactive-eval
       (if build
           (format
            "(require 'figwheel.main.api)
             (figwheel.main.api/start \"%s\")"
            build)
         "(user/fig-init)"))))

  (defun cider-repl-user-quit-cljs-repl ()
    (interactive)
    (cider-interactive-eval
     ":cljs/quit"))

  (defun cider-repl-user-switch-cljs-repl ()
    (interactive)
    (let* ((profiles
            (cider-figwheel-main-profiles))

           (build
            (unless (seq-empty-p profiles)
              (nil-blank-string
               (completing-read "switch cljs-repl to build: "
                                profiles nil nil nil nil ""))))

           (build
            (or build "dev")))

      (cider-interactive-eval
       ":cljs/quit")

      (sleep-for 3)

      (cider-interactive-eval
       (format
        "(require 'figwheel.main.api)
         (figwheel.main.api/cljs-repl \"%s\")"
        build build))
      ))

  (defun cider-repl-refresh-all ()
    (interactive)
    (cider-interactive-eval
     "(do (require 'clojure.tools.namespace.repl) (clojure.tools.namespace.repl/refresh-all))"))

  (defun cider-repl-switch-to-repl-buffer-and-ns ()
    (interactive)
    (call-interactively 'cider-repl-set-ns)
    (call-interactively 'cider-switch-to-repl-buffer))

  ;; TODO Fix automatic connect of REPLs


  (defvar my/user-system-running-p
    nil
    "Denotes wether user/system is currently running.")

  (defun my/sesman-current-project-session ()
    (sesman-current-session 'CIDER '(project-current)))

  (defun my/test-sesman-current-project-session ()
    (interactive)
    (message "Session: %s" (car (my/sesman-current-project-session))))

  (defun my/sesman-maybe-cider-jack-in-dwim ()
    "Check when inside a project if it has a running sesman cider session; if not, starts cider repl."
    (when (and (projectile-project-p)
               (not (my/sesman-current-project-session)))

      (cider-jack-in-dwim)))

  (defun my/lein-clean ()
    (interactive)
    (shell-command-to-string "lein clean"))

  (defun my/cider-jack-in-with-system-restart ()
    "Jack in with CIDER and run the system restart if necessary."
    (interactive)
    (MY/sesman-maybe-cider-jack-in-dwim)
    (sleep-for 3)
    (unless my/user-system-running-p
      (call-interactively #'cider-repl-user-system-start)))

  (defun my/cider-restart-project ()
    "Restart the current Clojure project by closing REPLs, cleaning, and restarting."
    (interactive)
    (when-let (project-session (my/sesman-current-project-session))
      (progn
        (message "Restarting session: %s" (car project-session))
        (sesman-quit project-session)
        (sleep-for 2)
        (call-interactively #'my/lein-clean)
        (sleep-for 1)
        (call-interactively #'my/cider-jack-in-with-system-restart))))


  (add-hook 'projectile-after-switch-project-hook #'my/cider-jack-in-with-system-restart)

  (setq
   cider-jack-in-default 'clojure-cli
   cider-default-cljs-repl 'figwheel-main

   cider-repl-pop-to-buffer-on-connect t
   cider-show-error-buffer t
   cider-repl-result-prefix ";; => "
   cider-repl-display-help-banner nil
   cider-prompt-for-symbol nil

   nrepl-log-messages t
   nrepl-hide-special-buffers nil

   ;; cider-repl-use-pretty-printing t
   ;; cider-print-fn 'pprint
   cider-print-options '(("length" 15))

   cider-use-overlays t
   cider-overlays-use-font-lock t

   cider-font-lock-dynamically '(macro core function var)
   cider-repl-use-clojure-font-lock t

   cider-error-highlight-face 'error-face)

  (define-clojure-indent
   (defroutes 'defun)
   (println 'defun)
   (pprint 'defun)
   (lazy-seq'defun)
   (routes 'defun)
   (render 'defun)
   (go 'defun)
   (html 'defun)
   (doall 'defun)
   (dosync 'defun)
   (log/spy 'defun)
   ;; (swap! 'defun)
   ;; (reset! 'defun)
   (is 'defun)
   (testing 'defun)
   (element 'defun)
   (match 'defun)
   (->files 'defun)
   (->dir 'defun)

   ;; Custom
   (interval 'defun)
   (routes 'defun)
   (context 'defun)
   (letk 'defun)
   (for-file 'defun)
   (entity 'defun)
   (type 'defun)
   (usage 'defun)
   (<with-transaction 'defun)
   (<with-temporary-db-file 'defun)
   (<with-attached-db 'defun)
   (<with-keep-awake 'defun)
   (<with-resource 'defun)
   (<!with-resource 'defun)
   (some-interact 'defun)

   ;; Compojure
   (GET 'defun)
   (PUT 'defun)
   (POST 'defun)
   (cp/GET 'defun)
   (cp/PUT 'defun)
   (cp/POST 'defun)
   (where 'defun)
   (add-watch 'defun)
   (listen! 'defun)

   ;; Reframe
   (register-handler 'defun)
   (register-sub 'defun)
   (reg-sub-raw 'defun)
   (reg-sub 'defun)
   (reg-event-fx 'defun)
   (reg-event-db 'defun)
   (reg-fx 'defun)
   (reg-cofx 'defun)
   (reg-acofx 'defun)
   (reg-acofx-by-fx 'defun)
   )

  :bind*
  (:map clojure-mode-map
        ("C-c M-j" . nil)

        ("C-c M-j J" . cider-jack-in-dwim)
        ("C-c M-j j" . cider-connect-clj)

        ("C-c C-M-j" . cider-switch-to-repl-buffer)
        ("C-c C-M-S-J" . cider-repl-switch-to-repl-buffer-and-ns)

        ("C-c M-j S" . cider-jack-in-cljs)
        ("C-c M-j s" . cider-connect-cljs)

        ("C-c M-j B" . cider-jack-in-clj&cljs)
        ("C-c M-j b" . cider-connect-clj&cljs)

        :map cider-mode-map
        ("C-M-g" . cider-interrupt)
        ("C-c C-c" . cider-eval-dwim)
        ("C-M-x" . cider-eval-last-sexp)
        ("C-M-S-X" . cider-insert-last-sexp-in-repl)
        ("C-c M-k" . cider-load-buffer)

        ("C-h h" . cider-doc)
        ("C-h H" . cider-javadoc)

        ("M-f d" . cider-find-var)
        ("M-f D" . cider-grimoire)
        ("M-f r" . cider-apropos)
        ("M-f f" . cider-find-resource)
        ("M-f n" . cider-find-ns)
        ("M-F" . cider-pop-back)

        ("C-c M-j" . nil)
        ("C-c C-M-j" . my-cider-switch-to-repl-buffer)
        ("C-c C-M-S-J" . cider-repl-switch-to-repl-buffer-and-ns)

        ("C-c M-q" . cider-quit)
        ("C-c M-r" . cider-restart)
        ("C-c M-o" . cider-find-and-clear-repl-output)
        ("C-c M-c" . cider-completion-flush-caches)

        ("M-u" . nil)
        ("M-u s" . cider-repl-user-system-start)
        ("M-u R" . my/cider-restart-project)
        ("M-u r" . cider-repl-user-system-restart)
        ("M-u S" . cider-repl-user-system-stop)
        ("M-u f" . cider-repl-user-fig-init)
        ("M-u w" . cider-repl-user-switch-cljs-repl)
        ("M-u W" . cider-repl-user-quit-cljs-repl)

        :map cider-repl-mode-map
        ("C-<left>" . sp-backward-sexp)
        ("M-<left>" . sp-forward-barf-sexp)
        ("C-M-<left>" . sp-backward-barf-sexp)

        ("C-<right>" . sp-forward-sexp)
        ("M-<right>" . sp-forward-slurp-sexp)
        ("C-M-<right>" . sp-backward-slurp-sexp)

        ("C-<up>" . sp-backward-up-sexp)
        ("M-<up>" . sp-convolute-sexp)

        ("C-<down>" . sp-down-sexp)
        ("M-<down>" . sp-raise-sexp)

        ("C-M-<up>" . sp-backward-transpose-sexp)
        ("C-M-<down>" . sp-forward-transpose-sexp)

        ("M-\"" . sp-wrap-doublequote)
        ("C-(" . sp-wrap-round)
        ("M-[" . sp-wrap-square)
        ("M-{" . sp-wrap-curly)

        ("C-M-(" . sp-rewrap-sexp)

        ("C-k" . sp-kill-sexp)
        ("C-S-K" . sp-unwrap-sexp)

        ("M-k" . sp-splice-sexp-killing-forward)
        ("M-K" . sp-splice-sexp-killing-backward)
        ("C-M-k" . sp-splice-sexp-killing-around)

        ("C-;" . sp-comment-dwim)

        ("C-<left>" . sp-backward-sexp)
        ("M-<left>" . sp-forward-barf-sexp)
        ("C-M-<left>" . sp-backward-barf-sexp)

        ("C-<right>" . sp-forward-sexp)
        ("M-<right>" . sp-forward-slurp-sexp)
        ("C-M-<right>" . sp-backward-slurp-sexp)

        ("C-<up>" . sp-backward-up-sexp)
        ("M-<up>" . sp-convolute-sexp)

        ("C-<down>" . sp-down-sexp)
        ("M-<down>" . sp-raise-sexp)

        ("C-M-<up>" . sp-backward-transpose-sexp)
        ("C-M-<down>" . sp-forward-transpose-sexp)

        ("M-\"" . sp-wrap-doublequote)
        ("C-(" . sp-wrap-round)
        ("M-[" . sp-wrap-square)
        ("M-{" . sp-wrap-curly)

        ("C-M-(" . sp-rewrap-sexp)

        ("C-k" . sp-kill-sexp)
        ("C-S-K" . sp-unwrap-sexp)

        ("M-k" . sp-splice-sexp-killing-forward)
        ("M-K" . sp-splice-sexp-killing-backward)
        ("C-M-k" . sp-splice-sexp-killing-around)

        ("C-;" . sp-comment-dwim)

        ("C-c M-j" . nil)
        ("C-c C-M-j" . cider-switch-to-last-clojure-buffer)

        ("RET" . cider-repl-return)
        ("C-<return>" . cider-repl-newline-and-indent)
        ("C-S-<return>" . cider-repl-newline-and-indent)

        ;; ("M-p" . history)
        ;; ("M-n" . history)

        ("M-P" . cider-repl-history)

        ("C-M-g" . cider-interrupt)

        ("C-h h" . cider-doc)
        ("C-h H" . cider-javadoc)

        ("M-j d" . cider-find-var)
        ("M-j D" . cider-grimoire)
        ("M-j r" . cider-apropos)
        ("M-j f" . cider-find-resource)
        ("M-j n" . cider-find-ns)
        ("M-J" . cider-pop-back)

        ("M-u" . nil)
        ("M-u s" . cider-repl-user-system-start)
        ("M-u S" . cider-repl-user-system-stop)
        ("M-u f" . cider-repl-user-fig-init)
        ("M-u w" . cider-repl-user-switch-cljs-repl)
        ("M-u W" . cider-repl-user-quit-cljs-repl)

        ("C-c M-q" . cider-quit)
        ("C-c M-r" . cider-restart)
        ("C-c M-o" . cider-find-and-clear-repl-output)

        :map cider-repl-history-mode-map
        ("M-p" . cider-repl-history-backward)
        ("M-n" . cider-repl-history-forward)

        ("C-s" . cider-repl-history-occur)
        ("C-r" . nil)

        ("C-_" . cider-repl-history-undo-other-window)))

;;TODO: Switch to clojure-ts
;; (use-package clojure-ts
;;   :ensure t
;;   :hook
;;   ((cider-mode        .  clojure-ts-mode-hook)
;;    (smartparens-mode  .  clojure-ts-mode-hook)
;;    (clj-refactor-mode .  clojure-ts-mode-hook))

;;   :config
;;   (setq clojure-ts-mode-map clojure-mode-map))

(use-package
  vega-view

  :config
  (setq vega-view-prefer-png t))


(provide 'lang--clojure)
