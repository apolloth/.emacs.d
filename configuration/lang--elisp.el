(use-package
  elisp-mode
  ;; :after el-smartparens
  :ensure nil

  :mode
  ("\\.el\\'" . emacs-lisp-mode)

  :bind*
  (:map emacs-lisp-mode-map
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
        ("C-c J" . sp-rewrap-sexp)

        ("C-k" . sp-kill-sexp)
        ("C-S-K" . sp-unwrap-sexp)

        ("M-k" . sp-splice-sexp)
        ("M-K" . sp-splice-sexp-killing-backward)
        ("C-M-k" . sp-splice-sexp-killing-around)

        ("C--" . sp-copy-sexp)
        ("C-<" . sp-forward-transpose-sexp)
        ("C-c ." . sp-trim-whitespace-of-sexp)
        ("C-;" . comment-dwim)

        ("C-c C-c" . eval-defun)
        ("C-c C-k" . eval-buffer)
        ("C-M-x" . eval-last-sexp))

  :config
  (with-eval-after-load 'smartparens

    (sp-with-modes 'emacs-lisp-mode
      (sp-local-pair "'" "'"
                     :when '(sp-in-string-p
                             sp-in-comment-p))
      (sp-local-pair "`" "`"
                     :when '(sp-in-string-p
                             sp-in-comment-p)))))

(provide 'lang--elisp)
