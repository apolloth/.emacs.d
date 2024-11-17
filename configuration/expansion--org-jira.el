
(require 'org-jira)

(defun my/org-jira--render-issue (group-by Issue)
  "Render single ISSUE."
  ;;  (org-jira-log "Rendering issue from issue list")
  ;;  (org-jira-log (org-jira-sdk-dump Issue))
  (with-slots (filename proj-key issue-id summary status priority headline id) Issue
    (let (p)
      (with-current-buffer (org-jira--get-project-buffer Issue)
        (org-jira-freeze-ui
         (org-jira-maybe-activate-mode)
         (org-jira--maybe-render-top-heading group-by)
         (setq p (org-find-entry-with-id issue-id))
         (save-restriction
           (if (and p (>= p (point-min))
                    (<= p (point-max)))
               (progn
                 (goto-char p)
                 (forward-thing 'whitespace)
                 (org-jira-kill-line))
             (goto-char (point-max))
             (unless (looking-at "^")
               (insert "\n"))
             (insert "** "))
           (org-jira-insert
            (concat (org-jira-get-org-keyword-from-status status)
                    " "
                    (org-jira-get-org-priority-cookie-from-issue priority)
                    headline))
           (save-excursion
             (unless (search-forward "\n" (point-max) 1)
               (insert "\n")))
           (org-narrow-to-subtree)
           (save-excursion
             (org-back-to-heading t)
             (org-set-tags-to (replace-regexp-in-string "-" "_" issue-id)))
           (org-jira-entry-put (point) "assignee" (or (slot-value Issue 'assignee) "Unassigned"))
           (mapc (lambda (entry)
                   (let ((val (slot-value Issue entry)))
                     (when (and val (not (string= val "")))
                       (org-jira-entry-put (point) (symbol-name entry) val))))
                 '(filename reporter type type-id priority labels resolution status components created updated sprint))

           (org-jira-entry-put (point) "ID" issue-id)
           (org-jira-entry-put (point) "CUSTOM_ID" issue-id)

           ;; Insert the duedate as a deadline if it exists
           (when org-jira-deadline-duedate-sync-p
             (let ((duedate (oref Issue duedate)))
               (when (> (length duedate) 0)
                 (org-deadline nil duedate))))

           (mapc
            (lambda (heading-entry)
              (ensure-on-issue-id-with-filename issue-id filename
                                                (let* ((entry-heading
                                                        (concat (symbol-name heading-entry)
                                                                (format ": [[%s][%s]]"
                                                                        (concat jiralib-url "/browse/" issue-id) issue-id))))
                                                  (setq p (org-find-exact-headline-in-buffer entry-heading))
                                                  (if (and p (>= p (point-min))
                                                           (<= p (point-max)))
                                                      (progn
                                                        (goto-char p)
                                                        (org-narrow-to-subtree)
                                                        (goto-char (point-min))
                                                        (forward-line 1)
                                                        (delete-region (point) (point-max)))
                                                    (if (org-goto-first-child)
                                                        (org-insert-heading)
                                                      (goto-char (point-max))
                                                      (open-line 1)
                                                      (org-insert-subheading t))
                                                    (org-jira-insert entry-heading "\n"))

                                                  ;;  Insert 2 spaces of indentation so Jira markup won't cause org-markup
                                                  (org-jira-insert
                                                   (replace-regexp-in-string
                                                    "^" "  "
                                                    (format "%s" (slot-value Issue heading-entry)))))))
            '(description))

           (when org-jira-download-comments
             (org-jira-update-comments-for-issue Issue)

             ;; FIXME: Re-enable when attachments are not erroring.
             ;;(org-jira-update-attachments-for-current-issue)
             )

           ;; only sync worklog clocks when the user sets it to be so.
           (when org-jira-worklog-sync-p
             (org-jira-update-worklogs-for-issue issue-id filename))))))))

(defun my/org-jira--render-issues-from-issue-list (Issues group-by)
  "Add the issues from ISSUES list into the org file(s).

ISSUES is a list of `org-jira-sdk-issue' records."
  ;; FIXME: Some type of loading error - the first async callback does not know about
  ;; the issues existing as a class, so we may need to instantiate here if we have none.
  (when (eq 0 (->> Issues (cl-remove-if-not #'org-jira-sdk-isa-issue?) length))
    (setq Issues (org-jira-sdk-create-issues-from-data-list Issues)))

  ;; First off, we never ever want to run on non-issues, so check our types early.
  (setq Issues (cl-remove-if-not #'org-jira-sdk-isa-issue? Issues))
  (org-jira-log (format "About to render %d issues." (length Issues)))

  ;; If we have any left, we map over them.
  (mapc #'(apply-partially my/org-jira--render-issue group-by) Issues)

  ;; Prior text: "Oh, are you the culprit?" - Not sure if this caused an issue at some point.
  ;; We want to ensure we fix broken org narrowing though, by doing org-show-all and then org-cycle.
  (switch-to-buffer (org-jira--get-project-buffer (-last-item Issues)))
  (org-show-all)
  (org-cycle))

(defun my/org-jira-get-issues (issues &optional group-by)
  "Get list of ISSUES into an org buffer.

Default is get unfinished issues assigned to you, but you can
customize jql with a prefix argument.
See`org-jira-get-issue-list'"
  ;; If the user doesn't provide a default, async call to build an issue list
  ;; from the JQL style query
  (interactive
   (org-jira-get-issue-list org-jira-get-issue-list-callback))
  (org-jira-log "Fetching issues...")
  (when (> (length issues) 0)
    (my/org-jira--render-issues-from-issue-list issues (or group-by 'proj-key))))

(defun my/org-jira-get-issues-from-custom-jql-callback (filename list group-by)
  "Generate a function that we can iterate over FILENAME and LIST with when callback finishes."
  (cl-function
   (lambda (&key data &allow-other-keys)
     "Callback for async, DATA is the response from the request call.

Will send a list of org-jira-sdk-issue objects to the list printer."
     (org-jira-log "Received data for org-jira-get-issues-from-custom-jql-callback.")
     (--> data
          (org-jira-sdk-path it '(issues))
          (append it nil)     ; convert the conses into a proper list.
          (org-jira-sdk-create-issues-from-data-list-with-filename filename it)
          (lambda (issues)
            (my/org-jira-get-issues issues group-by)))
     (setq org-jira-proj-key-override nil)
     (let ((next (rest list)))
       (when next
         (my/org-jira-get-issues-from-custom-jql next))))))

(defun my/org-jira-get-issues-from-custom-jql (&optional jql-list)
  "Get JQL-LIST list of issues from a custom JQL and PROJ-KEY.

The PROJ-KEY will act as the file name, while the JQL will be any
valid JQL to populate a file to store PROJ-KEY results in.

Please note that this is *not* concurrent or race condition
proof.  If you try to run multiple calls to this function, it
will mangle things badly, as they rely on globals DEFAULT-JQL and
ORG-JIRA-PROJ-KEY-OVERRIDE being set before and after running."
  (interactive)
  (let* ((jl (or jql-list org-jira-custom-jqls))
         (uno (car jl))
         (filename (cl-getf uno :filename))
         (limit (cl-getf uno :limit))
         (group-by (cl-getf uno :group-by))
         (jql (replace-regexp-in-string "[\n]" " " (cl-getf uno :jql))))
    (setq org-jira-proj-key-override filename)
    (jiralib-do-jql-search jql limit (my/org-jira-get-issues-from-custom-jql-callback filename jl group-by))))


(provide 'expansion--org-jira)

;;; expansion--org-jira.el ends here
