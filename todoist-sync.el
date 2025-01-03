;;; todoist-sync.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Felix Mühlenberend
;;
;; Author: Felix Mühlenberend <felix.muehlenberend@mailbox.org>
;; Maintainer: Felix Mühlenberend <felix.muehlenberend@mailbox.org>
;; Created: October 08, 2023
;; Modified: October 08, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/felix/todoist-sync
;;
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


;; TODO: support for creating recurring tasks
;; TODO: check for conflicts in the deadline and allow user to resolve them
;; TODO: give option to remove/refile headlines once completed
;; TODO: implement quick add to use todoists due date parsing

;; NOTES:
;; if we set an item_id in the request we should only get info about that item
;; Then we can also set the all_data flag to false to only get item info and not (ancestors, projects, section and notes)

;; Overview:
;; Syncing happens according to the following scheme:
;; 1. Find all org-todos in the current scope and collect their positions and potentially their sync-tokens
;;   - (agenda files, current buffer, region, single heading)
;; 2. For every unique sync token get the todoist changes
;;   - if all todos were synced at the same time there is only one
;;   - may be none if no todo has been synced previously
;; 3. Revisit all todos with the changes from todoist possibly updating either org or todoist or declaring a conflict

(require 'json)
(require 'request)
(require 'org)
(require 'org-id)

(defvar todoist-sync-token nil
  "The Todoist API token.")
(defvar todoist-sync-agenda-project "agenda"
  "The name of the project to sync with the agenda.")
;; Is also used as a marker for synced items
(defvar todoist-sync-org-prop-id "TODOIST_SYNCED")
(defvar todoist-sync-org-prop-synctoken "TODOIST_SYNC_TOKEN")
(defvar todoist-sync-org-prop-hash "TODOIST_BODY_HASH")

(defvar todoist-sync-todoist-org-file nil)

(defvar todoist-sync--api-url "https://api.todoist.com/sync/v9/sync")
(defvar todoist-sync--items-api-url "https://api.todoist.com/sync/v9/items/get")

;; This gets cached before the first sync
(defvar todoist-sync--agenda-uuid nil "The UUID of the agenda project.")

(defvar todoist-sync-fold-open-descriptions nil "Whether to fold open descriptions in the org file. Requires Emacs version > 29.1.")

(defvar todoist-sync-log-level
  'info
  "The log level for todoist-sync. Can be 'info, 'error, 'debug.")

(defvar todoist-sync-log-buffer
  "*todoist-sync-log*"
  "The buffer to log todoist-sync debug messages to.")

(defun todoist-sync--debug-msg (msg &rest args)
  (when (equal todoist-sync-log-level 'debug)
    ;; (message "[todoist-sync-dbg] %s" msg)
    (with-current-buffer (get-buffer-create todoist-sync-log-buffer)
      (goto-char (point-max))
      (insert (concat "[todoist-sync-dbg] " (apply 'format msg args) "\n")))))

(defun todoist-sync--info-msg (msg &rest args)
  (when (or (equal todoist-sync-log-level 'debug)
            (equal todoist-sync-log-level 'info))
    (message msg args)
    (with-current-buffer (get-buffer-create todoist-sync-log-buffer)
      (goto-char (point-max))
      (insert (concat "[todoist-sync-info] " (apply 'format msg args) "\n")))))

(defun todoist-sync--error-msg (msg &rest args)
  (when (or (equal todoist-sync-log-level 'debug)
            (equal todoist-sync-log-level 'info)
            (equal todoist-sync-log-level 'error))
    ;; (message (concat "[todoist-sync error] " msg) args)
    (with-current-buffer (get-buffer-create todoist-sync-log-buffer)
      (goto-char (point-max))
      (insert (concat "[todoist-sync-error] " (apply 'format msg args) "\n")))))

;; ================== API ==================

;; Data gathering functions. These simply call the API and pass a
;; "resources_type". Changes in items of that type since the last
;; sync are returned.

(cl-defun todoist-sync--make-request (&key sync-token resource-types commands callback)
  "Make a request to the Todoist API."
  (let ((sync-token (or sync-token "*"))
        (resource-types (or resource-types []))
        (commands (or commands []))
        (callback (or callback (lambda (_) nil))))
    (request
      todoist-sync--api-url
      :headers `(("Authorization" . ,(concat "Bearer " todoist-sync-token)))
      :params `(("sync_token" . ,sync-token)
                ("resource_types" . ,(json-encode resource-types))
                ("commands" . ,(json-encode commands)))
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (funcall callback data)))
      :error (cl-function
              (lambda (&key data &allow-other-keys)
                (todoist-sync--error-msg "RequestError:\n%S" data))))))
(lambda (&key data &allow-other-keys))
(defun todoist-sync-get-projects (callback)
  "Get all projects from the Todoist API. Ignores sync token."
  (todoist-sync--make-request
   :sync-token "*"
   :resource-types '(projects)
   :callback (lambda (data)
               (funcall callback (cdr (assoc 'projects data))))))

(defun todoist-sync-get-items (callback &optional sync-token)
  (todoist-sync--make-request
   :sync-token sync-token
   :resource-types '(items)
   :callback callback))

(defun todoist-sync-get-item (callback item-id)
  (request
    todoist-sync--items-api-url
    :headers `(("Authorization" . ,(concat "Bearer " todoist-sync-token)))
    :data `(("item_id" . ,item-id))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback (alist-get 'item data))))
    :error (cl-function
            (lambda (&key data &allow-other-keys)
              (todoist-sync--error-msg "RequestError:\n%S" data)))))

(defun todoist-sync--filter-by-project (project-id items)
  "Filter the list of items by the project ID."
  (cl-remove-if-not (lambda (item)
                      (equal (alist-get 'project_id item) project-id))
                    items))

;; Data pushing functions. (don't set any resource_type otherwise the request will return changes as well)
;; In general commands are concatenated in a command stack and then executed
;; The command stack also holds callbacks

(defun todoist-sync--get-empty-command-stack ()
  (list (cons 'commands nil)
        (cons 'callbacks nil)))

(defun todoist-sync--add-command (command callback command-stack)
  (setf (cdr (assoc 'commands command-stack))
        (cons command (cdr (assoc 'commands command-stack))))
  (setf (cdr (assoc 'callbacks command-stack))
        (cons callback (cdr (assoc 'callbacks command-stack)))))

(defun todoist-sync--add-item-command (temp_id args)
  `((type . "item_add")
    (temp_id . ,temp_id)
    (uuid . ,(todoist-sync--generate-uuid))
    (args . ,args)))

(defun todoist-sync--item-update-command (args)
  "ARGS must include id field"
  `((type . "item_update")
    (uuid . ,(todoist-sync--generate-uuid))
    (args . ,args)))

(defun todoist-sync--item-complete-command (id)
  ;; Item close should work with recurring tasks
  `((type . "item_close")
    (uuid . ,(todoist-sync--generate-uuid))
    (args . ((id . ,id)))))

(defun todoist-sync-add-item (args callback)
  (todoist-sync--make-request
   :commands (list (todoist-sync--add-item-command "TMP" args))
   :callback callback))

(defun todoist-sync--update-item (args callback)
  (todoist-sync--make-request
   :commands (list (todoist-sync--item-update-command args))
   :callback callback))

(defun todoist-sync--complete-item (id callback)
  (todoist-sync--make-request
   :commands (list (todoist-sync--item-complete-command id))
   :callback callback))

;; ====== Multiple API ==============

(defun todoist-sync--multiple-add-command (command callback data)
  (setf (cdr (assoc 'commands data))
        (cons command (cdr (assoc 'commands data))))
  (setf (cdr (assoc 'callbacks data))
        (cons callback (cdr (assoc 'callbacks data)))))

(defun todoist-sync--request-commands (command-stack &optional done-callback)
  (let ((commands (cdr (assoc 'commands command-stack)))
        (callbacks (cdr (assoc 'callbacks command-stack))))
    ;; fifo
    (setq commands (reverse commands))
    (setq callbacks (reverse callbacks))
    (todoist-sync--make-request
     :commands commands
     :callback (lambda (data)
                 (dolist (callback callbacks)
                   (funcall callback data))
                 (when done-callback
                   (funcall done-callback))))))

;; ====== Org mode integration ======

;; TODO: combine these functions
(defun todoist-sync--org-visit-agenda-todos (callback)
  "Visit all TODO, etc. entries from all agenda files and calls the
  callback with the marker placed on the heading."
  (let* ((today (org-today))
         (date (calendar-gregorian-from-absolute today))
         (files (org-agenda-files nil 'ifmode))
         (rtnall nil))
    (while (setq file (pop files))
      (org-check-agenda-file file)
      (let ((old-regexp org-not-done-regexp))
        (setq org-not-done-regexp org-todo-regexp)
        (let ((rtn (org-agenda-get-day-entries file date :todo)))
          (setq rtnall (append rtnall rtn)))
        (setq org-not-done-regexp old-regexp)))
    (dolist (entry rtnall)
      (let ((marker (get-text-property 0 'org-hd-marker entry)))
        (with-current-buffer (marker-buffer marker)
          (goto-char marker)
          (funcall callback))))))

;; TODO: do for buffer as well?
(defun todoist-sync--org-visit-todos-in-file (file callback)
  "Visit all TODO, etc. entries from the current buffer and calls the
  callback with the marker placed on the heading."
  (let* ((today (org-today))
         (date (calendar-gregorian-from-absolute today))
         (rtnall nil))
    (let ((old-regexp org-not-done-regexp))
      (setq org-not-done-regexp org-todo-regexp)
      (let ((rtn (org-agenda-get-day-entries file date :todo)))
        (setq rtnall (append rtnall rtn)))
      (setq org-not-done-regexp old-regexp))
    (dolist (entry rtnall)
      (let ((marker (get-text-property 0 'org-hd-marker entry)))
        (save-excursion
          (with-current-buffer (marker-buffer marker)
            (goto-char marker)
            (funcall callback)))))))

(defun todoist-sync--org-visit-todos-in-buffer
    (buffer callback &optional start end)
  "Visit all TODO, etc. entries from the current buffer and calls the CALLBACK.

If START and END are given, only visit the region between START and END. "
  (save-excursion
    (with-current-buffer buffer
      (let ((case-fold-search nil))
        (goto-char (or start (point-min)))
        (while (re-search-forward org-todo-line-regexp (or end (point-max)) t)
          (when (match-string 2)
            (funcall callback)))))))

(defun todoist-sync--get-first-synced-parent ()
  "Get the first parent heading of the heading at point that has a todoist id."
  (save-excursion
    (when (org-up-heading-safe)
      (let ((parent-synced-id (org-entry-get (point) todoist-sync-org-prop-id)))
        (if parent-synced-id
            parent-synced-id
          (todoist-sync--get-first-synced-parent))))))

;; ================== Write todoist items to org file ==================
(defun todoist-sync--insert-todoist-file-explanation ()
  "Inserts a comment explaining the file format."
  ;; TODO: Finish header
  (insert "# This file is automatically generated by todoist-sync.el\n\n"))

(defun todoist-sync--build-todo-tree (parent-id items)
  "Recursively build the todo tree starting from PARENT-ID."
  ;; TODO make more efficient
  (setq items (sort items (lambda (a b)
                            (> (alist-get 'child_order a)
                               (alist-get 'child_order b)))))
  (let (todos)
    (dotimes (i (length items))
      (let ((item (elt items i)))
        (when (equal (alist-get 'parent_id item) parent-id)
          (let* ((child-todos (todoist-sync--build-todo-tree (alist-get 'id item) items))
                 (todo (list (cons 'data item)
                             (cons 'todos child-todos))))
            (push (cons (alist-get 'id item) todo) todos)))))
    (nreverse todos)))

(defun todoist-sync--hierarchicalize-projects (projects items)
  ;; sort projects by child_order property
  (setq projects (sort projects (lambda (a b)
                                  (> (alist-get 'child_order a)
                                     (alist-get 'child_order b)))))
  (let (result)
    (dotimes (i (length projects))
      (let* ((project (elt projects i))
             (project-id (alist-get 'id project))
             (project-items (todoist-sync--filter-by-project project-id items))
             (project-items-hierarchical (todoist-sync--build-todo-tree nil project-items)))
        (push `(,project-id .
                ((data . ,project)
                 (todos . ,project-items-hierarchical)))
              result)))
    result))

(defun todoist-sync--write-todo-to-org (todo sync-token level)
  "Write a single TODO item to the current buffer, indented to LEVEL."
  (let ((data (cdr (assoc 'data todo)))
        (sub-todos (cdr (assoc 'todos todo))))
    (insert (make-string level ?*) " TODO " (alist-get 'content data) "\n")
    (org-entry-put (point) todoist-sync-org-prop-id (alist-get 'id data))
    (org-entry-put (point) todoist-sync-org-prop-synctoken sync-token)
    (when (alist-get 'due data)
      (org-deadline nil (alist-get 'date (alist-get 'due data))))
    (when (alist-get 'description data)
      (insert (alist-get 'description data) "\n"))
    (let* ((heading (org-get-heading t t t t))
           (body (substring-no-properties (todoist-sync--get-body-without-subtree)))
           (hash (todoist-sync--hash-org-element body heading)))
      (org-entry-put (point) todoist-sync-org-prop-hash hash))
    (dolist (child-todo sub-todos)
      (todoist-sync--write-todo-to-org child-todo sync-token (+ 1 level)))))

(defun todoist-sync--write-project-to-org (project sync-token)
  "Write a single PROJECT and its TODOs to the current buffer."
  (let ((data (cdr (assoc 'data project)))
        (todos (cdr (assoc 'todos project))))
    (insert "* " (alist-get 'name data) "\n")
    (dolist (todo todos)
      (todoist-sync--write-todo-to-org todo sync-token 2))))

(defun todoist-sync--write-hierarchical-data-to-org-file (hierarchical-data sync-token)
  (dolist (project hierarchical-data)
    (todoist-sync--write-project-to-org project sync-token)))

(defun todoist-sync--insert-todos-into-file (&optional done-callback)
  "Inserts todos into file."
                                        ;: TODO: using a separate sync token tracking keep an internal representation
  ;; and modify it as we go along
  (todoist-sync-get-projects
   ;; HACK to do full sync
   (lambda (projects)
     (todoist-sync-get-items
      (lambda (data)
        (let* ((items (alist-get 'items data))
               (hierarchical-data (todoist-sync--hierarchicalize-projects projects items))
               (sync-token (alist-get 'sync_token data)))
          (todoist-sync--write-hierarchical-data-to-org-file hierarchical-data sync-token))
        (funcall done-callback))))))

(defun todoist-sync-write-to-file (&optional done-callback)
  "Writes todoist-information to org file."
  (interactive)
  (when (not todoist-sync-todoist-org-file)
    (setq todoist-sync-todoist-org-file (read-file-name "Todoist org file: ")))
  (find-file todoist-sync-todoist-org-file)
  (erase-buffer)
  (insert "#+TITLE: Todoist\n")
  (todoist-sync--insert-todoist-file-explanation)
  (todoist-sync--insert-todos-into-file
   (lambda ()
     ;; if emacs version is > 29.1
     (when (and (not todoist-sync-fold-open-descriptions) (fboundp 'org-cycle-content))
       (org-cycle-content -1))
     (save-buffer)
     ;; goto first heading
     (goto-char (point-min))
     (re-search-forward "^\\*")
     (funcall done-callback))))

;; ================== Sync Org Files ==================

(defun todoist-sync--get-body-without-subtree ()
  "Get the content between the current heading and the next heading, excluding the heading itself and subheadings."
  (save-excursion
    (org-back-to-heading t)
    (forward-line 1)
    (let ((start (point))
          (end (progn
                 (outline-next-heading)
                 (point))))
      (buffer-substring-no-properties start end))))

;; TODO: do this properly
;; TODO: make sure the description is not too long
(defun todoist-sync--clean-org-text (text)
  "Remove the PROPERTIES drawer and deadline from the org text.

DO NOT USE FOR HASH GENERATION! (Since the deadline is removed)"
  (let* ((text (replace-regexp-in-string ":PROPERTIES:\\(.*\n\\)*?:END:" "" text))
         (text (replace-regexp-in-string "DEADLINE:.*" "" text))
         (text (replace-regexp-in-string "\\`[ \t\n]*" "" text))
         (text (replace-regexp-in-string "[ \t\n]*\\'" "" text)))
    text))

(defun todoist-sync--hash-org-element (body-text heading-text)
  "Hashsed everything in the heading but the properties. Importantly uses the deadline in the hash as well"
  (let ((text-no-props (replace-regexp-in-string ":PROPERTIES:\\(.*\n\\)*?:END:" "" body-text)))
    (md5 (concat text-no-props heading-text))))

;; TODOIST somehow assumes that the task should be at two
;; (defun todoist-sync--org-to-todoist-date (org-time-str)
;;   "Convert an org date string to a todoist date string."
;;   (format-time-string "%Y-%m-%dT%H:%M:%S.000000Z" (org-time-string-to-time org-time-str)))

(defun todoist-sync--org-to-todoist-date (org-time-str)
  "Converts <2023-10-09 Sun> to 2023-10-09"
  (replace-regexp-in-string "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\).*>" "\\1" org-time-str))

(defun todoist-sync--todoist-date-for-at-point ()
  (let* ((org-due-str (org-entry-get (point) "DEADLINE"))
         (todoist-due-string
          (when org-due-str (todoist-sync--org-to-todoist-date org-due-str))))
    (when todoist-due-string `((string . ,todoist-due-string)))))

(defun todoist-sync--generate-uuid ()
  "Generate a random UUID."
  (replace-regexp-in-string " " "-" (downcase (md5 (format "%s%s%s" (random) (current-time) user-mail-address)))))

(defun todoist-sync--generate-temp_id ()
  "Generate a random UUID prefixed with tmp_."
  (concat "tmp_" (todoist-sync--generate-uuid)))

(defun todoist-sync--extract-agenda-project-uuid (projects)
  "Extract the UUID of the agenda project from the list of projects."
  (cdr (assoc 'id (car (seq-filter
                        (lambda (project)
                          (string= (cdr (assoc 'name project)) todoist-sync-agenda-project))
                        projects)))))

;; Used in all entry points to the package.
(defun todoist-sync--ensure-agenda-uuid (callback)
  "Makes sure that the agenda project UUID is cached.
  Then calls the callback with the UUID as argument."
  (if todoist-sync--agenda-uuid
      (funcall callback)
    (todoist-sync-get-projects
     (lambda (projects)
       (let ((agenda-project-uuid (todoist-sync--extract-agenda-project-uuid projects)))
         (setq todoist-sync--agenda-uuid agenda-project-uuid)
         (funcall callback))))))

(defun todoist-sync--remove-todoist-properties (marker)
  "Remove all todoist properties from the org entry at MARKER."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (org-entry-delete marker todoist-sync-org-prop-id)
      (org-entry-delete marker todoist-sync-org-prop-synctoken)
      (org-entry-delete marker todoist-sync-org-prop-hash))))

;; Main function for syncing a single heading
(defun todoist-sync--visit-org-heading (marker updated-data command-stack)
  "Syncs the org heading at point. Adds the commands for todoist to COMMAND-STACK.
UPDATED-ITEMS is a list of items that have been updated in todoist
since the last sync (with the sync id of the heading)."
  ;; goto buffer
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (todoist-sync--debug-msg "Visiting heading: %s" (org-get-heading t t t t))
      (let* ((synced-id (org-entry-get (point) todoist-sync-org-prop-id))
             (todoist-changes (cdr (assoc synced-id
                                          (alist-get 'items updated-data))))
             (new-sync-token (alist-get 'sync_token updated-data))
             (todoist-is-done (alist-get 'completed_at todoist-changes))
             (org-is-done (org-entry-is-done-p))
             (heading (org-get-heading t t t t))
             (body (substring-no-properties (todoist-sync--get-body-without-subtree)))
             (description (todoist-sync--clean-org-text body))
             (hash (todoist-sync--hash-org-element body heading))
             (hash-old (org-entry-get (point) todoist-sync-org-prop-hash))
             (org-has-changes (not (string= hash hash-old)))
             (due (todoist-sync--todoist-date-for-at-point))
             (conflict (and todoist-changes org-has-changes)))
        ;; if hash is nil but we have sync ID this should not happen
        (when (and synced-id (not hash-old))
          (todoist-sync--error-msg "Hash is nil but we have a sync ID for item %s" synced-id))

        (when todoist-changes
          (todoist-sync--debug-msg "todoist-changes: %s" todoist-changes))
        (when org-has-changes
          (todoist-sync--debug-msg "org-has-changes: %s" org-has-changes))
        (when conflict
          (todoist-sync--info-msg "Conflict for item %s" synced-id))
        (cond
         ;; newly done in orgmode
         ((and synced-id org-is-done (not todoist-is-done))
          (todoist-sync--debug-msg "Completing item %s in todoist." synced-id)
          (todoist-sync--add-command
           (todoist-sync--item-complete-command synced-id)
           (lambda (_)
             (unless conflict
               (todoist-sync--remove-todoist-properties marker))
             (todoist-sync--info-msg "Completed item %s in todoist." synced-id))
           command-stack))
         ;; newly done in todoist
         ((and synced-id todoist-is-done (not org-is-done))
          (todoist-sync--debug-msg "Completing item %s in org-mode." synced-id)
          (org-todo 'done)
          (unless conflict
            (todoist-sync--remove-todoist-properties marker))
          (todoist-sync--info-msg "Completed item %s in org-mode." synced-id))
         ;; push org changes to todoist
         ((and synced-id (not conflict) org-has-changes)
          (todoist-sync--debug-msg "Updating item %s in todoist." synced-id)
          (todoist-sync--add-command
           (todoist-sync--item-update-command
            `((id . ,synced-id)
              (content . ,heading)
              (description . ,description)
              (due . ,due)))
           (lambda (data)
             (org-entry-put marker todoist-sync-org-prop-synctoken
                            (alist-get 'sync_token data))
             (org-entry-put marker todoist-sync-org-prop-hash hash)
             (todoist-sync--info-msg "Successfully updated item %s" synced-id))
           command-stack))
         ((and synced-id (not conflict) todoist-changes)
          (todoist-sync--debug-msg "Updating item %s in org-mode." synced-id)
          ;; TODO: incorporate changes from todoist
          (let* ((todoist-heading (alist-get 'content todoist-changes))
                 (todoist-description (alist-get 'description todoist-changes))
                 (todoist-due (alist-get 'date (alist-get 'due todoist-changes))))
            (if todoist-due
                (org--deadline-or-schedule nil 'deadline todoist-due)
              ;; removes deadline:
              (org--deadline-or-schedule `(4) 'deadline nil))
            (org-edit-headline todoist-heading)
            ;; edit description (This works but we need to change the code
            ;; that pushes the description in the first place to not push the entire subtree
            ;; (save-excursion
            ;;   (org-narrow-to-subtree)
            ;;   (goto-char (point-min))
            ;;   (re-search-forward "^:END:")
            ;;   (forward-line)
            ;;   ;; delete till the next heading starts or buffer ends
            ;;   (let ((pos (point)))
            ;;     (delete-region pos (point)))
            ;;   (insert todoist-description)
            ;;   (widen))
            (org-entry-put marker todoist-sync-org-prop-synctoken
                           new-sync-token)
            (let ((heading (org-get-heading t t t t))
                  (description (todoist-sync--clean-org-text
                                (substring-no-properties (todoist-sync--get-body-without-subtree)))))
              (org-entry-put marker todoist-sync-org-prop-hash
                             (todoist-sync--hash-org-element description heading)))))
         ;; not yet synced and not done
         ((and (not synced-id) (not org-is-done))
          (todoist-sync--debug-msg "Adding item %s to todoist." heading)
          (let* ((temp_id (todoist-sync--generate-temp_id))
                 (synced-id-parent (todoist-sync--get-first-synced-parent))
                 (command
                  (todoist-sync--add-item-command
                   temp_id
                   `((content . ,heading)
                     (project_id . ,todoist-sync--agenda-uuid)
                     (description . ,description)
                     (due . ,due)
                     (parent_id . ,synced-id-parent))))
                 (callback
                  (lambda (data)
                    ;; Add the id of the new item to the org entry
                    (let* ((temp_id_mapping (alist-get 'temp_id_mapping data))
                           (temp_id_symbol (intern temp_id))
                           (id (alist-get temp_id_symbol temp_id_mapping))
                           (sync-token (alist-get 'sync_token data)))
                      ;; Here the temp id is replaced with the actual id after the sync
                      (org-entry-put marker todoist-sync-org-prop-id id)
                      (org-entry-put marker todoist-sync-org-prop-synctoken sync-token)
                      (org-entry-put marker todoist-sync-org-prop-hash hash)
                      (todoist-sync--info-msg "Added item %s" id)))))
            ;; Store temp id so that other items can reference it during this update
            (org-entry-put marker todoist-sync-org-prop-id temp_id)
            (todoist-sync--add-command command callback command-stack))))))))

(defun todoist-sync--items-to-items-by-id (items)
  "Creates alist to lookup items by id."
  (mapcar (lambda (item) (cons (alist-get 'id item) item)) items))

(defun todoist-sync--multiple-get-items-requests (sync-tokens callback)
  "Get all updates for the given SYNC-TOKENS asynchronously.
  Calls CALLBACk with:
 ((<sync_token> . ((sync_token . <new_sync_token>) (items . <item>))) alist"
  (todoist-sync--debug-msg "multiple get items requests: %s" sync-tokens)
  (let ((result nil)
        (remaining-sync-tokens sync-tokens))
    (unless sync-tokens
      (funcall callback nil))
    (dolist (sync-token sync-tokens)
      (todoist-sync-get-items
       (lambda (data)
         (let ((new-sync-token (alist-get 'sync_token data))
               (items (todoist-sync--items-to-items-by-id (alist-get 'items data))))
           (push (cons sync-token `((sync_token . ,new-sync-token) (items . ,items))) result))
         (setq remaining-sync-tokens (remove sync-token remaining-sync-tokens))
         (when (eq remaining-sync-tokens nil)
           (funcall callback result)))
       sync-token))))

(defun todoist-sync--visit-org-headings (headings &optional done-callback)
  "Visit all org headings in HEADINGS and update them with the TODOIST-UPDATES-BY-SYNC-ID.
  HEADINGS is a list of tuples (marker . sync-token)."
  (let* ((unique-sync-tokens
          (seq-filter #'identity (seq-uniq (mapcar #'cdr headings)))))
    (todoist-sync--debug-msg "unique sync tokens: %s" unique-sync-tokens)
    (todoist-sync--multiple-get-items-requests
     unique-sync-tokens
     (lambda (updated-items-by-sync-token)
       (todoist-sync--debug-msg "updated items from todoist:\n%s" updated-items-by-sync-token)
       (let ((command-stack (todoist-sync--get-empty-command-stack)))
         (dolist (heading headings)
           (let* ((marker (car heading))
                  (sync-token (cdr heading))
                  (updated-data (assoc sync-token updated-items-by-sync-token nil)))
             (todoist-sync--visit-org-heading
              marker
              updated-data
              command-stack)))
         (todoist-sync--request-commands command-stack done-callback))))))

(defun todoist-sync-file (&optional done-callback)
  "Syncronizes the todos in the current file with todoist."
  (interactive)
  (let ((buffer (current-buffer)))
    (todoist-sync--ensure-agenda-uuid
     (lambda ()
       (todoist-sync--debug-msg "syncing file =====================================\n%s\n===========================" (buffer-file-name))
       (let ((headings nil))
         (todoist-sync--org-visit-todos-in-file
          (buffer-file-name buffer)
          (lambda ()
            (let ((marker (point-marker))
                  (sync-token (org-entry-get (point) todoist-sync-org-prop-synctoken)))
              ;; append to end of list
              (setq headings (append headings (list (cons marker sync-token)))))))
         (todoist-sync--debug-msg "found headings:\n%s" headings)
         (todoist-sync--visit-org-headings headings done-callback))))))

(defun todoist-sync-heading ()
  "Syncronizes the todo at point with todoist."
  (interactive)
  (todoist-sync--ensure-agenda-uuid
   (lambda ()
     (todoist-sync--debug-msg "syncing heading =====================================\n%s\n===========================" (buffer-file-name))
     (save-excursion
       (org-back-to-heading)
       (let* ((marker (point-marker))
              (sync-token (org-entry-get (point) todoist-sync-org-prop-synctoken))
              (headings (list (cons marker sync-token))))
         (todoist-sync--visit-org-headings headings))))))

(defun todoist-sync-region ()
  "Syncronizes the region with todoist."
  (interactive)
  (todoist-sync--ensure-agenda-uuid
   (lambda ()
     (todoist-sync--debug-msg "syncing region =====================================\n%s\n===========================" (buffer-file-name))
     (let ((headings nil))
       (todoist-sync--org-visit-todos-in-buffer
        (current-buffer)
        (lambda ()
          (let ((marker (point-marker))
                (sync-token (org-entry-get (point) todoist-sync-org-prop-synctoken)))
            ;; append to end of list
            (setq headings (append headings (list (cons marker sync-token)))))))
       (todoist-sync--debug-msg "found headings:\n%s" headings)
       (todoist-sync--visit-org-headings headings)))))

(defun todoist-sync-buffer ()
  "Syncronizes the buffer with todoist."
  (interactive)
  (todoist-sync--ensure-agenda-uuid
   (lambda ()
     (todoist-sync--debug-msg "syncing buffer =====================================\n%s\n===========================" (buffer-file-name))
     (let ((headings nil))
       (todoist-sync--org-visit-todos-in-buffer
        (current-buffer)
        (lambda ()
          (let ((marker (point-marker))
                (sync-token (org-entry-get (point) todoist-sync-org-prop-synctoken)))
            ;; append to end of list
            (setq headings (append headings (list (cons marker sync-token)))))))
       (todoist-sync--debug-msg "found headings:\n%s" headings)
       (todoist-sync--visit-org-headings headings)))))

(defun todoist-sync-subtree ()
  "Syncronizes the subtree at point with todoist."
  (interactive)
  (todoist-sync--ensure-agenda-uuid
   (lambda ()
     (todoist-sync--debug-msg "syncing subtree =====================================\n%s\n===========================" (buffer-file-name))
     (let ((headings nil))
       (save-excursion
         (org-back-to-heading)
         (org-narrow-to-subtree)
         (todoist-sync--org-visit-todos-in-buffer
          (current-buffer)
          (lambda ()
            (let ((marker (point-marker))
                  (sync-token (org-entry-get (point) todoist-sync-org-prop-synctoken)))
              ;; append to end of list
              (setq headings (append headings (list (cons marker sync-token)))))))
         (widen))
       (todoist-sync--debug-msg "found headings:\n%s" headings)
       (todoist-sync--visit-org-headings headings)))))

(defun todoist-sync-agenda ()
  "Syncronizes the todos in the agenda with todoist."
  (interactive)
  (todoist-sync--ensure-agenda-uuid
   (lambda ()
     (todoist-sync--debug-msg "syncing agenda =====================================\n%s\n===========================" (buffer-file-name))
     (let ((headings nil))
       (todoist-sync--org-visit-agenda-todos
        (lambda ()
          (let ((marker (point-marker))
                (sync-token (org-entry-get (point) todoist-sync-org-prop-synctoken)))
            ;; append to end of list
            (setq headings (append headings (list (cons marker sync-token)))))))
       (todoist-sync--debug-msg "found headings:\n%s" headings)
       (todoist-sync--visit-org-headings headings)))))

(provide 'todoist-sync)
;;; todoist-sync.el ends here
