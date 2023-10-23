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
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


;; TODO: support for recurring tasks
;; TODO: check for conflicts in the deadline and allow user to resolve them
;; TODO: write file??? (or ask user if file should be written to)
;; TODO: if called interactively ask to save changed files

(require 'json)
(require 'request)
(require 'org)
(require 'org-id)

(defvar todoist-sync-token nil
  "The Todoist API token.")
(defvar todoist-sync-agenda-project "agenda"
  "The name of the project to sync with the agenda.")
(defvar todoist-sync-org-prop-synced "TODOIST_SYNCED")

(defvar todoist-sync-todoist-org-file nil)

;; TODO: test without incremental sync
(defvar todoist-sync-incremental-sync t "Whether to use incremental sync or not.")
(defvar todoist-sync--sync-token "nil" "The sync token for the Todoist API.")

(defvar todoist-sync--api-url "https://api.todoist.com/sync/v9/sync")

;; This gets cached before the first sync
(defvar todoist-sync--agenda-uuid nil "The UUID of the agenda project.")

(defvar todoist-sync-fold-open-descriptions nil "Whether to fold open descriptions in the org file. Requires emacs version > 29.1")

(defun todoist-sync--get-sync-token ()
  "Get the sync token for the Todoist API. May return '*'
   to indicate a full sync is configured."
  (if todoist-sync-incremental-sync
      todoist-sync--sync-token
    "*"))

(defun todoist-sync--update-sync-token (response)
  "Updates the sync token from a todoist response"
  (setq todoist-sync--sync-token (cdr (assoc 'sync_token response))))

;; ================== API ==================

(defun todoist-sync-get-projects (callback)
  "Get all projects from the Todoist API. Ignores sync token."
  (request
    todoist-sync--api-url
    :headers `(("Authorization" . ,(concat "Bearer " todoist-sync-token)))
    :params `(("sync_token" . "*")
              ("resource_types" . "[\"projects\"]"))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback (cdr (assoc 'projects data)))))
    :error (cl-function
            (lambda (&key data &allow-other-keys)
              (message "Got error: %S" data)))))

(defun todoist-sync-get-items (callback &optional update-sync-token)
  "Get all items changed since the last sync from the Todoist API."
  (request
    todoist-sync--api-url
    :headers `(("Authorization" . ,(concat "Bearer " todoist-sync-token)))
    :params `(("sync_token" . ,(todoist-sync--get-sync-token))
              ("resource_types" . "[\"items\"]"))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (if update-sync-token
                    (todoist-sync--update-sync-token data))
                (funcall callback (cdr (assoc 'items data)))))
    :error (cl-function
            (lambda (&key data &allow-other-keys)
              (message "Got error: %S" data)))))

(defun todoist-sync--filter-by-project (project-id items)
  "Filter the list of items by the project ID."
  (cl-remove-if-not (lambda (item)
                      (equal (alist-get 'project_id item) project-id))
                    items))

(defun todoist-sync-add-item (args callback)
  (request
    todoist-sync--api-url
    :headers `(("Authorization" . ,(concat "Bearer " todoist-sync-token)))
    :params `(("sync_token" . ,(todoist-sync--get-sync-token))
              ("resource_types" . "[\"items\"]")
              ("commands" . ,(json-encode `(((type . "item_add")
                                             ;; TODO: [MULTIPLE] use temp ids here
                                             (temp_id . ,"TMP-ID")
                                             (uuid . ,(todoist-sync--generate-uuid))
                                             (args . ,args))))))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))
    :error (cl-function
            (lambda (&key data &allow-other-keys)
              (message "Got error: %S" data)))))

;; NOTE: untested
(defun todoist-sync--update-item (args callback)
  (request
    todoist-sync--api-url
    :headers `(("Authorization" . ,(concat "Bearer " todoist-sync-token)))
    :params `(("sync_token" . ,(todoist-sync--get-sync-token))
              ("resource_types" . "[\"items\"]")
              ("commands" . ,(json-encode `(((type . "item_update")
                                             (uuid . ,(todoist-sync--generate-uuid))
                                             (args . ,args))))))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))
    :error (cl-function
            (lambda (&key data &allow-other-keys)
              (message "Got error: %S" data)))))

(defun todoist-sync--complete-item (id callback)
  (request
    todoist-sync--api-url
    :headers `(("Authorization" . ,(concat "Bearer " todoist-sync-token)))
    :params `(("sync_token" . ,(todoist-sync--get-sync-token))
              ("resource_types" . "[\"items\"]")
              ("commands" . ,(json-encode `(((type . "item_complete")
                                             (uuid . ,(todoist-sync--generate-uuid))
                                             (args . ((id . ,id)))))
                                          )))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))
    :error (cl-function
            (lambda (&key data &allow-other-keys)
              (message "Got error: %S" data)))))

;; ====== Org mode integration ======

;; TODO: combine these functions
(defun todoist-sync--org-visit-todos (callback)
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
        (with-current-buffer (marker-buffer marker)
          (goto-char marker)
          (funcall callback))))))

(defun todoist-sync--get-first-synced-parent ()
  "Get the first parent heading of the heading at point that has a todoist id."
  (save-excursion
    (when (org-up-heading-safe)
      (let ((parent-synced-id (org-entry-get (point) todoist-sync-org-prop-synced)))
        (if parent-synced-id
            parent-synced-id
          (todoist-sync--get-first-synced-parent))))))

;; ================== Write todoist items to org file ==================
(defun todoist-sync--insert-todoist-file-explanation ()
  "Inserts a comment explaining the file format."
  ;; TODO: Finish header
  (insert "# This file is automatically generated by todoist-sync.el\n"))

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

(defun todoist-sync--write-todo-to-org (todo level)
  "Write a single TODO item to the current buffer, indented to LEVEL."
  (let ((data (cdr (assoc 'data todo)))
        (sub-todos (cdr (assoc 'todos todo))))
    (insert (make-string level ?*) " TODO " (alist-get 'content data) "\n")
    (org-entry-put (point) todoist-sync-org-prop-synced (alist-get 'id data))
    (when (alist-get 'due data)
      (org-deadline nil (alist-get 'date (alist-get 'due data))))
    (when (alist-get 'description data)
      (insert (alist-get 'description data) "\n"))
    (dolist (child-todo sub-todos)
      (todoist-sync--write-todo-to-org child-todo (+ 1 level)))))

(defun todoist-sync--write-project-to-org (project)
  "Write a single PROJECT and its TODOs to the current buffer."
  (let ((data (cdr (assoc 'data project)))
        (todos (cdr (assoc 'todos project))))
    (insert "* " (alist-get 'name data) "\n")
    (dolist (todo todos)
      (todoist-sync--write-todo-to-org todo 2))))

(defun todoist-sync--write-hierarchical-data-to-org-file (hierarchical-data)
  (dolist (project hierarchical-data)
    (todoist-sync--write-project-to-org project)))

(defun todoist-sync--insert-todos-into-file (&optional done-callback)
  "Inserts todos into file."
                                        ;: TODO: using a separate sync token tracking keep an internal representation
  ;; and modify it as we go along
  (todoist-sync-get-projects
   ;; HACK to do full sync
   (lambda (projects)
     (let ((todoist-sync--sync-token "*"))
       (todoist-sync-get-items
        (lambda (items)
          (setq titems items)
          (setq tproject projects)
          (let ((hierarchical-data (todoist-sync--hierarchicalize-projects projects items)))
            (todoist-sync--write-hierarchical-data-to-org-file hierarchical-data))
          (funcall done-callback)))))))

(defun todoist-sync-write-to-file ()
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
     (save-buffer))))

;; ================== Sync Agenda Files ==================

;; TODO: do this properly
;; TODO: remove DEADLINE from description
(defun todoist-sync--clean-org-text (text)
  "Remove the PROPERTIES drawer from the org text."
  (let* ((text (replace-regexp-in-string ":PROPERTIES:\\(.*\n\\)*?:END:\n" "" text))
         (text (replace-regexp-in-string "DEADLINE:.*" "" text)))
    text))

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

;; Gets all updated items from the agenda project
(defun todoist-sync--get-agenda-udpates (callback)
  "Get all items from the agenda project.
  Calls the callback with the changed items as argument."
  (todoist-sync-get-items
   (lambda (items)
     (funcall callback (todoist-sync--filter-by-project todoist-sync--agenda-uuid items)))
   t))

(defun todoist-sync--visit-org-heading (changed-agenda-items-by-id)
  (let ((synced-id (org-entry-get (point) todoist-sync-org-prop-synced))
        ;; BUG: If the parent is currently being pushed there is no ID yet
        ;; Solve: use uuids and collect all changes then do one request
        (synced-id-parent (todoist-sync--get-first-synced-parent))
        (heading (org-get-heading t t t t))
        (description (todoist-sync--clean-org-text (org-get-entry)))
        (due (todoist-sync--todoist-date-for-at-point))
        (org-is-done (org-entry-is-done-p))
        (mark-pos (point-marker)))
    (when synced-id
      ;; If the item is already synced we need to check if it has changed
      (let* ((todoist-item (cdr (assoc synced-id changed-agenda-items-by-id)))
             (todoist-is-done (cdr (assoc 'completed_at todoist-item))))
        (when org-is-done
          (todoist-sync--complete-item
           synced-id
           (lambda (_)
             (org-entry-delete mark-pos todoist-sync-org-prop-synced))))
        (when todoist-is-done
          (org-todo 'done)
          ;; TODO: handle cancled etc better
          (org-entry-delete mark-pos todoist-sync-org-prop-synced))))
    (when (and (not synced-id) (not org-is-done))
      ;; TODO: [MULTIPLE] collect all items and send them in one request
      ;; TODO: If the parent is also a todo item add it as the parent-task
      (todoist-sync-add-item
       `((content . ,heading)
         (project_id . ,todoist-sync--agenda-uuid)
         (description . ,description)
         (due . ,due)
         (parent_id . ,synced-id-parent))
       (lambda (data)
         ;; Add the id of the new item to the org entry
         (let* ((temp_id_mapping (car (cdr (assoc 'temp_id_mapping data))))
                (id (cdr temp_id_mapping)))
           (org-entry-put mark-pos todoist-sync-org-prop-synced id)
           (message "Successfully added item %s" id)))))))

(defun todoist-sync--do-sync ()
  "Sync the agenda with todoist."
  (todoist-sync--get-agenda-udpates
   (lambda (items)
     (let ((agenda-items-by-id (mapcar (lambda (item) (cons (cdr (assoc 'id item)) item)) items)))
       (todoist-sync--org-visit-todos
        (lambda ()
          (todoist-sync--visit-org-heading agenda-items-by-id)))))))

(defun todoist-sync-push-buffer ()
  "Push the changes in the current buffer to todoist."
  (interactive)
  (todoist-sync--org-visit-todos-in-file
   (buffer-file-name)
   (lambda ()
     (todoist-sync--visit-org-heading nil))))

(defun todoist-sync-push-heading ()
  "Push the changes in the current heading to todoist."
  (interactive)
  ;; TODO: provide a recursive variant of this function that pushes parent headings as well
  ;; TODO: provide a push region function
  (save-excursion
    (org-back-to-heading)
    (todoist-sync--visit-org-heading nil)))

;; The only public function in the package
(defun todoist-sync ()
  "Sync the agenda with todoist."
  (interactive)
  (todoist-sync--ensure-agenda-uuid #'todoist-sync--do-sync))

(provide 'todoist-sync)
;;; todoist-sync.el ends here
