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



(require 'json)
(require 'request)
(require 'org)
(require 'org-id)

(defvar todoist-sync-token nil
  "The Todoist API token.")
(defvar todoist-sync-agenda-project "agenda"
  "The name of the project to sync with the agenda.")
(defvar todoist-sync-org-prop-synced "TODOIST_SYNCED")

;; BUG: This must be t for the package to work
;; TODO: when incremental sync is disabled the response contains all items that are not done
;; Any item that is not in the response should be assumed to be done
(defvar todoist-sync-incremental-sync t "Whether to use incremental sync or not.")
(defvar todoist-sync--sync-token "nil" "The sync token for the Todoist API.")
;; todo load from file

(defvar todoist-sync--api-url "https://api.todoist.com/sync/v9/sync")

(defvar todoist-sync--agenda-uuid-cache nil "The UUID of the agenda project.")


(defun todoist-sync--get-sync-token ()
  "Either returns the sync token or * if a full sync is needed."
  (if todoist-sync-incremental-sync
      todoist-sync--sync-token
    "*"))

;; TODO: remove this again
(defun todoist-sync--update-sync-token (new-token)
  "Updates the sync token"
  (setq todoist-sync--sync-token new-token))

(defun todoist-sync--generate-uuid ()
  "Generate a random UUID."
  (replace-regexp-in-string " " "-" (downcase (md5 (format "%s%s%s" (random) (current-time) user-mail-address)))))

(defun todoist-sync-get-projects (callback)
  "Get all projects from the Todoist API. Ignores sync token."
  (message "Getting projects")
  (request
    todoist-sync--api-url
    :headers `(("Authorization" . ,(concat "Bearer " todoist-sync-token)))
    :params `(("sync_token" . "*")
              ("resource_types" . "[\"projects\"]"))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (todoist-sync--update-sync-token (cdr (assoc 'sync_token data)))
                (funcall callback (cdr (assoc 'projects data)))))
    :error (cl-function
            (lambda (&key data &allow-other-keys)
              (message "Got error: %S" data)))
    ))

;; DEPRECATED: the updated items will usually just be the response of the add-item request
(defun todoist-sync-get-items (callback)
  "Get all items from the Todoist API."
  (request
    todoist-sync--api-url
    :headers `(("Authorization" . ,(concat "Bearer " todoist-sync-token)))
    :params `(("sync_token" . ,(todoist-sync--get-sync-token))
              ("resource_types" . "[\"items\"]"))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (todoist-sync--update-sync-token (cdr (assoc 'sync_token data)))
                (funcall callback (cdr (assoc 'items data)))))
    :error (cl-function
            (lambda (&key data &allow-other-keys)
              (message "Got error: %S" data)))))

(defun todoist-sync-add-item (marker args agenda-uuid &optional sync-todo-state)
  "Add an item to a project."

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
    ;; TODO custom success / failure callbacks
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (when sync-todo-state
                  (message "Syncing todo state")
                  (todoist-sync--update-sync-token (cdr (assoc 'sync_token data)))
                  ;; NOTE: I'm not sure if there could be conflicts if the some heading is updated at the same time asynchronously (also not everything has an id yet ...)
                  (todoist-sync--org-update-from-items
                   (todoist-sync--filter-by-project
                    (cdr (assoc 'items data))
                    agenda-uuid)))
                ;; TODO: [MULTIPLE] For now assume that only one item is added at a time.
                (let* ((temp_id_mapping (car (cdr (assoc 'temp_id_mapping data))))
                       (id (cdr temp_id_mapping)))
                  (org-entry-put marker todoist-sync-org-prop-synced id)
                  (message "Successfully added item %s" id))))
    :error (cl-function
            (lambda (&key data &allow-other-keys)
              (message "Got error: %S" data)))))

(defun todoist-sync--extract-agenda-project-uuid (projects)
  "Extract the UUID of the agenda project from the list of projects."
  (cdr (assoc 'id (car (seq-filter
                        (lambda (project)
                          (string= (cdr (assoc 'name project)) todoist-sync-agenda-project))
                        projects)))))

(defun todoist-sync--filter-by-project (items project-uuid)
  "Filter the list of items by the project UUID."
  (seq-filter
   (lambda (item)
     (string= (cdr (assoc 'project_id item)) project-uuid))
   items))


(defun todoist-sync--ensure-agenda-uuid (callback)
  (if todoist-sync--agenda-uuid-cache
      (funcall callback todoist-sync--agenda-uuid-cache)
    (todoist-sync-get-projects
     (lambda (projects)
       (let ((agenda-project-uuid (todoist-sync--extract-agenda-project-uuid projects)))
         (setq todoist-sync--agenda-uuid-cache agenda-project-uuid)
         (funcall callback agenda-project-uuid)))
     )))


;; @DEPRECATED the update should be done by the update function and when
;; sending items.
(defun todoist-sync-get-agenda-items (callback)
  "Get all items from the agenda project."
  (todoist-sync--ensure-agenda-uuid
   (lambda (agenda-uuid)
     (todoist-sync-get-items
      (lambda (items)
        (funcall callback (todoist-sync--filter-by-project items agenda-uuid))))
     )))


;; ====== Org mode integration ======

(defun todoist-sync--org-visit-todos (callback)
  "Return a list of all (not done) TODO entries from all agenda files.
Each element in the list is a cons cell (HEADING . FILENAME)."
  (let* ((today (org-today))
         (date (calendar-gregorian-from-absolute today))
         (files (org-agenda-files nil 'ifmode))
         (rtnall nil))
    (while (setq file (pop files))
      (org-check-agenda-file file)
      (let ((rtn (org-agenda-get-day-entries file date :todo)))
        (setq rtnall (append rtnall rtn))))
    (dolist (entry rtnall)
      (let ((marker (get-text-property 0 'org-hd-marker entry)))
        (with-current-buffer (marker-buffer marker)
          (goto-char marker)
          (funcall callback))))))

;;
;; TODO: do this properly
(defun todoist-sync--clean-org-text (text)
  "Remove the PROPERTIES drawer from the org text."
  (replace-regexp-in-string ":PROPERTIES:\\(.*\n\\)*?:END:\n" "" text))

;; TODOIST somehow assumes that the task should be at two
;; (defun todoist-sync--org-to-todoist-date (org-time-str)
;;   "Convert an org date string to a todoist date string."
;;   (format-time-string "%Y-%m-%dT%H:%M:%S.000000Z" (org-time-string-to-time org-time-str)))

(defun todoist-sync--org-to-todoist-date (org-time-str)
  "Converts <2023-10-09 Sun> to 2023-10-09"
  (replace-regexp-in-string "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\).*>" "\\1" org-time-str))

(defun todoist-sync-push-agenda-todos (&optional sync-todo-state)
  "Push all TODO entries from all agenda files to todoist."
  (todoist-sync--org-visit-todos
   (lambda ()
     (let* ((synced (org-entry-get (point) todoist-sync-org-prop-synced))
            (heading (org-get-heading t t t t))
            (description (todoist-sync--clean-org-text (org-get-entry)))
            (org-due-str (org-entry-get (point) "DEADLINE"))
            (todoist-due-string (when org-due-str (todoist-sync--org-to-todoist-date org-due-str)))
            ;; TODO: Use this when directly assigning the time
            ;; (due (when todoist-due-string `((date . ,todoist-due-string))))
            (due (when todoist-due-string `((string . ,todoist-due-string)))))
       (when (not synced)
         ;; TODO: [MULTIPLE] collect all items and send them in one request
         (todoist-sync--ensure-agenda-uuid
          (lambda (agenda-uuid)
            (todoist-sync-add-item
             (point-marker) `((content . ,heading)
                              (project_id . ,agenda-uuid)
                              (description . ,description)
                              (due . ,due))
             agenda-uuid
             sync-todo-state))))))))


(defun todoist-sync--org-do-update (id item)
  "Called for each org heading where the todoist item has changed."
  ;; At the moment the cursor is placed at that position ... TODO: pass marker insead
  (let* ((org-is-done (org-entry-is-done-p))
         (todoist-is-done (cdr (assoc 'completed_at item))))
    (if (and (not org-is-done) todoist-is-done)
        (org-todo 'done))
    ;; BUG: can't be visited because the org-crawling function ignores tasks that are done
    (if (and org-is-done (not todoist-is-done))
        (message "%s (%s) is done in org but not in todoist." (org-get-heading t t t t) id))))


(defun todoist-sync--org-update-from-items (items)
  "Update the org entry with the data from the Todoist API."
  (message "Updating org from todoist:\n%s" items)
  (todoist-sync--ensure-agenda-uuid
   (lambda (agenda-uuid)
     (let* ((agenda-items (todoist-sync--filter-by-project items agenda-uuid))
            (agenda-items-by-id (mapcar (lambda (item) (cons (cdr (assoc 'id item)) item)) agenda-items)))
       (if agenda-items
           (todoist-sync--org-visit-todos
            (lambda ()
              (let* ((id (org-entry-get (point) todoist-sync-org-prop-synced))
                     (item (cdr (assoc id agenda-items-by-id))))
                (if item (todoist-sync--org-do-update id item)))))
         (message "Nothing changed in todoist."))))))

(defun todoist-sync-sync-todo-state ()
  "Download the todo state from todoist and update the org files."
  (interactive)
  (todoist-sync-get-agenda-items 'todoist-sync--org-update-from-items))


;; TODO: this only syncs if at least one item is added
(defun todoist-sync ()
  "Sync the agenda with todoist."
  (interactive)
  (todoist-sync-push-agenda-todos t)
  (todoist-sync-sync-todo-state))

(provide 'todoist-sync)
;;; todoist-sync.el ends here
