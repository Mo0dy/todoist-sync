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


(require 'json)
(require 'request)
(require 'org)
(require 'org-id)

(defvar todoist-sync-token nil
  "The Todoist API token.")
(defvar todoist-sync-agenda-project "agenda"
  "The name of the project to sync with the agenda.")
(defvar todoist-sync-org-prop-synced "TODOIST_SYNCED")

(defvar todoist-sync-incremental-sync nil "Whether to use incremental sync or not.")
(defvar todoist-sync--sync-token "nil" "The sync token for the Todoist API.")
;; todo load from file

(defvar todoist-sync--api-url "https://api.todoist.com/sync/v9/sync")

(defvar todoist-sync--agenda-uuid-cache nil "The UUID of the agenda project.")


(defun todoist-sync--get-sync-token ()
  "Either returns the sync token or * if a full sync is needed."
  (if todoist-sync-incremental-sync
      todoist-sync--sync-token
    "*"))

(defun todoist-sync--update-sync-token (new-token)
  "Updates the sync token"
  (setq todoist-sync--sync-token new-token)
  (message "Updated sync token to %s" new-token))

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

(defun todoist-sync-add-item (temp_id args)
  "Add an item to a project."

  (request
    todoist-sync--api-url
    :headers `(("Authorization" . ,(concat "Bearer " todoist-sync-token)))
    :params `(("sync_token" . ,(todoist-sync--get-sync-token))
              ("resource_types" . "[\"items\"]")
              ("commands" . ,(json-encode `(((type . "item_add")
                                             (temp_id . ,temp_id)
                                             (uuid . ,(todoist-sync--generate-uuid))
                                             (args . ,args))))))
    :parser 'json-read
    ;; TODO custom success / failure callbacks
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                                        ;: For now only getting data updates the token.
                ;; If setting data updates the token then we will have to parse the response and update accordingly.
                ;; (todoist-sync--update-sync-token (cdr (assoc 'sync_token data)))
                ;; (message "Got data: %s" data)
                (message "Successfully added item %s" temp_id)
                ))
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


(defun todoist-sync-push-agenda-todos ()
  (todoist-sync--org-visit-todos
   (lambda ()
     (let ((synced (org-entry-get (point) todoist-sync-org-prop-synced))
           (org_id (org-id-get-create))
           (heading (org-get-heading t t t t)))
       (when (not synced)
         ;; TODO: only mark synced if request was successful
         (org-entry-put (point) todoist-sync-org-prop-synced "t")
         ;; TODO: collect all items and send them in one request
         (todoist-sync-add-item org_id `((content . ,heading)
                                         (project_id . ,(todoist-sync--ensure-agenda-uuid
                                                         (lambda (agenda-uuid)
                                                           agenda-uuid))))))
       ))))

;; (todoist-sync-push-agenda-todos)

;; (todoist-sync-get-agenda-items (lambda (items)
;;                                  (message "%s" (json-encode items))))


;; (PUSH (cons heading (buffer-file-name)) todo-list))))))


;; (setq todoist-sync--agenda-uuid-cache nil)
;; Temp test code
;; (todoist-sync-get-agenda-items (lambda (items)
;;                                  (message "%s" (json-encode items))))

;; (todoist-sync--ensure-agenda-uuid
;;  (lambda (agenda-uuid)
;;    (todoist-sync-add-item "temp-id" `((content . "test")
;;                                       (project_id . ,agenda-uuid))))
;;  )

(provide 'todoist-sync)
;;; todoist-sync.el ends here
