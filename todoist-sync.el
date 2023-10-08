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

(defvar todoist-sync-token nil
  "The Todoist API token.")
(defvar todoist-sync-agenda-project "agenda"
  "The name of the project to sync with the agenda.")

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
                (funcall callback (cdr (assoc 'projects data)))
                (todoist-sync--update-sync-token (cdr (assoc 'sync_token data)))))
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
                (funcall callback (cdr (assoc 'items data)))))
    :error (cl-function
            (lambda (&key data &allow-other-keys)
              (message "Got error: %S" data)))))

(defun todoist-sync--generate-uuid ()
  "Generate a UUID."
  (sha1 (random) (current-time) (emacs-pid)))

(defun todoist-sync-add-item (temp_id))

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


(setq todoist-sync--agenda-uuid-cache nil)
;; Temp test code
(todoist-sync-get-agenda-items (lambda (items)
                                 (message "%s" (json-encode items))))


(provide 'todoist-sync)
;;; todoist-sync.el ends here
