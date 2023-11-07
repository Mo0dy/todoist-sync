# todoist-sync for org-agenda todos

## Description

`todoist-sync.el` is an Emacs package that enables synchronization between your Org mode agenda and Todoist tasks. This package allows you to maintain a consistent task list across Org mode and Todoist by supporting task creation, update, and completion.

The main goal is to push any todois in org-agenda-files to todoist and pull their state back into org-agenda-files. Additioally the function `todo-sync-write-to-file` pulls all todos and their state from todoist into a file specified by `todoist-sync-todoist-org-file`. This file can be used to display the todoist tasks in org-agenda and push changes back to todoist using `todoist-sync-push-buffer`.

## Features

- Incremental sync with Todoist API.
- Handles due dates.
- Push org-mode items to todoist
- Syncrhonizes todo state, due date and heading of todos between todoist and org-mode
- Pull todoist items into an interactive org-mode file


## Requirements

- Emacs
- Org mode package
- `request.el` package for HTTP requests
- `json.el` package for JSON parsing

## Installation

1. Download the `todoist-sync.el` file to your Emacs directory.
2. Add the following line to your `.emacs` or `init.el` file:

```elisp
(load-file "path/to/todoist-sync.el")
```

## Configuration

Set the following variables before running `todoist-sync`:

```elisp
(setq todoist-sync-token "Your Todoist API Token")
(setq todoist-sync-agenda-project "Name of your Agenda Project in Todoist")
(setq todoist-sync-todoist-org-file "Path to the org file used to display Todoist tasks")
```

## Usage

Syn the current orgmode heading:

``` elisp
M-x todoist-sync-heading
```

To manually synchronize the current org file with todoist run the following command:

```elisp
M-x todoist-sync-file
```

To sync all agenda files:

```elisp
M-x todoist-sync-agenda
```

To pull all todos from todoist into the file specified by `todoist-sync-todoist-org-file` run:

```elisp
M-x todoist-sync-write-to-file
```

## Quality of Like Helpers

Sync todos when the todo state is updated in org-mode

``` elisp
(defun my/todoist-sync-push-heading-if-done ()
    (cond ((derived-mode-p 'org-agenda-mode)
        (let* ((marker (org-get-at-bol 'org-marker))
               (buffer (marker-buffer marker)))
          (with-current-buffer buffer
            (save-excursion
            (widen)
            (goto-char (marker-position marker))
            (org-fold-show-context 'agenda)
            (org-back-to-heading)
            (when (org-entry-is-done-p
                (todoist-sync-heading)))))))
          (t (when (org-entry-is-done-p)
               (todoist-sync-heading)))))
    
(after! org
    (add-hook 'org-after-todo-state-change-hook #'my/todoist-sync-push-heading-if-done))
```

Open the todoist file in agenda-view

``` elisp
(defun my/todoist-sync-create-todoist-file-and-open-agenda ()
  (interactive)
  (todoist-sync-write-to-file
     (lambda ()
        (org-agenda nil "a"))))
```

Automatically run a sync before agenda view. (Note that this may not complete in time.)
    
```elisp
(advice-add 'org-agenda :before #'todoist-sync)
```

Run sync every 5 minutes.

``` elisp
(run-with-timer 0 300 #'todoist-sync)
```

## Contribution

Contributions are welcome. If you find any bugs or have suggestions, please open an issue.

## License

This project is licensed under the MIT License.

## Disclaimer

This is not an official Todoist product. Use at your own risk.

---

For more details, please refer to the comments and documentation within the `todoist-sync.el` file.
