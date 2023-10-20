# todoist-sync for org-agenda todos

## Description

`todoist-sync.el` is an Emacs package that enables synchronization between your Org mode agenda and Todoist tasks. This package allows you to maintain a consistent task list across Org mode and Todoist by supporting task creation, update, and completion.

The main goal is to push any todois in org-agenda-files to todoist and pull their state back into org-agenda-files. Additioally the function `todo-sync-write-to-file` pulls all todos and their state from todoist into a file specified by `todoist-sync-todoist-org-file`. This file can be used to display the todoist tasks in org-agenda and push changes back to todoist using `todoist-sync-push-buffer`.

## Features

- Incremental sync with Todoist API.
- Handles due dates.
- Push existing agenda items to todoist
- Syncrhonizes todo state between orgmode and todoist for items in the agenda project
- Pull todoist items into a synchronized orgmode file


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

Automatically run a sync before agenda view. (Note that this may not complete in time.)
    
```elisp
(advice-add 'org-agenda :before #'todoist-sync)
```

Run sync every 5 minutes.

``` elisp
(run-with-timer 0 300 #'todoist-sync)
```

## Usage

To manually synchronize the Org agenda with Todoist, run the following command:

```elisp
M-x todoist-sync
```

To push all todos in the current file to todoist run:

```elisp
M-x todoist-sync-push-buffer
```

To pull all todos from todoist into the file specified by `todoist-sync-todoist-org-file` run:

```elisp
M-x todoist-sync-write-to-file
```

## Contribution

Contributions are welcome. If you find any bugs or have suggestions, please open an issue.

## License

This project is licensed under the MIT License.

## Disclaimer

This is not an official Todoist product. Use at your own risk.

---

For more details, please refer to the comments and documentation within the `todoist-sync.el` file.
