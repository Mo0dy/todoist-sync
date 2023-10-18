# todoist-sync for org-agenda todos

## Description

`todoist-sync.el` is an Emacs package that enables synchronization between your Org mode agenda and Todoist tasks. This package allows you to maintain a consistent task list across Org mode and Todoist by supporting task creation, update, and completion.

## Features

- Incremental sync with Todoist API.
- Handles due dates.
- Push existing agenda items to todoist
- Syncrhonizes todo state between orgmode and todoist for items in the agenda project

## Upcoming

- Also pull todoist items into orgmode

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
```

## Usage

To manually synchronize the Org agenda with Todoist, run the following command:

```elisp
M-x todoist-sync
```

## Contribution

Contributions are welcome. If you find any bugs or have suggestions, please open an issue.

## License

This project is licensed under the MIT License.

## Disclaimer

This is not an official Todoist product. Use at your own risk.

---

For more details, please refer to the comments and documentation within the `todoist-sync.el` file.
