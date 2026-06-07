### Mastering Magit

Magit is essentially a keyboard-driven interface for Git. Instead of typing out long strings of Git commands, you press single keystrokes to interact with your repository.

To start, press `C-x g` (which we mapped earlier) to open the `magit-status` buffer. This is your command center.

**The Basic Workflow:**

1. **Navigate:** Use your standard Evil keys (`j` and `k`) to move up and down the status buffer.
2. **Expand/Collapse:** Press `TAB` on any file or unstaged hunk to see the exact diff of what changed.
3. **Stage:** Press `s` on a file or hunk to stage it. Press `u` to unstage it.
4. **Commit:** Press `c c` to initiate a commit. A new window will pop up for your commit message. Type your message, then press `C-c C-c` to finalize it (or `C-c C-k` to cancel).
5. **Push:** Press `P p` to push your commits to the remote origin.
6. **Pull:** Press `F p` to pull changes from the remote.

**Other Handy Magit Keys (from the status buffer):**

* `b b`: Switch branches or create a new one.
* `l l`: View the git log (commit history).
* `z z`: Stash your current changes.

---

### Getting Started with Org Mode

Org mode is a highly flexible plain-text system for note-taking, task management, and document authoring.

**1. Structure and Outlining**
Headings are created using asterisks. The number of asterisks determines the depth.

```org
* Top Level Heading
** Second Level Heading
*** Third Level Heading

```

* **Expand/Collapse:** Place your cursor on a heading and press `TAB` (`org-cycle`) to toggle its visibility. Press `S-TAB` (Shift + Tab) to expand or collapse the entire document at once.

**2. Task Management (TODOs)**
Any heading can become a task.

* **Toggle Status:** Press `C-c C-t` (`org-todo`) while on a heading to cycle it from standard text, to `TODO`, to `DONE`.
* **Log Completion:** Because of the `(org-log-done 'time)` setting added previously, marking a task as `DONE` will automatically insert a timestamp beneath it.

**3. Scheduling and Deadlines**

* **Schedule:** Press `C-c C-s` on a TODO item to assign a start date. Emacs will prompt you with a mini-calendar.
* **Deadline:** Press `C-c C-d` to assign a hard deadline.

**4. Links**

* **Insert Link:** Press `C-c C-l` to insert a hyperlink. It will prompt you for the URL, and then the description (the text that will actually be displayed).

---

### The 50 Most Useful Emacs Commands

Here are 50 essential Emacs commands (functions), categorized by utility. You can execute any of these by pressing `M-x` (or `SPC SPC` via Smex) and typing the command name, though many are already bound to keys in your configuration.

#### Files & Buffers

| # | Command | Description |
| --- | --- | --- |
| 1 | `find-file` | Open a file or create a new one. |
| 2 | `save-buffer` | Save the current file. |
| 3 | `write-file` | Save the file under a new name (Save As). |
| 4 | `switch-to-buffer` | Switch to an already open buffer. |
| 5 | `kill-buffer` | Close the current buffer. |
| 6 | `revert-buffer` | Reload the current file from disk. |
| 7 | `ibuffer` | Open a rich, interactive list of all open buffers. |
| 8 | `dired` | Open the Emacs file manager. |
| 9 | `dired-jump` | Open Dired in the directory of the current file. |
| 10 | `recentf-open-files` | View and open recently accessed files. |

#### Window Management

| # | Command | Description |
| --- | --- | --- |
| 11 | `split-window-right` | Split the current window vertically. |
| 12 | `split-window-below` | Split the current window horizontally. |
| 13 | `other-window` | Move focus to the next window. |
| 14 | `delete-window` | Close the currently active window. |
| 15 | `delete-other-windows` | Close all windows except the active one. |
| 16 | `balance-windows` | Equalize the dimensions of all open windows. |

#### Help & Discoverability

| # | Command | Description |
| --- | --- | --- |
| 17 | `describe-function` | Show documentation for an Emacs function. |
| 18 | `describe-variable` | Show the value and documentation for a variable. |
| 19 | `describe-key` | Press a key combination to see what command it runs. |
| 20 | `describe-mode` | See all active minor modes and their keybindings. |
| 21 | `describe-bindings` | List all currently available keybindings. |
| 22 | `apropos` | Search for functions/variables matching a keyword. |
| 23 | `info` | Open the built-in Emacs manual. |
| 24 | `eldoc-box-help-at-point` | Open a floating window with docs for the symbol at point. |

#### Code & LSP Integration

| # | Command | Description |
| --- | --- | --- |
| 25 | `eglot` | Manually start the Language Server Protocol for a project. |
| 26 | `eglot-rename` | Safely rename a variable/function across a project. |
| 27 | `xref-find-definitions` | Jump to the definition of a symbol. |
| 28 | `xref-find-references` | Find all references of a symbol in the project. |
| 29 | `flymake-show-diagnostic-at-point` | Show error/warning details in the minibuffer. |
| 30 | `flymake-goto-next-error` | Jump to the next diagnostic error in the file. |
| 31 | `flymake-goto-prev-error` | Jump to the previous diagnostic error. |
| 32 | `apheleia-format-buffer` | Run the language's standard auto-formatter on the file. |

#### Search & Text Manipulation

| # | Command | Description |
| --- | --- | --- |
| 33 | `isearch-forward` | Start incrementally searching the file. |
| 34 | `query-replace` | Interactive find-and-replace. |
| 35 | `query-replace-regexp` | Interactive find-and-replace using Regular Expressions. |
| 36 | `occur` | List all lines matching a search string in a new buffer. |
| 37 | `flush-lines` | Delete all lines matching a specific pattern. |
| 38 | `keep-lines` | Delete all lines *except* those matching a pattern. |
| 39 | `align-regexp` | Vertically align text based on a specific character (e.g., `=`). |

#### Magit & Git

| # | Command | Description |
| --- | --- | --- |
| 40 | `magit-status` | Open the main Git dashboard. |
| 41 | `magit-log-all` | View the commit tree for the whole repository. |
| 42 | `magit-blame` | Show who wrote each line in the current file. |
| 43 | `magit-dispatch` | Open a menu of all Magit commands. |
| 44 | `magit-file-dispatch` | Open Magit commands specific to the current file. |
| 45 | `diff-hl-mode` | Toggle Git gutter highlights in the margins. |

#### Org Mode & Emacs Core

| # | Command | Description |
| --- | --- | --- |
| 46 | `org-agenda` | Open the central dashboard for all scheduled tasks. |
| 47 | `org-capture` | Quickly capture a thought or task without leaving your file. |
| 48 | `org-cycle` | Expand/collapse an Org heading. |
| 49 | `eval-buffer` | Execute all the Emacs Lisp code in the current buffer. |
| 50 | `execute-extended-command` | The canonical name for `M-x`. |