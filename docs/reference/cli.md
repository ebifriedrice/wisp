# CLI reference

The `wisp` command is a dotnet tool (`Wisp.Cli`) and also runs directly from
`src/Wisp.Cli`.

## Commands

### `wisp <file.wisp>`

Evaluates one file in a fresh session. Project references and project loading
are not used.

### `wisp --eval <form>`

Evaluates one form and prints its value.

### `wisp`

Starts the REPL in a fresh session.

### `wisp repl [project]`

Starts the REPL. With a project path, loads the whole project graph and watches
its sources and manifests.

### `wisp build [project]`

Loads the project graph, evaluates declarations and values, and exits. Does not
invoke the startup function.

### `wisp run [project]`

Builds the project, then invokes `StartupObject` with no arguments.

### `wisp init [directory]`

Scaffolds a project in the current directory or the given directory.

Options:

- `--template app|lib|workspace` — default `app`.
- `--name <name>` — project name and root namespace.
- `--force` — overwrite an existing template.

### `wisp new <directory>`

Same as `init`, but requires a directory name and creates it.

### `wisp templates`

Lists the built-in templates.

### `wisp --version`, `wisp --help`

Print the version or usage.

## Project REPL

The project REPL is line-oriented and supports multiline input, Wisp syntax
highlighting, and persisted history (`~/.wisp_history`). Press Enter to submit a
line; an incomplete form continues on the next line.

| Key | Action |
|---|---|
| `Enter` | Submit the line, or continue a multiline form |
| `Up` / `Down` | Previous / next history entry |
| `Ctrl-C` | Clear the current input |
| `Ctrl-D` | Exit on an empty line |
| `Ctrl-L` | Clear the screen |
| `Ctrl-U` / `Ctrl-K` | Delete to the start / end of the line |
| `Left` / `Right`, `Home` / `End` | Move the cursor |
| `Backspace` / `Delete` | Delete a character |
| `Tab` | Insert two spaces |

Commands:

- `:help` — show command help.
- `:reload` — force a project reload.
- `:quit` / `:q` / `:exit` — quit.

### Hot reload

The project REPL watches every project directory and `*.wisp.proj` manifest.

- A changed function or value file reloads only the changed files in source
  order, preserving the session.
- A changed type declaration, namespace dependency, manifest, added file, or
  removed file triggers a full project reload with a fresh session.
- A dependency project source change is reloaded in the same way.

Removed definitions leave their old vars in the session until a full reload.

## Diagnostics

Reader, compiler, and runtime diagnostics are rendered with file, line,
column, and a source caret when the source is available. Uncaught runtime
failures also include a Wisp stack trace with one
`at <namespace>/<function> (<file>:<line>:<column>)` line per active function.
Each line points at that function's call site; the final line is
`<top-level>`.

## Packages

- `Wisp.Cli` — dotnet tool, command name `wisp`.
- `Wisp.Sdk` — MSBuild SDK with `WispBuild` and `WispRun` targets.

Both packages publish to GitHub Packages and to the GitHub release for a
`bootstrap-*` tag.
