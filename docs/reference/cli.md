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

Starts the TUI REPL in a fresh session.

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

An interactive project REPL uses a Terminal.Gui editor with multiline editing,
selection, clipboard, undo/redo, and Wisp syntax highlighting.

| Key | Action |
|---|---|
| `F5` / `Ctrl+Enter` | Evaluate the editor contents |
| `Ctrl+/` | Show REPL key help |
| `Alt+Up` / `Alt+Down` | Previous / next expression history |
| `Ctrl+Q` | Quit |
| `Ctrl+C` / `Ctrl+X` / `Ctrl+V` | Copy / cut / paste |
| `Ctrl+Z` / `Ctrl+Y` | Undo / redo |
| `Ctrl+A` | Select all |
| `Ctrl+F` / `Ctrl+H` | Find / replace (editor) |

Commands are submitted with `F5`:

- `:help` — key help.
- `:reload` — force a project reload.
- `:quit` / `:q` / `:exit` — quit.

When stdin or stdout is redirected, the CLI falls back to a line-oriented REPL
with persisted history (`~/.wisp_history`) and multiline input.

### Hot reload

The project REPL watches every project directory and `*.wisp.proj` manifest.

- A changed function or value file reloads only the changed files in source
  order, preserving the session.
- A changed type declaration, namespace dependency, manifest, added file, or
  removed file triggers a full project reload with a fresh session.
- A dependency project source change is reloaded in the same way.

Removed definitions leave their old vars in the session until a full reload.

## Packages

- `Wisp.Cli` — dotnet tool, command name `wisp`.
- `Wisp.Sdk` — MSBuild SDK with `WispBuild` and `WispRun` targets.

Both packages publish to GitHub Packages and to the GitHub release for a
`bootstrap-*` tag.
