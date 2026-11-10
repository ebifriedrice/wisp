# Project file reference

A Wisp project manifest is an XML file ending in `.wisp.proj`. It describes
source files, a startup function, and project dependencies.

## Project element

```xml
<Project>
  ...
</Project>
```

With the MSBuild SDK:

```xml
<Project Sdk="Wisp.Sdk">
  ...
</Project>
```

## PropertyGroup

### `Name`

Default: the project file name without `.wisp.proj` or `.proj`.

The display name used in build output and as the default root namespace.

### `RootNamespace`

Default: `Name`.

The namespace assumed for source files that do not contain an `ns` form. Also
the namespace used when `StartupObject` has no namespace part.

### `OutputType`

Default: `Library`.

`Library` or `Exe`. The project runner always loads libraries; `wisp run` uses
the root project's startup function regardless of `OutputType`.

### `StartupObject`

Default: `<RootNamespace>/main`.

Format: `namespace/name` or `name`. `wisp run` resolves the var and invokes it
with zero arguments.

Example:

```xml
<StartupObject>demo.app/main</StartupObject>
```

## ItemGroup

### `Wisp`

Include one or more source patterns:

```xml
<Wisp Include="src/**/*.wisp" />
<Wisp Include="scripts/*.wisp" />
```

Patterns are relative to the project file. `*` matches one path segment, `?`
matches one character, and `**` matches zero or more segments. The default is
`**/*.wisp`.

`bin`, `obj`, `.git`, and `node_modules` directories are excluded.

### `ProjectReference`

```xml
<ProjectReference Include="../math/math.wisp.proj" />
```

Paths are relative to the project file. Referenced projects load first, and all
projects share one namespace registry. Project cycles are reported as errors.

## Evaluation order

Project references are loaded depth-first, dependencies first. Namespaces are
ordered by `:require` and `:use` clauses.

Within a namespace, forms run linearly in source order: macro-expand, resolve,
evaluate, then continue. Types, macros, values, and methods must be defined
before use. Use `(declare ...)` for forward vars and mutual recursion; see
[About evaluation order and declarations](../explanation/double-pass-projects.md).

## CLI commands

### `wisp build [project]`

Loads the project graph and evaluates declarations and values. Does not invoke a
startup function. Prints `Build succeeded: <name>` on success.

`project` may be a `.wisp.proj` file or a directory containing `wisp.proj` or
one `*.wisp.proj` file. The default is the current directory.

### `wisp run [project]`

Performs the build steps, then invokes `StartupObject`. Prints a non-nil return
value.

### `wisp <file.wisp>`

Evaluates one file with the REPL session. This does not use the project loader.

### `wisp --eval <form>`

Evaluates one form and prints its value.

### `wisp`

Starts the REPL.

## Exit codes

- `0` — success.
- `1` — evaluation, project, or startup diagnostics.
- `2` — usage error.

Diagnostics are rendered with file, line, and column where a source span exists.

## MSBuild SDK

Package: `Wisp.Sdk`.

### Properties

| Property | Default | Meaning |
|---|---|---|
| `WispTool` | `wisp` | Tool command or full path used by the targets. |
| `WispProject` | `$(MSBuildProjectFullPath)` | Manifest passed to the tool. |

### Targets

| Target | Behavior |
|---|---|
| `WispBuild` | Runs `wisp build`. |
| `WispRun` | Depends on `WispBuild`; runs `wisp run`. |
| `Wisp` | Depends on `WispRun`. |

The SDK package is `Wisp.Sdk` version `0.1.5-bootstrap`. The CLI tool package is
`Wisp.Cli` version `0.1.5-bootstrap` with command name `wisp`.

## Current package limits

- A project build loads source files; it does not emit a reusable `.dll`.
- `ProjectReference` is the supported library dependency mechanism.
- NuGet packages currently ship the CLI tool and MSBuild SDK. Wisp source
  libraries are not yet consumed through `PackageReference`.
