# How to create a project from a template

This page shows you how to scaffold a Wisp application, library, or workspace
from the built-in templates.

## Create an application

```console
$ wisp new hello
$ wisp run hello/wisp.proj
Hello from hello!
```

`wisp new` creates the directory and a project named after it.

## Create a library

```console
$ wisp init my-lib --template lib
$ wisp build my-lib/wisp.proj
Build succeeded: my-lib
```

## Create a workspace with an app and a library

```console
$ wisp init my-app --template workspace
$ wisp run my-app/wisp.proj
Hello, Wisp!
```

The workspace template creates:

```text
my-app/
  wisp.proj              # application project
  src/main.wisp
  lib/
    lib.wisp.proj        # library project
    src/lib.wisp
```

The application project contains the `ProjectReference`.

## List templates

```console
$ wisp templates
app        A console application with a startup function.
lib        A library project with one public function.
workspace  An application project referencing a library project.
```

## Options

- `wisp init [directory]` — scaffold in the current directory or a specific one.
- `wisp new <directory>` — create the directory first.
- `--template app|lib|workspace` — choose the template. Default: `app`.
- `--name <name>` — override the project name and root namespace.
- `--force` — overwrite template files if `wisp.proj` already exists.

## Verify it worked

Run the generated project:

```console
$ wisp run path/to/project/wisp.proj
```

A missing `StartupObject` or an empty source set is reported as a project
diagnostic.

See the [CLI reference](../reference/cli.md) for all commands and the
[project file reference](../reference/project-file.md) for manifest fields.
