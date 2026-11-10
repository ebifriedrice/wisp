# About double-pass evaluation

Every Wisp batch uses declaration-oriented passes: a source file, a REPL
submission, or the combined project graph. Functions and types can be referenced
before their definition text in the same batch.

## Why declarations exist in Clojure

Clojure compiles and evaluates each top-level form as it is read. A function body
may mention a var that does not exist yet, but calling it before the defining
form runs fails. Clojure therefore provides `(declare name)` for forward
references.

Wisp applies declaration passes whenever it has a batch of forms. A project has
all files up front; a file or REPL submission has all submitted forms up front.

## The four project phases

### 1. Namespaces

Every source file is read and its `ns` form runs. Imports, aliases, referrals,
and namespace docs are registered. Files are ordered by `:require` and `:use`
dependencies, with project references loaded first.

### 2. Types

`defclass`, `deftype`, `defstruct`, `definterface`, `defdelegate`, and
`defunion` forms run before bodies. A type that refers to a later type fails on
its first attempt and is retried after other declarations have run. This is why
`Holder` can contain a `^Later` field before `Later` exists.

A cycle with no resolvable order eventually stops making progress and reports
the remaining diagnostics.

### Project graph ordering

Project loading adds dependency ordering on top of the same passes. Referenced
projects load first, and files are arranged by `:require`/`:use` namespaces.
The declaration passes themselves are identical to a single-file or REPL
batch.

### 3. Macros and functions

Macros are evaluated before functions so later forms can expand them. Function
definitions are then evaluated. Their bodies are not executed, only compiled,
so a function may reference another function defined later in the source or in
another file.

### 4. Values and remaining forms

`def` values, `defmethod` forms, constructors, and other top-level expressions
run in dependency order. Only after this phase does `wisp run` invoke the
startup function.

## What double-pass does not change

Declaration passes do not make eager initialization order irrelevant:

- A top-level expression that calls a function before that function's `def`
  value has run still sees the value that exists at that moment.
- A macro that expands to a definition is still expanded where it appears.
- A macro body that calls a helper function can only see helpers evaluated
  before the macro is expanded.
- Cyclic project references are rejected; namespace cycles are loaded
  best-effort in source order.

These are runtime ordering facts, not declaration facts. Use explicit function
calls after definitions when initialization order matters.

## Where to go next

- [How to reference another Wisp project](../how-to-guides/reference-a-wisp-project.md)
- [Project file reference](../reference/project-file.md)
- [About exhaustive pattern matching](exhaustive-matching.md)
