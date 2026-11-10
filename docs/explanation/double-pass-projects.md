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

### 2. Macro definitions

`defmacro` forms run before ordinary forms so subsequent forms can use them.
Macros receive raw forms, so this phase does not depend on type resolution.

### 3. Macro expansion and classification

Every remaining form is macro-expanded. Classification happens on the expanded
form, so a macro that expands to `def`, `defn`, `defclass`, `defunion`, or
another definition participates in the normal declaration passes. A macro that
expands to another `defmacro` is evaluated as encountered.

Resolution runs after this expansion step. Type hints, var references, and
special forms are resolved against the expanded form.

### 4. Types

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

### 5. Functions

Function definitions and active patterns are evaluated after types. Their bodies
are not executed, only compiled, so a function may reference another function
defined later in the source or in another file.

### 6. Values and remaining forms

`def` values, `defmethod` forms, constructors, and other top-level expressions
run in dependency order. Only after this phase does `wisp run` invoke the
startup function.

## What double-pass does not change

Declaration passes do not make eager initialization order irrelevant:

- A top-level expression that calls a function before that function's `def`
  value has run still sees the value that exists at that moment.
- A macro body that calls a helper function can only see helpers evaluated
  before the macro is expanded.
- A macro used earlier in the same batch must be defined before that use.
- Cyclic project references are rejected; namespace cycles are loaded
  best-effort in source order.

These are runtime ordering facts, not declaration facts. Use explicit function
calls after definitions when initialization order matters.

## Where to go next

- [How to reference another Wisp project](../how-to-guides/reference-a-wisp-project.md)
- [Project file reference](../reference/project-file.md)
- [About exhaustive pattern matching](exhaustive-matching.md)
