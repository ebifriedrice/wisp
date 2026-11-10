# About evaluation order and declarations

Wisp evaluates forms linearly within a namespace. Cross-namespace order is
determined by dependencies, not by the order files happen to be passed to the
CLI.

## Within a namespace

A file, REPL submission, or namespace body runs in source order:

1. Each form is macro-expanded.
2. The expanded form is resolved and compiled.
3. The form is evaluated.
4. The next form runs.

This means types, macros, values, and methods must be defined before they are
used. The compiler does not collect all definitions from a namespace and run
them in dependency order.

The REPL keeps the same model. A submission runs forms in order, and later
submissions see the vars created by earlier ones.

## Forward references and `declare`

Function bodies are not executed when they are defined, so a function can refer
to a var that is defined later, as long as the call happens after the definition.
For explicit forward references and mutual recursion, use `declare`:

```clojure
(declare even? odd?)

(defn even? [n]
  (if (= n 0) true (odd? (- n 1))))

(defn odd? [n]
  (if (= n 0) false (even? (- n 1))))
```

`declare` interns the names at that source position without overwriting existing
bindings. It is a normal form, not a hidden batch pre-pass.

## Across namespaces and projects

`ns` declarations describe namespace dependencies:

```clojure
(ns demo.app
  (:require [demo.math :as math]))
```

Project loading uses those edges, plus `ProjectReference` edges, to order files.
A referenced project loads before its consumer, and a required namespace loads
before the namespace that requires it. The order of files on disk or in the
manifest does not decide evaluation order across namespaces.

Within each namespace, the linear rules above apply.

## Macros

Macros expand before the expanded form is resolved. This lets a macro produce a
`def`, `defn`, `defclass`, or another form, and that expanded form is then
compiled normally.

A macro itself must be defined before it is used in the same namespace. Macro
bodies run at expansion time, so helper functions called by a macro must already
be defined when that macro expands.

## What this means in practice

- Define types before fields, methods, or functions that name them.
- Define macros before forms that use them.
- Use `(declare ...)` for explicit forward vars and mutual recursion.
- Use `:require` and project references to express cross-namespace order.
- Project loaders order namespaces; they do not make a namespace's own body
  order-independent.

## Where to go next

- [How to reference another Wisp project](../how-to-guides/reference-a-wisp-project.md)
- [Project file reference](../reference/project-file.md)
- [About exhaustive pattern matching](exhaustive-matching.md)
