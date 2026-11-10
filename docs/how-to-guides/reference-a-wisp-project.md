# How to reference another Wisp project

This page shows you how to add a library project to an application project and
make its namespaces available through `:require`.

## Add the project reference

In the consuming project's `*.wisp.proj`, add a `ProjectReference` item under an
`ItemGroup`:

```xml
<ItemGroup>
  <Wisp Include="src/**/*.wisp" />
  <ProjectReference Include="../math/math.wisp.proj" />
</ItemGroup>
```

The path is relative to the project file, not to the current shell directory.

## Require the library namespace

Add the namespace to the consuming source file:

```clojure
(ns demo.app
  (:require [demo.math :as math]))

(println (math/add 20 22))
```

Use `:refer [name]` if you want the library vars unqualified. Use `:use` for the
whole public namespace.

## Verify the graph

Build the application project:

```console
$ dotnet run --project src/Wisp.Cli --no-build -- build path/to/app.wisp.proj
Build succeeded: demo.app
```

A missing reference, a bad namespace, or a project cycle is reported as a
diagnostic and exits non-zero.

## Resolve order

Referenced projects load before the project that references them. Within the
combined source set, namespaces load in `:require`/`:use` dependency order, so a
library namespace can itself require another library project.

If you need to control the exact evaluation order of two independent projects,
list both through a common application project rather than relying on the file
system order.

See the [project file reference](../reference/project-file.md) for every manifest
element and the [evaluation-order explanation](../explanation/double-pass-projects.md)
for how namespaces and `declare` interact.
