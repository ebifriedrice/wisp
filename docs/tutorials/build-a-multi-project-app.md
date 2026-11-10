# Build a multi-project Wisp application

In this tutorial we will build a small application that depends on a separate
Wisp library project, run it, and use `declare` for forward references.

## Prerequisites

- The repository checked out at its root.
- A .NET SDK with .NET 10 or .NET 11 installed.

Run all commands from the repository root. Build the CLI once:

```console
$ dotnet build src/Wisp.Cli/Wisp.Cli.fsproj -v:quiet
```

## Step 1: Create the library project

Create `samples/projects/math/math.wisp.proj`:

```xml
<Project>
  <PropertyGroup>
    <Name>demo.math</Name>
    <RootNamespace>demo.math</RootNamespace>
    <OutputType>Library</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <Wisp Include="src/**/*.wisp" />
  </ItemGroup>
</Project>
```

Now create `samples/projects/math/src/math.wisp`:

```clojure
(ns demo.math "Small math helpers.")

(defn add [a b] (+ a b))

(declare defined-later)

(defn later [] (defined-later))

(defn defined-later [] 99)
```

Evaluation is linear within a namespace. The `declare` form interns
`defined-later` before `later` refers to it.

## Step 2: Create the application project

Create `samples/projects/app/app.wisp.proj`:

```xml
<Project>
  <PropertyGroup>
    <Name>demo.app</Name>
    <RootNamespace>demo.app</RootNamespace>
    <OutputType>Exe</OutputType>
    <StartupObject>demo.app/main</StartupObject>
  </PropertyGroup>
  <ItemGroup>
    <Wisp Include="src/**/*.wisp" />
    <ProjectReference Include="../math/math.wisp.proj" />
  </ItemGroup>
</Project>
```

The `ProjectReference` loads the library project before the application.

Create `samples/projects/app/src/app.wisp`:

```clojure
(ns demo.app
  "The demo application."
  (:require [demo.math :as math]))

(defclass Later [^int n])

(defclass Holder [^Later value])

(declare helper)

(defn main []
  (println "sum:" (math/add 20 22))
  (println "forward:" (helper))
  (println "library-forward:" (math/later))
  (println "type:" (.-n (.-value (Holder {:value (Later {:n 7})})))))

(defn helper []
  "declared before use")
```

Types follow source order, so `Later` comes before `Holder`. `declare` makes the
forward function reference explicit.

## Step 3: Run the application

```console
$ dotnet run --project src/Wisp.Cli --no-build -- run samples/projects/app/app.wisp.proj
sum: 42
forward: declared before use
library-forward: 99
type: 7
```

The output should match exactly.

Notice that the application used `demo.math/add` through its `:require` alias,
and that both projects used `declare` for forward function references.

## Step 4: Build without running

```console
$ dotnet run --project src/Wisp.Cli --no-build -- build samples/projects/app/app.wisp.proj
Build succeeded: demo.app
```

`build` compiles and loads every project in the graph but does not call the
startup function.

## What we built

We built a two-project Wisp application with a project reference, a
cross-namespace require, and explicit `declare` forward references.

Continue with [how to reference another Wisp project](../how-to-guides/reference-a-wisp-project.md)
or read the [project file reference](../reference/project-file.md).
