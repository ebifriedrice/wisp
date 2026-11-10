# Build a multi-project Wisp application

In this tutorial we will build a small application that depends on a separate
Wisp library project, run it, and use forward references without declaration
forms.

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

(defn later [] (defined-later))

(defn defined-later [] 99)
```

Notice that `later` calls `defined-later` before it is written. A project build
declares all functions before values run, so no `(declare defined-later)` form
is needed.

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

(defclass Holder [^Later value])

(defclass Later [^int n])

(defn main []
  (println "sum:" (math/add 20 22))
  (println "forward:" (helper))
  (println "library-forward:" (math/later))
  (println "type-forward:" (.-n (.-value (Holder {:value (Later {:n 7})})))))

(defn helper []
  "no pre-declaration")
```

`Holder` references `Later` before `Later` is declared, and `main` references
`helper` before `helper` is defined. The project loader resolves both.

## Step 3: Run the application

```console
$ dotnet run --project src/Wisp.Cli --no-build -- run samples/projects/app/app.wisp.proj
sum: 42
forward: no pre-declaration
library-forward: 99
type-forward: 7
```

The output should match exactly.

Notice that the application used `demo.math/add` through its `:require` alias,
and that the library's own forward reference also worked.

## Step 4: Build without running

```console
$ dotnet run --project src/Wisp.Cli --no-build -- build samples/projects/app/app.wisp.proj
Build succeeded: demo.app
```

`build` compiles and loads every project in the graph but does not call the
startup function.

## What we built

We built a two-project Wisp application with a project reference, a
cross-namespace require, and forward references across functions and types.

Continue with [how to reference another Wisp project](../how-to-guides/reference-a-wisp-project.md)
or read the [project file reference](../reference/project-file.md).
