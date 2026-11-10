# Wisp

A tiny Clojure-shaped Lisp for .NET with persistent data, native CLR interop,
a lossless syntax tree, a nanopass compiler, and a project-aware REPL that hot
reloads changed files.

## 1. Get Wisp

Requirements:

- .NET 10 or .NET 11 SDK.
- A terminal for the TUI REPL.

Build from the repository:

```bash
dotnet build Wisp.slnx
dotnet run --project src/Wisp.Cli -- --version
```

The CLI is also published as the `Wisp.Cli` dotnet tool and the build integration
as the `Wisp.Sdk` MSBuild SDK. Both are attached to GitHub releases and published
to GitHub Packages:

```bash
dotnet tool install --global Wisp.Cli \
  --add-source https://nuget.pkg.github.com/ebifriedrice/index.json \
  --version 0.1.1-bootstrap
wisp --version
```

## 2. Create a project

```bash
wisp new hello
cd hello
wisp run wisp.proj
```

Output:

```text
Hello from hello!
```

Available templates:

```text
app        A console application with a startup function.
lib        A library project with one public function.
workspace  An application project referencing a library project.
```

```bash
wisp init my-lib --template lib
wisp init my-app --template workspace
wisp templates
```

A template creates a `.wisp.proj` manifest, source files, and, for a workspace,
a library project with a `ProjectReference`.

## 3. Project layout

```text
hello/
  wisp.proj
  src/
    main.wisp
```

`wisp.proj`:

```xml
<Project>
  <PropertyGroup>
    <Name>hello</Name>
    <RootNamespace>hello</RootNamespace>
    <OutputType>Exe</OutputType>
    <StartupObject>hello/main</StartupObject>
  </PropertyGroup>
  <ItemGroup>
    <Wisp Include="src/**/*.wisp" />
  </ItemGroup>
</Project>
```

`src/main.wisp`:

```clojure
(ns hello)

(defn main []
  (println "Hello from hello!"))
```

A library project adds `ProjectReference` to the consuming project:

```xml
<ProjectReference Include="../lib/lib.wisp.proj" />
```

Project references load first, share one runtime, and resolve namespaces through
ordinary `:require`/`:use` forms:

```clojure
(ns demo.app
  (:require [demo.math :as math]))

(println (math/add 20 22))
```

## 4. Build, run, and REPL

```bash
wisp build wisp.proj
wisp run wisp.proj
```

From the repository, use `dotnet run --project src/Wisp.Cli --no-build -- ...`.

Start a REPL:

```bash
wisp repl
wisp repl path/to/project/wisp.proj
```

The interactive REPL uses a Terminal.Gui editor with multiline editing, selection,
clipboard, undo/redo, Wisp syntax highlighting, and a read-only transcript.

| Key | Action |
|---|---|
| `F5` / `Ctrl+Enter` | Evaluate the editor contents |
| `Ctrl+/` | Show key help |
| `Alt+Up` / `Alt+Down` | Previous / next expression history |
| `Ctrl+Q` | Quit |
| `Ctrl+C` / `Ctrl+X` / `Ctrl+V` | Copy / cut / paste |
| `Ctrl+Z` / `Ctrl+Y` | Undo / redo |
| `Ctrl+A` | Select all |

When stdin or stdout is redirected, the CLI falls back to a line-oriented REPL
with persisted history (`~/.wisp_history`).

### Hot reload

A project REPL watches every project directory and `*.wisp.proj` manifest:

- changed function or value files reload only those files;
- changed types, namespace dependencies, manifests, added files, or removed
  files trigger a full reload;
- dependency project changes are watched and reloaded too;
- `:reload` forces a refresh.

See [the CLI reference](docs/reference/cli.md) for all commands and REPL keys.

## 5. First program

```clojure
(ns hello)

(def answer 42)

(defn add [a b]
  (+ a b))

(let [x 2
      y (+ x 3)]
  [x y (add x y)])

(println "answer:" answer)
```

Values print as Clojure-shaped data:

```clojure
{:name "Wisp" :versions [10 11]}
#{:a :b}
'(1 2 3)
```

Persistent collections, metadata, keywords, symbols, namespaces, vars, ratios,
lazy sequences, reader conditionals, tagged literals, and syntax quote are
available through `Wisp.Core` and `Wisp.Reader`.

## 6. Functions and control flow

```clojure
(defn fact [n]
  (if (= n 0)
    1
    (* n (fact (- n 1)))))

(loop [i 0 acc 0]
  (if (= i 10)
    acc
    (recur (inc i) (+ acc i))))

(try
  (throw (new System.InvalidOperationException "boom"))
  (catch System.ArgumentException e :argument)
  (catch System.InvalidOperationException e (.-Message e))
  (catch System.Exception e :other)
  (finally (println "cleanup")))
```

`recur` must be in tail position and match its enclosing loop or function arity.
Typed catches match CLR assignability in source order; an untyped catch must be
last.

### Destructuring

```clojure
(let [[a b & rest] [1 2 3 4]]
  [a b rest])

(let [{:keys [a b] :or {b 9} :as m} {:a 1}]
  [a b m])

(defn add-pair [[a b]]
  (+ a b))
```

Vector and map patterns work in `let`, function parameters, and `loop`
bindings, including nesting, `&`, `:as`, `:keys`, `:strs`, `:syms`, and `:or`.

## 7. Macros

```clojure
(defmacro unless [test then else]
  `(if ~test ~else ~then))

(unless false "no" "yes")

(defmacro all [& forms]
  `(do ~@forms))
```

Syntax quote expands at read time to builder forms and supports `~`, `~@`, and
auto-gensyms (`x#`). Macros receive raw forms, expand recursively, and retain
invocation spans; failures carry an expansion trace.

## 8. Namespaces and declarations

```clojure
(ns demo.lib "A demo library.")
(def answer 42)

(ns demo.app
  "The demo application."
  (:import [System.Text StringBuilder])
  (:require [demo.lib :as lib :refer [answer]]))

(def sb (StringBuilder. "wisp"))
(lib/answer)
answer

(ns demo.uses (:use demo.lib))
(ns demo.only (:use [demo.lib :only [answer]]))
```

Imports and aliases are namespace-scoped. `:refer` and `:use` add referrals;
`:use` skips `^:private` vars. `(in-ns 'demo.other)` switches the session
namespace.

Project loading uses declaration-oriented passes over all source files:
namespaces first, then types (with retries for forward type references), then
macros and functions, then values and methods. No `declare` form is needed for
forward references across files or projects.

## 9. Documentation in the language

Definition forms accept Markdown docstrings:

```clojure
(ns demo.app "The **demo** application.")

(defn add
  "Adds **two** numbers.

  See `sub` for subtraction."
  [a b]
  (+ a b))

(defclass Person "A person with a name." [^String name])
```

Docs are stored as `:doc` metadata. Method, constructor, and namespace docs are
stored on their runtime entries. `doc` and `meta` accept bare symbols:

```clojure
(doc add)
(meta add)
(doc Person.greet)
(doc demo.app)
(doc defn)    ; built-in form docs with usage examples
```

Backticked names and Markdown links become references:

```clojure
(defn multiply
  "Multiplies two numbers. See `add` and [division](doc:divide)."
  [a b]
  (* a b))
```

`doc` renders Markdown in the REPL and prints resolved references under
`See also`.

## 10. Pattern matching

```clojure
(defunion Shape
  (Circle [radius])
  (Rect [w h])
  (Empty))

(def c (Circle {:radius 3}))

(match c
  ((Circle r) (* r r))
  ((Rect w h) (* w h))
  ((Empty) 0))
; => 9
```

Patterns include literals, bindings, `_`, vectors with `&`/`:as`, maps with
`:keys`/`:or`/`:as`, type tests (`^Circle c`), union cases, active patterns, and
`(or ...)`. Clauses are `(pattern body...)` with an optional `:when` guard.

```clojure
(defactive even? [n]
  (when (= 0 (mod n 2)) n))

(match n
  ((active even? x) (* x 2))
  (_ -1))
```

`match` checks exhaustiveness for finite domains:

- every union case must be covered;
- booleans must cover `true` and `false`;
- `_`, bindings, `(or ...)`, typed booleans, whole-union hints, and typed union
  cases count as coverage;
- guarded clauses do not count.

Missing cases report `WISP3044` with the missing names. See
[About exhaustive pattern matching](docs/explanation/exhaustive-matching.md).

## 11. Types, records, and interfaces

```clojure
(definterface IShape
  (^double Area [this]))

(defclass Circle [^double radius]
  (implements IShape))

(defstruct Point [^double x ^double y])

(defmethod IShape.Area [^Circle this]
  (* System.Math/PI (.-radius this) (.-radius this)))

(defmethod Circle.Diameter [^Circle this]
  (* 2.0 (.-radius this)))

; Receiver hint infers the owning class.
(defmethod Circumference [^Circle this]
  (* 2.0 System.Math/PI (.-radius this)))
```

- `Circle.Area` applies to the class; `IShape.Area` applies to the interface and
  uses the `^Circle` receiver hint to join the implementation to `Circle`.
- Methods are public by default; `^:private` rejects calls from other namespaces.
- Interface members with a receiver first parameter exclude it from the emitted
  CLR signature.
- `defstruct` emits a real CLR value type with value equality, printing, methods,
  and interface dispatch; it cannot inherit.

Constructors use the class/struct name:

```clojure
(defclass Pair [a b])

(def p (Pair {:a 1 :b 2}))     ; default field-map constructor

(defmethod Pair [a b] {:a a :b b})
(defmethod Pair [a] {:a a :b (* a 2)})

(Pair 3 4)
(Pair 5)
```

Fields and generics use C#-shaped hints:

```clojure
(defclass [Box T (where [IComparable T])]
  [^T value])

(def b (new [Box Int] 42))

(defclass [Assoc K V (where [IComparable K])]
  [^[Dictionary [K V]] data])
```

`(inherits Base)` supports a custom `WispRecordBase` subtype exposing a
`(string, string[], obj[])` constructor. `deftype` is a deprecated alias for
`defclass`; explicit `(defmethod Type :name ...)` targets, keyword-headed hints,
and `->Type` constructors remain supported.

### Byref interface parameters

```clojure
(definterface IRefs
  (^int Bump [this ^int& value])
  (^bool TryGet [this ^:out ^int& value])
  (^int Peek [this ^:readonly ^int& value]))
```

`^Int32&` is byref notation; `^:out` and `^:readonly` add CLR `Out`/`In`
metadata. Writable byref methods return `[result out1 ref1 ...]`; the stub writes
the tail values back and returns the head.

## 12. Delegates and events

```clojure
(defdelegate ^int IntUnary [^int x])

(def twice (delegate IntUnary (fn [x] (* x 2))))
(twice 21)       ; 42

(set! twice (delegate IntUnary (fn [x] (+ x 1))))
(twice 41)       ; 42

(set! twice nil) ; disassociate
```

Wisp functions adapt automatically to CLR delegate parameters, record fields,
and event handlers. CLR events use their `add_`/`remove_` accessors:

```clojure
(.add_Ping source handler)
(.RaisePing source)
(.remove_Ping source handler)
```

Delegate signatures support Wisp-defined types, byref parameters, and pointers:

```clojure
(defstruct Point [^int x])
(defdelegate ^Point MovePoint [^Point p])

(defdelegate ^int RefFn [^int& x ^:out ^int& y])
(defdelegate ^int ReadInt [^Int32* p])
```

Writable byref parameters use the `[result out1 ref1 ...]` convention; pointer
parameters are exposed as `IntPtr`.

## 13. Async and await

```clojure
(defn work [n]
  (async
    (println "start" n)
    (await (System.Threading.Tasks.Task/Delay 10))
    (println "after" n)
    (+ n 1)))

(def task (work 41))
(await task)    ; 42
```

`await` also works outside `async`, where it waits synchronously:

```clojure
(await (async 42))
```

Async bodies return `Task<obj>` by default. A return-type hint selects a value
task:

```clojure
(def vt (^ValueTask async (await (System.Threading.Tasks.Task/Delay 1)) 42))
(await vt) ; nil

(def typed (^[ValueTask Int] async 42))
(await typed) ; 42
```

`await` accepts generic and non-generic `Task` and `ValueTask`.

On .NET 11 the Reflection.Emit backend emits runtime-async methods
(`MethodImplAttributes.Async` plus `AsyncHelpers.Await`). On .NET 10, and for
the interpreter, async bodies compile to Task continuation chains.
`Session(forceCompilerAsync = true)` selects the compiler transform.

## 14. CLR interop

```clojure
(System.Math/Abs -5)
(System.Math/PI)
(System.String/Concat "a" "b")

(def builder (System.Text.StringBuilder. "wisp"))
(.Append builder " + CLR")
(println (.ToString builder))

(def numbers (new [List Int]))
(.Add numbers 10)
(.-Count numbers)
```

Type hints use C#-shaped symbols; generic arguments may be flat or grouped:

```clojure
^int
^[List [IComparable]]
^[List [List [IComparable]]]
^[Dictionary [String Int]]
^[array {:rank 2} int]
^[byref int]
```

`TypeHints.resolve` constructs reified generics with `MakeGenericType`, arrays,
pointers, and byrefs. Keyword-headed hints are accepted but deprecated.

## 15. Packages and build integration

- **Wisp.Cli** — dotnet tool, command name `wisp`.
- **Wisp.Sdk** — MSBuild SDK with `WispBuild`, `WispRun`, and `Wisp` targets.
- **GitHub releases** — `bootstrap-*` tags attach both `.nupkg` files.
- **GitHub Packages** — both packages publish to the repository's NuGet feed.

Use the SDK from a `.wisp.proj`:

```xml
<Project Sdk="Wisp.Sdk">
  <PropertyGroup>
    <Name>demo.app</Name>
    <RootNamespace>demo.app</RootNamespace>
    <StartupObject>demo.app/main</StartupObject>
  </PropertyGroup>
</Project>
```

```bash
dotnet msbuild app.wisp.proj -t:WispRun -p:WispTool=wisp
```

See [How to use the MSBuild SDK](docs/how-to-guides/use-the-msbuild-sdk.md).

## 16. Architecture

- **Wisp.Core** — persistent collections, lazy sequences, `IFn`/`AFn`, keywords,
  symbols, metadata, namespaces/vars, diagnostics, printing, type hints.
- **Wisp.Reader** — Clojure-shaped reader with source metadata on every
  metadata-capable form.
- **Wisp.Syntax** — lossless green/red syntax tree with trivia and incremental
  top-level reparse.
- **Wisp.Compiler** — reader forms → macro expansion → Core IR → validation →
  optimization → interpreter or Reflection.Emit.
- **Wisp.Cli** — file runner, project runner, template scaffolding, TUI/line
  REPL, and hot reload.

Compiler layers:

1. Surface forms with metadata and source ranges.
2. Macro expansion with expansion traces.
3. Core IR: `Const`, `Local`, `VarRef`, `If`, `Do`, `Let`, `Lambda`, `Call`,
   `Def`, `Quote`, namespace ops, CLR call/property/construct nodes, type and
   method definitions, `Async`, `Await`, `Loop`, `Recur`, `Try`, and `Throw`.
4. `recur` tail-position and arity validation.
5. Constant folding, `do` flattening, and identity-let elimination.
6. Interpreter or emitted IL in a collectible dynamic assembly. Lambdas become
   generated `AFn` subclasses; async bodies use runtime async on .NET 11 and a
   continuation transform on .NET 10.

## 17. Samples

- `samples/hello.wisp` — functions, recursion, printing
- `samples/interop.wisp` — static/instance CLR calls and generic construction
- `samples/macros.wisp` — macro expansion
- `samples/reader.wisp` — reader conditionals and syntax quote
- `samples/control.wisp` — loop/recur and try/catch/finally
- `samples/destructuring.wisp` — let/fn/loop patterns
- `samples/records.wisp` — classes, structs, interfaces, methods, constructors
- `samples/matching.wisp` — unions, pattern matching, active patterns
- `samples/namespaces.wisp` — imports, requires, in-ns
- `samples/delegates.wisp` — delegate declarations, assignment, invocation
- `samples/async.wisp` — async/await, tasks, and value tasks
- `samples/projects/app` — multi-project application with a library reference

Run any single-file sample:

```bash
wisp samples/records.wisp
```

## 18. Current limits

- `definterface` does not accept a generic header or default members yet.
- Delegate return-byref signatures are not supported. `set!` supports vars only,
  not mutable locals or record fields.
- Wisp-defined events are not emitted yet; CLR events use add/remove accessors.
- The .NET 10 async transform does not support `await` inside `loop`/`recur` or
  `finally`; the .NET 11 runtime-async path does.
- Byref writeback uses the vector convention above; return-byref signatures and
  call-site `(ref ...)` / `(out ...)` / `(in ...)` forms are not implemented.
- `:refer` supports name vectors or `:all`; arbitrary `:rename`/`:exclude`
  options are not implemented.
- Constructor overload selection uses arity plus type hints; C# exact overload
  ranking and implicit numeric ranking are not implemented.
- `defunion` cases cannot declare type parameters.
- `or` patterns cannot bind names, and active patterns bind one result per call.
- Typed record fields support primitives, arrays, dictionaries, and common
  generic collections; other CLR collection types fall back to exact/assignable
  conversion.
- Project libraries are loaded from source. The compiler does not emit reusable
  `.dll` files yet, and Wisp source libraries are not consumed through
  `PackageReference`.
- The compiler consumes reader values with source metadata rather than lowering
  directly from the syntax tree.
- The TUI captures Wisp session output; direct `Console` writes from CLR interop
  can paint over the screen.

## Documentation

- [Documentation index](docs/README.md)
- [Build a multi-project application](docs/tutorials/build-a-multi-project-app.md)
- [Create a project from a template](docs/how-to-guides/create-a-project-from-a-template.md)
- [Reference another project](docs/how-to-guides/reference-a-wisp-project.md)
- [Use the MSBuild SDK](docs/how-to-guides/use-the-msbuild-sdk.md)
- [CLI and REPL reference](docs/reference/cli.md)
- [Project file reference](docs/reference/project-file.md)
- [About double-pass project loading](docs/explanation/double-pass-projects.md)
- [About exhaustive pattern matching](docs/explanation/exhaustive-matching.md)
