# Wisp

A tiny Clojure-shaped Lisp for .NET with native CLR interop, persistent data
structures, a lossless incremental syntax tree, a nanopass compiler, and a
REPL that hot-reloads definitions.

## Quick start

```bash
dotnet test Wisp.slnx
dotnet run --project src/Wisp.Cli -- --eval "(+ 1 2 3)"
dotnet run --project src/Wisp.Cli -- samples/hello.wisp
```

REPL:

```bash
dotnet run --project src/Wisp.Cli --
```

Single-file framework-dependent publish:

```bash
dotnet publish src/Wisp.Cli/Wisp.Cli.fsproj -c Release -r linux-x64 \
  --self-contained false /p:PublishSingleFile=true -o artifacts/publish
artifacts/publish/wisp --eval "(System.Math/Abs -42)"
```

## Projects

Wisp projects are XML manifests ending in `.wisp.proj`:

```xml
<Project>
  <PropertyGroup>
    <Name>demo.app</Name>
    <RootNamespace>demo.app</RootNamespace>
    <StartupObject>demo.app/main</StartupObject>
  </PropertyGroup>
  <ItemGroup>
    <Wisp Include="src/**/*.wisp" />
    <ProjectReference Include="../math/math.wisp.proj" />
  </ItemGroup>
</Project>
```

```bash
dotnet build src/Wisp.Cli/Wisp.Cli.fsproj
dotnet run --project src/Wisp.Cli --no-build -- build samples/projects/app/app.wisp.proj
dotnet run --project src/Wisp.Cli --no-build -- run   samples/projects/app/app.wisp.proj
```

Scaffold a new project from a built-in template:

```bash
wisp new hello
wisp init my-lib --template lib
wisp init my-app --template workspace
wisp templates
```

Start a project-aware REPL:

```bash
wisp repl samples/projects/app/app.wisp.proj
```

The TUI REPL uses a real multiline editor with clipboard, undo/redo, syntax
highlighting, history, and `F5` evaluation. It watches project sources and
manifests, reloading changed function files in place and rebuilding the session
when types or the project graph change.

Project loading uses a declaration pass for namespaces, types, macros, and
functions, so no `declare` forms are needed for forward references. Referenced
projects load first and share one runtime. The CLI ships as the `Wisp.Cli`
dotnet tool, and `Wisp.Sdk` provides MSBuild targets for `dotnet msbuild`.

See [the documentation index](docs/README.md) for tutorials, how-to guides,
reference, and explanations.

## Language examples

```clojure
(ns demo.app)

(defn fact [n]
  (if (= n 0)
    1
    (* n (fact (- n 1)))))

(println "6! =" (fact 6))

(let [x 2
      y (+ x 3)]
  [x y (* x y)])

(defmacro unless [test then else]
  (list 'if test else then))

(println (unless false "no" "yes"))
```

Persistent collections, metadata, keywords, namespaces, ratios, lazy sequences,
reader macros, and tagged literals are available through `Wisp.Core` and
`Wisp.Reader`.

## Destructuring

Vector and map patterns work in `let`, `fn`/`defn` parameters, and `loop`
bindings:

```clojure
(let [[a b & rest] [1 2 3 4]]
  [a b rest])

(let [{:keys [a b] :or {b 9} :as m} {:a 1}]
  [a b m])

(defn add-pair [[a b]]
  (+ a b))

(loop [[i acc] [0 0]]
  (if (= i 4) acc (recur [(inc i) (+ acc i)])))
```

Patterns can nest, use `&` rest bindings and `:as`, and map patterns support
`:keys`, `:strs`, `:syms`, `:or`, and `:as`. Invalid patterns report `WISP3029`
with a source span.

## Macros

```clojure
(defmacro unless [test then else]
  `(if ~test ~else ~then))

(unless false "no" "yes")

(defmacro all [& forms]
  `(do ~@forms))
```

Loop/recur and try/catch:

```clojure
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

(defn factorial [n acc]
  (if (= n 0)
    acc
    (recur (dec n) (* acc n))))
```

`recur` is validated to be in tail position and rebinds either the enclosing
loop or function parameters.

Catch clauses are typed with `(catch Type binding body...)` or untyped with
`(catch binding body...)`. Typed clauses match by CLR assignability in source
order, so the first matching clause wins; `Exception` matches everything. An
untyped catch catches all remaining exceptions and must be the last catch
clause (`WISP3031` otherwise). Catch types must resolve to an exception type,
reported as `WISP3030` with the type form's source span. `throw` rethrows a
given `Exception` object unchanged, preserving its runtime type.

Syntax quote expands at read time to builder forms using `concat`, `list`,
`apply`, `vector`, `hash-map`, and `hash-set`. It supports `~`, `~@`, and
auto-gensyms (`x#`). Nested syntax quotes expand inside-out: the inner expansion
becomes data for the outer quote, and `~~` escapes both levels. Map templates
support `~@` at the flat key/value argument level, so a splice can supply a
value and the following key. Reader conditionals select on the configured
feature set (`:wisp` and `:clr` by default) with `:default` fallback; `#?@`
splices inside lists, vectors, maps, and sets. Splicing also works after reader
prefixes: quote, var-quote, deref, and unquote distribute over each selected
item, metadata applies to every item, syntax quote expands each item, and
discard drops them all.

Macros receive raw forms, expand recursively, and retain invocation spans. Macro
failures and compiler errors after expansion carry an expansion trace in their
notes (`macro user/unless: ... => ...`). Macro vars can be redefined in the REPL.

## Definitions and methods

`defstruct`, `defclass`, and `definterface` declare types. Type hints use
C#-shaped symbols, not keywords:

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

; Receiver hint infers the owning class when no target is given.
(defmethod Circumference [^Circle this]
  (* 2.0 System.Math/PI (.-radius this)))
```

- `Circle.Area` applies to the class; `IShape.Area` applies to the interface and
  uses the `^Circle` receiver hint to join the implementation to `Circle`.
- Method visibility is `^:private` or public by default. Private methods are
  rejected when called from another namespace.
- Interface members declared with a receiver first parameter (`[this ...]`)
  exclude that receiver from the emitted CLR signature.

Constructors use the class/struct name itself:

```clojure
(defclass Pair [a b])

; Default constructor takes a field map.
(def p (Pair {:a 1 :b 2}))

; Override with one or more constructor defmethods. The body returns a field map.
(defmethod Pair [a b] {:a a :b b})
(defmethod Pair [a] {:a a :b (* a 2)})

(Pair 3 4)   ; matches the two-argument constructor
(Pair 5)     ; matches the one-argument constructor
```

Fields can be typed, and generic definitions use a C#-shaped header:

```clojure
(defclass [Box T (where [IComparable T])]
  [^T value])

(def b (new [Box Int] 42))
(.-value b)        ; System.Int32

(defclass [Assoc K V (where [IComparable K])]
  [^[Dictionary [K V]] data])

(defclass Holder [^[List [IComparable]] items])
```

`(inherits Base)` supports a custom `WispRecordBase` subtype exposing a
`(string, string[], obj[])` constructor; invalid bases report `WISP3032`.
`defstruct` emits a real CLR value type (`System.ValueType`) with value
equality, printing, methods, and interface dispatch; it cannot inherit. Invalid
interfaces report `WISP3033`.

The old `deftype` is now an alias for `defclass`; explicit
`(defmethod Type :name ...)` targets, keyword-headed hints (`[:List :Int]`), and
`->Type` constructors remain supported but are deprecated.

## Documentation

Definition forms accept Markdown docstrings after the name. The REPL renders
headings, emphasis, lists, code blocks, and links; other outputs render the same
text without ANSI styling.

````clojure
(ns demo.app "The **demo** application.")

(def answer "The answer to everything." 42)

(defn add
  "Adds **two** numbers.

  ## Details

  See `sub` for subtraction.

  ```clojure
  (add 1 2)
  ```"
  [a b]
  (+ a b))

(defclass Person "A person with a name." [^String name])

(defstruct Point "A 2D point." [^int x ^int y])

(definterface Greeter "Something that can greet."
  (^String Greet [this]))

(defdelegate ^int IntUnary "An int transform." [^int x])

(defunion Shape "A shape."
  (Circle "A circle." [radius])
  (Square [side]))

(defmethod Person :greet "Greets the person." [^Person this]
  (str "Hi " (.-name this)))
````

Docs are stored as `:doc` metadata on the defined var or type var. Method docs
are stored on the runtime method entry; constructor docs are stored on the
constructor entry; namespace docs live on the namespace. Both `doc` and `meta`
accept a bare symbol (or a quoted one):

```clojure
(doc add)     ; renders the Markdown docstring
(meta add)    ; {:doc "Adds **two** numbers. ..." ...}
(doc Person.greet)
(doc demo.app)
(doc defn)    ; built-in form docs, including a usage example
```

Inside docstrings, backticked names and Markdown links become references:

```clojure
(defn multiply
  "Multiplies two numbers. See `add` and [division](doc:divide)."
  [a b]
  (* a b))
```

References are resolved against vars, types, methods, namespaces, and built-in
forms; `doc` prints them under `See also`. Elixir-style arity suffixes such as
`` `add/2` `` resolve to `add`.

## Namespaces

`ns` follows Clojure CLR conventions for imports and requires:

```clojure
(ns demo.lib)
(def answer 42)

(ns demo.app
  (:import [System.Text StringBuilder])
  (:require [demo.lib :as lib :refer [answer]]))

(def sb (StringBuilder. "wisp"))
(lib/answer)        ; qualified
answer              ; referred

(ns demo.uses (:use demo.lib))
(ns demo.only (:use [demo.lib :only [answer]]))
```

Imports are namespace-scoped: an alias imported in `demo.app` does not resolve
from another namespace. Required aliases resolve in qualified symbols and
auto-resolved keywords (`::thing`). `:refer` and `:use` add var referrals;
`:use` skips `^:private` vars. `(in-ns 'demo.other)` switches the session
namespace.

Interface parameters can declare CLR byref modifiers:

```clojure
(definterface IRefs
  (^int Bump [this ^int& value])
  (^bool TryGet [this ^:out ^int& value])
  (^int Peek [this ^:readonly ^int& value]))
```

`^Int32&` is byref type notation; `^:out` and `^:readonly` add CLR `Out`/`In`
parameter metadata (plain `&` is a writable ref). The `&:out` / `&:readonly`
suffix forms are accepted as well. A method with writable byref parameters
returns a vector `[result out1 ref1 ...]`; the stub writes the tail values back
and returns the head.

## Pattern matching, unions, and active patterns

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
`:keys`/`:or`/`:as`, type tests (`^Circle c`), union cases (`(Circle r)`), active
patterns, and `(or ...)`. Clauses are `(pattern body...)` with an optional
`:when` guard:

```clojure
(defactive even? [n]
  (when (= 0 (mod n 2)) n))

(match n
  ((active even? x) (* x 2))
  (_ -1))
```

`defunion` emits a marker interface plus one class per case; cases use normal
map/positional constructors and can carry typed fields. Active pattern
functions return nil for no match or a value for a successful bind.

`match` checks exhaustiveness for finite domains. A union match must cover every
case, and a boolean match must cover `true` and `false`, unless a `_`, binding,
`(or ...)`, or typed catch-all covers the rest. Guards do not count as coverage.
Missing cases report `WISP3044` with the missing names. See
[About exhaustive pattern matching](docs/explanation/exhaustive-matching.md).

## Delegates and events

```clojure
(defdelegate ^int IntUnary [^int x])

(def twice (delegate IntUnary (fn [x] (* x 2))))
(twice 21)       ; 42

(set! twice (delegate IntUnary (fn [x] (+ x 1))))
(twice 41)       ; 42

(set! twice nil) ; disassociate
```

Wisp functions automatically adapt to CLR delegate parameters, including record
fields and event handlers. CLR events are wired through their `add_`/`remove_`
accessors:

```clojure
(.add_Ping source handler)
(.RaisePing source)
(.remove_Ping source handler)
```

`defdelegate` emits a real CLR delegate type; `delegate` wraps an `IFn`, and
delegates are callable directly from Wisp.

Delegate signatures may use Wisp-defined types, byref parameters, and pointer
parameters:

```clojure
(defstruct Point [^int x])
(defdelegate ^Point MovePoint [^Point p])

(defdelegate ^int RefFn [^int& x ^:out ^int& y])
(defdelegate ^int ReadInt [^Int32* p])
```

Writable byref delegate parameters use the same `[result out1 ref1 ...]`
writeback convention as interface methods; pointer parameters are exposed to
Wisp as `IntPtr`.

## Async and await

`async` runs a body as a `Task<obj>` computation; `await` suspends until a task
or value task completes:

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
task instead:

```clojure
(def vt (^ValueTask async (await (System.Threading.Tasks.Task/Delay 1)) 42))
(await vt) ; nil

(def typed (^[ValueTask Int] async 42))
(await typed) ; 42
```

`await` accepts both generic and non-generic `ValueTask` values.

On .NET 11 the Reflection.Emit backend emits runtime-async methods
(`MethodImplAttributes.Async` plus `AsyncHelpers.Await`). On .NET 10, and for
the interpreter, async bodies are transformed into Task continuation chains.
`Session(forceCompilerAsync = true)` selects the compiler transform explicitly;
this is useful for exercising the .NET 10 path on a newer runtime.

## Native CLR interop

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

Type hints use C#-shaped symbol types. Generic arguments may be flat or grouped:

```clojure
^int
^[List [IComparable]]
^[List [List [IComparable]]]
^[Dictionary [String Int]]
^[array {:rank 2} int]
^[byref int]
```

`TypeHints.resolve` constructs reified generic CLR types with
`MakeGenericType`, arrays, pointers, and byrefs. Keyword-headed hints are still
accepted for compatibility but deprecated.

## Architecture

- **Wisp.Core** — persistent list/vector/map/set, lazy sequences, `IFn`/`AFn`,
  keywords, symbols, metadata, namespaces/vars, diagnostics, printing, Hiccup
  type hints.
- **Wisp.Reader** — Clojure-shaped data reader. Every metadata-capable form gets
  `:wisp/start`, `:wisp/end`, `:wisp/line`, and `:wisp/column` metadata.
- **Wisp.Syntax** — lossless green/red syntax tree with trivia and a
  rust-analyzer-style incremental top-level reparse. The reparser reuses green
  nodes and agrees with full reparses across randomized edits.
- **Wisp.Compiler** — reader forms → macro expansion → core IR → optimized core
  IR → interpreter or Reflection.Emit execution. `Session` owns one persistent
  collectible dynamic assembly and adds an evaluator type per form.
- **Wisp.Cli** — file runner, `--eval`, and multiline REPL.

## Compiler layers

1. Surface forms: reader data, including metadata and source ranges.
2. Macro expansion: `defmacro` vars are invoked on raw forms, with expansion
   traces attached to diagnostics.
3. Core IR: `Const`, `Local`, `VarRef`, `If`, `Do`, `Let`, `Lambda`, `Call`,
   `Def`, `Quote`, `SetNamespace`, CLR call/property/construct nodes,
   `DefineType`, `DefineMethod`, `Loop`, `Recur`, `Try`, and `Throw`.
4. Validation: `recur` must be in tail position and must match its enclosing
   loop or function arity.
5. Optimized Core IR: constant conditional folding, `do` flattening, and
   identity-let elimination.
6. Execution: interpreter or emitted IL in a collectible dynamic assembly.
   Emitted methods handle control flow, bindings, defs, calls, interop,
   record/method registration, loop/recur, try/catch/finally, and lambda bodies.
   Each lambda becomes a generated `AFn` subclass with its own IL `Invoke`
   method and captured environment; no interpreter closure is created on the
   Reflection.Emit path. Async bodies use runtime async on .NET 11 and a Task
   continuation transform on .NET 10.

## Samples

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

## Current limits

- `definterface` does not yet accept a generic header or default members.
- Delegate return-byref signatures are not supported. `set!` supports vars
  only, not mutable locals or record fields.
- Wisp-defined events are not emitted yet; CLR events work through their
  add/remove accessors.
- The .NET 10 async transform does not support `await` inside `loop`/`recur`
  or `finally`; the .NET 11 runtime-async path does.
- Byref writeback uses the vector convention above; return-byref signatures are
  not supported.
- Byref `&` hints currently apply to interface parameters; `defn` ABI signatures
  and call-site `(ref ...)` / `(out ...)` / `(in ...)` forms are not implemented.
- `:refer` supports name vectors or `:all`; arbitrary `:rename`/`:exclude`
  options are not implemented.
- Constructor overload selection is by arity plus parameter type hints; C#
  exact overload ranking and implicit numeric ranking are not implemented.
- `defunion` cases cannot declare type parameters yet.
- `or` patterns cannot bind names, and active patterns bind one function result
  per call.
- Typed record fields support primitives, arrays, dictionaries, and common
  generic collections; arbitrary CLR collection types still fall back to
  exact/assignable conversion.
- Codegen emits lambda bodies as IL; common runtime services such as var lookup,
  collection construction, and interop dispatch are centralized helper calls.
- Maps and sets wrap BCL immutable HAMTs; the vector is a custom 32-way trie.
- The compiler consumes reader values with source metadata rather than lowering
  directly from the syntax tree yet.
