namespace Wisp.Compiler

open System
open System.Collections
open System.IO
open System.Numerics
open Wisp.Core

module Builtins =
    let private expectedArity minArgs maxArgs =
        if maxArgs < 0 then sprintf "at least %d" minArgs
        elif minArgs = maxArgs then string minArgs
        else sprintf "%d to %d" minArgs maxArgs

    let private native minArgs maxArgs (implementation: obj[] -> obj) : obj =
        Fn(fun args ->
            if args.Length < minArgs || (maxArgs >= 0 && args.Length > maxArgs) then
                raise (ArityException(expectedArity minArgs maxArgs, args.Length))
            implementation args)
        :> obj

    let private invoke (callee: obj) (args: obj[]) =
        match callee with
        | :? Delegate as delegateValue -> DelegateOps.Invoke(delegateValue, args)
        | :? IFn as functionValue -> functionValue.Invoke args
        | _ -> raise (ArgumentException(sprintf "%s is not callable." (Print.printString callee)))

    let private countValue (value: obj) =
        match value with
        | null -> 0L
        | :? string as text -> int64 text.Length
        | :? IPersistentMap as map -> int64 map.Count
        | :? PersistentList as list -> int64 list.Count
        | :? PersistentVector as vector -> int64 vector.Count
        | :? PersistentSet as set -> int64 set.Count
        | :? ISeq as sequence -> int64 sequence.Count
        | :? ICollection as collection -> int64 collection.Count
        | :? IEnumerable as enumerable -> enumerable |> Seq.cast<obj> |> Seq.length |> int64
        | _ -> raise (ArgumentException(sprintf "%s is not countable." (Print.printString value)))

    let private seqOrNil (value: obj) =
        Seqs.ofSeqable value |> Option.map (fun sequence -> sequence :> obj) |> Option.defaultValue null

    let private restOf (value: obj) =
        match Seqs.ofSeqable value with
        | None -> PersistentList.Empty :> obj
        | Some sequence ->
            let more = sequence.More
            if more.IsEmpty then PersistentList.Empty :> obj else more :> obj

    let private nextOf (value: obj) =
        Seqs.ofSeqable value
        |> Option.bind (fun sequence -> sequence.Next)
        |> Option.map (fun sequence -> sequence :> obj)
        |> Option.defaultValue null

    let private firstOf (value: obj) =
        Seqs.ofSeqable value |> Option.map (fun sequence -> sequence.First) |> Option.defaultValue null

    let private nthValue (value: obj) (index: int) =
        match value with
        | null -> None
        | :? PersistentVector as vector ->
            if index >= 0 && index < vector.Count then Some(vector.Nth index) else None
        | :? PersistentList as list ->
            if index >= 0 && index < list.Count then list.Items |> Seq.tryItem index else None
        | :? string as text ->
            if index >= 0 && index < text.Length then Some(box text.[index]) else None
        | :? System.Collections.IList as list ->
            if index >= 0 && index < list.Count then Some(list.[index]) else None
        | :? ISeq as sequence ->
            let mutable current = sequence
            let mutable remaining = index
            let mutable result = None
            while result.IsNone && not current.IsEmpty && remaining >= 0 do
                if remaining = 0 then result <- Some current.First
                else
                    remaining <- remaining - 1
                    match current.Next with
                    | Some next -> current <- next
                    | None -> result <- None
            result
        | _ -> None

    let private truthy = Truth.isTruthy

    let private integerToValue (value: obj) =
        match value with
        | :? BigInteger as number -> box (int64 number)
        | _ -> box (Convert.ToInt64(value, Globalization.CultureInfo.InvariantCulture))

    let private doubleToValue (value: obj) = box (Convert.ToDouble(value, Globalization.CultureInfo.InvariantCulture))

    let private equality (left: obj) (right: obj) = Numeric.equalValues left right

    let private formDocs =
        Map.ofList
            [ ("def",
               """Defines a var. The optional docstring is Markdown and is stored as `:doc` metadata.

```clojure
(def answer 42)
(def answer "The **ultimate** answer." 42)
(doc answer)
```""");
              ("defn",
               """Defines a function. The optional docstring is Markdown and is stored as `:doc` metadata.

```clojure
(defn add
  "Adds **two** numbers."
  [a b]
  (+ a b))

(doc add)
```""");
              ("defactive",
               """Defines an active pattern used by `match`. The function returns a truthy value or an extracted value.

```clojure
(defactive even? [n]
  (when (= 0 (mod n 2)) n))

(match 4
  ((active even? x) (* x 2))
  (_ -1))
```""");
              ("defmacro",
               """Defines a macro. Macros receive their arguments as unevaluated forms.

```clojure
(defmacro unless [test then else]
  (list 'if test else then))

(unless false "yes" "no")
```""");
              ("defclass",
               """Defines a CLR class or record. Fields and methods are typed with C#-shaped hints.

```clojure
(defclass Person
  "A person with a name."
  [^String name])

(def p (Person {:name "Ada"}))
(.-name p)
```""");
              ("deftype",
               """Deprecated alias for `defclass`.

```clojure
(deftype Pair [a b])
(def p (Pair {:a 1 :b 2}))
```""");
              ("defstruct",
               """Defines a CLR value type. Structs have value equality and cannot inherit.

```clojure
(defstruct Point
  "A point in 2D."
  [^int x ^int y])

(def p (Point {:x 1 :y 2}))
(.-x p)
```""");
              ("definterface",
               """Defines a CLR interface. Member declarations use a receiver as the first parameter.

```clojure
(definterface IShape
  "Something with an area."
  (^double Area [this]))

(defclass Circle [^double radius]
  (implements IShape))
```""");
              ("defdelegate",
               """Defines a CLR delegate type. Signatures support byref, pointer, and Wisp-defined types.

```clojure
(defdelegate ^int IntUnary
  "An integer transform."
  [^int x])

(def twice (delegate IntUnary (fn [x] (* x 2))))
(twice 21)
```""");
              ("defunion",
               """Defines a union made of record cases. Cases are matched with `match`.

```clojure
(defunion Shape
  "A shape."
  (Circle "A circle." [radius])
  (Square [side]))

(def c (Circle {:radius 3}))

(match c
  ((Circle r) (* r r))
  ((Square s) (* s s)))
```""");
              ("defmethod",
               """Defines a method or constructor on a type. The optional docstring goes before the parameter vector.

```clojure
(defmethod Person :greet
  "Greets the person."
  [^Person this]
  (str "Hi " (.-name this)))

(defmethod Person
  "Builds a person from a name."
  [^String name]
  {:name name})
```""");
              ("ns",
               """Switches to a namespace and processes import, require, and use clauses. The optional docstring is Markdown and is stored on the namespace.

```clojure
(ns demo.app
  "The **demo** application."
  (:require [demo.lib :as lib :refer [answer]]))

(doc demo.app)
```""");
              ("async",
               """Runs a body as a `Task<obj>` computation. `await` suspends inside the async body. Add a `ValueTask` hint to return a value task instead.

```clojure
(def t (async
         (await (System.Threading.Tasks.Task/Delay 10))
         42))

(await t)

(def vt (^[ValueTask Int] async 42))
(await vt)
```""");
              ("await",
               """Waits for a task or value task and returns its result. Outside `async` it waits synchronously.

```clojure
(await (async 42))
(await (System.Threading.Tasks.Task/Delay 10))
```""");
              ("doc",
               """Renders Markdown documentation for a var, type, method, namespace, or built-in form. A bare symbol is quoted automatically.

```clojure
(doc add)
(doc Person.greet)
(doc defn)
```""");
              ("meta",
               """Returns the metadata of a var, namespace, or metadata-capable value. A bare symbol is quoted automatically.

```clojure
(meta add)
(meta demo.app)
```""") ]

    type private DocEntry =
        { Canonical: string
          Text: string }

    let private resolveVarOption (runtime: RuntimeState) (symbol: Symbol) =
        let namespaceName =
            match symbol.Namespace with
            | Some namespaceName ->
                runtime.Registry.ResolveAlias(runtime.CurrentNamespace, namespaceName)
                |> Option.defaultValue namespaceName
            | None -> runtime.CurrentNamespace
        runtime.Registry.FindVar(namespaceName, symbol.Name)

    let private docString (value: obj) =
        match value with
        | :? string as text -> text
        | _ -> Print.printString value

    let private normalizeReference (text: string) =
        let text = text.Trim()
        let text =
            if text.StartsWith("doc:", StringComparison.Ordinal) then text.Substring 4
            elif text.StartsWith("wisp:", StringComparison.Ordinal) then text.Substring 5
            elif text.StartsWith("#", StringComparison.Ordinal) then text.Substring 1
            else text
        let slash = text.LastIndexOf '/'
        if slash > 0
           && slash < text.Length - 1
           && (text.Substring(slash + 1) |> Seq.forall Char.IsDigit) then
            text.Substring(0, slash)
        else
            text

    let private namespaceDoc (runtime: RuntimeState) (name: string) =
        runtime.Registry.TryFind name
        |> Option.bind (fun namespaceValue ->
            Meta.tryGet (Keyword.Intern "doc") namespaceValue.Meta
            |> Option.map (fun doc -> namespaceValue, docString doc))

    let private resolveDoc (runtime: RuntimeState) (text: string) : DocEntry option =
        let text = normalizeReference text
        if String.IsNullOrWhiteSpace text then
            None
        else
            let symbol = Symbol.Parse text
            let typeName =
                match symbol.Namespace with
                | Some namespaceName -> namespaceName + "." + symbol.Name
                | None -> symbol.Name
            let methodDoc =
                match typeName.LastIndexOf '.' with
                | index when index > 0 && index < typeName.Length - 1 ->
                    let owner = typeName.Substring(0, index)
                    let methodName = typeName.Substring(index + 1)
                    DefinitionDocs.tryFind (DefinitionDocs.methodKey owner methodName)
                    |> Option.map (fun doc -> { Canonical = owner + "." + methodName; Text = doc })
                | _ -> None
            resolveVarOption runtime symbol
            |> Option.bind (fun variable ->
                Meta.tryGet (Keyword.Intern "doc") variable.Meta
                |> Option.map (fun doc ->
                    { Canonical = variable.FullName
                      Text = docString doc }))
            |> Option.orElse (
                DefinitionDocs.tryFind (DefinitionDocs.typeKey typeName)
                |> Option.map (fun doc -> { Canonical = typeName; Text = doc }))
            |> Option.orElse methodDoc
            |> Option.orElse (
                namespaceDoc runtime symbol.Name
                |> Option.map (fun (namespaceValue, doc) ->
                    { Canonical = namespaceValue.Name
                      Text = doc }))
            |> Option.orElse (
                Map.tryFind symbol.Name formDocs
                |> Option.map (fun doc -> { Canonical = symbol.Name; Text = doc }))

    let private referenceText (value: obj) =
        match value with
        | :? Symbol as symbol -> Some symbol.FullName
        | :? Keyword as keyword ->
            Some(
                match keyword.Namespace with
                | Some namespaceName -> namespaceName + "." + keyword.Name
                | None -> keyword.Name
            )
        | :? string as text -> Some text
        | _ -> None

    let create (runtime: RuntimeState) : Map<string, obj> =
        let output = runtime.Output

        let functions =
            [ "+", native 0 -1 (fun args -> Numeric.add (List.ofArray args))
              "-", native 1 -1 (fun args -> Numeric.subtract (List.ofArray args))
              "*", native 0 -1 (fun args -> Numeric.multiply (List.ofArray args))
              "/", native 1 -1 (fun args -> Numeric.divide (List.ofArray args))
              "inc", native 1 1 (fun args -> Numeric.increment args.[0])
              "dec", native 1 1 (fun args -> Numeric.decrement args.[0])
              "mod", native 2 2 (fun args ->
                  let divisor = Numeric.toInt64 args.[1]
                  if divisor = 0L then raise (DivideByZeroException "Division by zero.")
                  box (Numeric.toInt64 args.[0] % divisor))
              "=", native 1 -1 (fun args ->
                  let first = args.[0]
                  args |> Array.skip 1 |> Array.forall (fun value -> equality first value) |> box)
              "<", native 1 -1 (fun args ->
                  args |> Array.pairwise |> Array.forall (fun (left, right) -> Numeric.compareValues left right < 0) |> box)
              ">", native 1 -1 (fun args ->
                  args |> Array.pairwise |> Array.forall (fun (left, right) -> Numeric.compareValues left right > 0) |> box)
              "<=", native 1 -1 (fun args ->
                  args |> Array.pairwise |> Array.forall (fun (left, right) -> Numeric.compareValues left right <= 0) |> box)
              ">=", native 1 -1 (fun args ->
                  args |> Array.pairwise |> Array.forall (fun (left, right) -> Numeric.compareValues left right >= 0) |> box)
              "not", native 1 1 (fun args -> not (truthy args.[0]) |> box)
              "nil?", native 1 1 (fun args -> isNull args.[0] |> box)
              "some?", native 1 1 (fun args -> not (isNull args.[0]) |> box)
              "map?", native 1 1 (fun args -> (args.[0] :? IPersistentMap) |> box)
              "sequential?", native 1 1 (fun args ->
                  match args.[0] with
                  | :? IPersistentMap -> box false
                  | :? string -> box false
                  | :? ISeq
                  | :? ISeqable
                  | :? System.Collections.Generic.IReadOnlyList<obj> -> box true
                  | _ -> box false)
              "true?", native 1 1 (fun args -> (match args.[0] with :? bool as flag -> flag | _ -> false) |> box)
              "false?", native 1 1 (fun args -> (match args.[0] with :? bool as flag -> not flag | _ -> false) |> box)
              "identity", native 1 1 (fun args -> args.[0])
              "str", native 0 -1 (fun args -> args |> Array.map Print.displayString |> String.concat "" :> obj)
              "print", native 0 -1 (fun args -> output.Write(Print.printMany args); null)
              "println", native 0 -1 (fun args -> output.WriteLine(Print.printMany args); null)
              "pr", native 0 -1 (fun args -> output.Write(Print.prStrMany args); null)
              "prn", native 0 -1 (fun args -> output.WriteLine(Print.prStrMany args); null)
              "doc", native 1 1 (fun args ->
                  match referenceText args.[0] with
                  | Some text ->
                      match resolveDoc runtime text with
                      | Some entry ->
                          output.WriteLine entry.Canonical
                          let body = new StringWriter()
                          let references =
                              Markdown.render
                                  body
                                  entry.Text
                                  (fun reference -> resolveDoc runtime reference |> Option.map (fun target -> target.Canonical))
                                  runtime.RenderMarkdown
                          let lines =
                              body.ToString().Replace("\r\n", "\n").Split('\n')
                              |> Array.skipWhile String.IsNullOrEmpty
                              |> Array.rev
                              |> Array.skipWhile String.IsNullOrEmpty
                              |> Array.rev
                          for line in lines do
                              if line = "" then output.WriteLine()
                              else output.WriteLine("  " + line)
                          let references = references |> List.filter ((<>) entry.Canonical)
                          if not references.IsEmpty then
                              output.WriteLine()
                              output.WriteLine("  See also: " + String.Join(", ", references))
                      | None -> output.WriteLine(sprintf "%s has no documentation." text)
                      null
                  | None ->
                      raise (ArgumentException(sprintf "doc expects a symbol, keyword, or string, got %s." (Print.printString args.[0]))))
              "meta", native 1 1 (fun args ->
                  match args.[0] with
                  | :? Symbol as symbol ->
                      match resolveVarOption runtime symbol with
                      | Some variable -> variable.Meta |> Option.map box |> Option.defaultValue null
                      | None ->
                          match runtime.Registry.TryFind symbol.Name with
                          | Some namespaceValue -> namespaceValue.Meta |> Option.map box |> Option.defaultValue null
                          | None -> null
                  | :? IMeta as value -> value.Meta |> Option.map box |> Option.defaultValue null
                  | _ -> null)
              "list", native 0 -1 (fun args -> PersistentList.OfSeq args :> obj)
              "concat", native 0 -1 (fun args ->
                  let items = ResizeArray<obj>()
                  for argument in args do
                      items.AddRange(Seqs.toList argument)
                  PersistentList.OfSeq items :> obj)
              "drop", native 2 2 (fun args ->
                  let count = max 0 (Convert.ToInt32(args.[0], Globalization.CultureInfo.InvariantCulture))
                  let items = Seqs.toList args.[1]
                  let remaining = if count >= items.Length then [] else items |> List.skip count
                  PersistentList.OfSeq remaining :> obj)
              "vector", native 0 -1 (fun args -> PersistentVector.OfSeq args :> obj)
              "hash-map", native 0 -1 (fun args ->
                  if args.Length % 2 <> 0 then raise (ArgumentException "hash-map needs an even number of arguments.")
                  let mutable map = PersistentMap.Empty
                  for index in 0 .. 2 .. args.Length - 1 do
                      map <- map.Assoc(args.[index], args.[index + 1])
                  map :> obj)
              "hash-set", native 0 -1 (fun args -> PersistentSet.OfSeq args :> obj)
              "cons", native 2 2 (fun args ->
                  let rest =
                      match Seqs.ofSeqable args.[1] with
                      | Some sequence -> sequence
                      | None -> PersistentList.Empty :> ISeq
                  Cons(args.[0], rest) :> obj)
              "first", native 1 1 (fun args -> firstOf args.[0])
              "rest", native 1 1 (fun args -> restOf args.[0])
              "next", native 1 1 (fun args -> nextOf args.[0])
              "seq", native 1 1 (fun args -> seqOrNil args.[0])
              "count", native 1 1 (fun args -> countValue args.[0] :> obj)
              "empty?", native 1 1 (fun args -> (countValue args.[0] = 0L) |> box)
              "not-empty", native 1 1 (fun args -> if countValue args.[0] = 0L then null else args.[0])
              "nth", native 2 3 (fun args ->
                  let index = Convert.ToInt32(args.[1], Globalization.CultureInfo.InvariantCulture)
                  match nthValue args.[0] index with
                  | Some value -> value
                  | None when args.Length = 3 -> args.[2]
                  | None -> raise (ArgumentOutOfRangeException("index", sprintf "Index %d is out of bounds." index)))
              "get", native 2 3 (fun args ->
                  let defaultValue = if args.Length = 3 then args.[2] else null
                  match args.[0] with
                  | null -> defaultValue
                  | :? IPersistentMap as map ->
                      match map.TryGet args.[1] with
                      | Some value -> value
                      | None -> defaultValue
                  | :? PersistentVector as vector ->
                      let index = Convert.ToInt32(args.[1], Globalization.CultureInfo.InvariantCulture)
                      if index >= 0 && index < vector.Count then vector.Nth index else defaultValue
                  | :? System.Collections.IList as list ->
                      let index = Convert.ToInt32(args.[1], Globalization.CultureInfo.InvariantCulture)
                      if index >= 0 && index < list.Count then list.[index] else defaultValue
                  | :? System.Collections.IDictionary as dictionary ->
                      if dictionary.Contains args.[1] then dictionary.[args.[1]] else defaultValue
                  | :? PersistentSet as set ->
                      if set.Contains args.[1] then args.[1] else defaultValue
                  | :? string as text ->
                      let index = Convert.ToInt32(args.[1], Globalization.CultureInfo.InvariantCulture)
                      if index >= 0 && index < text.Length then box text.[index] else defaultValue
                  | _ -> defaultValue)
              "assoc", native 1 -1 (fun args ->
                  if args.Length < 3 || (args.Length - 1) % 2 <> 0 then
                      raise (ArgumentException "assoc needs a map and key/value pairs.")
                  let mutable map =
                      match args.[0] with
                      | null -> PersistentMap.Empty
                      | :? PersistentMap as existing -> existing
                      | _ -> raise (ArgumentException "assoc expects a map.")
                  for index in 1 .. 2 .. args.Length - 2 do
                      map <- map.Assoc(args.[index], args.[index + 1])
                  map :> obj)
              "dissoc", native 2 -1 (fun args ->
                  let mutable map =
                      match args.[0] with
                      | null -> PersistentMap.Empty
                      | :? PersistentMap as existing -> existing
                      | _ -> raise (ArgumentException "dissoc expects a map.")
                  for index in 1 .. args.Length - 1 do
                      map <- map.Without args.[index]
                  map :> obj)
              "contains?", native 2 2 (fun args ->
                  match args.[0] with
                  | :? IPersistentMap as map -> (map.TryGet args.[1] |> Option.isSome) |> box
                  | :? PersistentSet as set -> set.Contains args.[1] |> box
                  | :? PersistentVector as vector ->
                      let index = Convert.ToInt32(args.[1], Globalization.CultureInfo.InvariantCulture)
                      (index >= 0 && index < vector.Count) |> box
                  | _ -> false |> box)
              "conj", native 2 -1 (fun args ->
                  let mutable collection = args.[0]
                  for index in 1 .. args.Length - 1 do
                      let value = args.[index]
                      collection <-
                          match collection with
                          | null -> PersistentList.Empty.Cons(value) :> obj
                          | :? PersistentList as list -> list.Cons(value) :> obj
                          | :? PersistentVector as vector -> vector.Add(value) :> obj
                          | :? PersistentSet as set -> set.Add(value) :> obj
                          | :? PersistentMap as map ->
                              match value with
                              | :? PersistentVector as entry when entry.Count = 2 -> map.Assoc(entry.Nth 0, entry.Nth 1) :> obj
                              | :? IPersistentMap as other ->
                                  let mutable result = map
                                  for entry in other.Entries do
                                      result <- result.Assoc(entry.Key, entry.Value)
                                  result :> obj
                              | _ -> raise (ArgumentException "conj on a map expects an entry vector or map.")
                          | :? ISeq as sequence -> Cons(value, sequence) :> obj
                          | _ -> raise (ArgumentException(sprintf "%s cannot be conjoined." (Print.printString collection)))
                  collection)
              "in-ns", native 1 1 (fun args ->
                  let name =
                      match args.[0] with
                      | :? Symbol as symbol -> symbol.FullName
                      | :? Keyword as keyword ->
                          match keyword.Namespace with
                          | Some namespaceName -> namespaceName + "." + keyword.Name
                          | None -> keyword.Name
                      | :? string as text -> text
                      | value -> raise (ArgumentException(sprintf "in-ns expects a symbol, keyword, or string, got %s." (Print.printString value)))
                  if String.IsNullOrWhiteSpace name then
                      raise (ArgumentException "in-ns expects a non-empty namespace name.")
                  runtime.CurrentNamespace <- name
                  runtime.Registry.GetOrCreate name |> ignore
                  box name)
              "keyword", native 1 1 (fun args ->
                  match args.[0] with
                  | null -> null
                  | :? Keyword as keyword -> keyword :> obj
                  | :? string as text -> Keyword.Intern text :> obj
                  | :? Symbol as symbol ->
                      match symbol.Namespace with
                      | Some namespaceName -> Keyword.Intern(namespaceName, symbol.Name) :> obj
                      | None -> Keyword.Intern symbol.Name :> obj
                  | _ -> raise (ArgumentException "keyword expects a string, symbol, or keyword."))
              "symbol", native 1 1 (fun args ->
                  match args.[0] with
                  | null -> null
                  | :? Symbol as symbol -> symbol :> obj
                  | :? string as text -> Symbol.Parse text :> obj
                  | :? Keyword as keyword ->
                      match keyword.Namespace with
                      | Some namespaceName -> Symbol.Create(namespaceName, keyword.Name) :> obj
                      | None -> Symbol.Create keyword.Name :> obj
                  | _ -> raise (ArgumentException "symbol expects a string, symbol, or keyword."))
              "name", native 1 1 (fun args ->
                  match args.[0] with
                  | :? Keyword as keyword -> box keyword.Name
                  | :? Symbol as symbol -> box symbol.Name
                  | :? string as text -> box text
                  | _ -> null)
              "namespace", native 1 1 (fun args ->
                  match args.[0] with
                  | :? Keyword as keyword -> keyword.Namespace |> Option.map box |> Option.defaultValue null
                  | :? Symbol as symbol -> symbol.Namespace |> Option.map box |> Option.defaultValue null
                  | _ -> null)
              "apply", native 2 -1 (fun args ->
                  let callee = args.[0]
                  let prefix = if args.Length > 2 then args.[1 .. args.Length - 2] else Array.empty<obj>
                  let suffix = Seqs.toList args.[args.Length - 1]
                  invoke callee (Array.append prefix (List.toArray suffix)))
              "map", native 2 2 (fun args ->
                  let callee = args.[0]
                  Seqs.toList args.[1]
                  |> List.map (fun value -> invoke callee [| value |])
                  |> PersistentList.OfSeq
                  :> obj)
              "filter", native 2 2 (fun args ->
                  let callee = args.[0]
                  Seqs.toList args.[1]
                  |> List.filter (fun value -> truthy (invoke callee [| value |]))
                  |> PersistentList.OfSeq
                  :> obj)
              "reduce", native 2 3 (fun args ->
                  let callee = args.[0]
                  match args.Length with
                  | 2 ->
                      match Seqs.toList args.[1] with
                      | [] -> invoke callee [||]
                      | first :: rest -> rest |> List.fold (fun acc value -> invoke callee [| acc; value |]) first
                  | _ ->
                      Seqs.toList args.[2]
                      |> List.fold (fun acc value -> invoke callee [| acc; value |]) args.[1])
              "int", native 1 1 (fun args -> integerToValue args.[0])
              "long", native 1 1 (fun args -> integerToValue args.[0])
              "double", native 1 1 (fun args -> doubleToValue args.[0])
              "boolean", native 1 1 (fun args -> truthy args.[0] |> box)
              "zero?", native 1 1 (fun args -> (Numeric.compareValues args.[0] (box 0L) = 0) |> box)
              "pos?", native 1 1 (fun args -> (Numeric.compareValues args.[0] (box 0L) > 0) |> box)
              "neg?", native 1 1 (fun args -> (Numeric.compareValues args.[0] (box 0L) < 0) |> box)
              "type", native 1 1 (fun args -> if isNull args.[0] then null else args.[0].GetType() :> obj)
              "class", native 1 1 (fun args -> if isNull args.[0] then null else args.[0].GetType() :> obj) ]

        functions |> Map.ofList
