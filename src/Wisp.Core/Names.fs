namespace Wisp.Core

open System
open System.Collections.Concurrent

/// Lisp symbol. Namespace is the part before the first slash; the rest is the
/// name, so dotted CLR namespaces stay ordinary name characters.
[<Sealed>]
type Symbol private (namespaceName: string option, name: string, metadata: IPersistentMap option) =
    static member Create(name: string) =
        if isNull name then nullArg (nameof name)
        Symbol(None, name, None)

    static member Create(namespaceName: string, name: string) =
        if isNull name then nullArg (nameof name)
        Symbol(Option.ofObj namespaceName, name, None)

    static member Parse(text: string) =
        if isNull text then nullArg (nameof text)
        if text = "/" then
            Symbol(None, "/", None)
        else
            let slash = text.IndexOf '/'
            if slash > 0 && slash < text.Length - 1 then
                Symbol(Some(text.Substring(0, slash)), text.Substring(slash + 1), None)
            else
                Symbol(None, text, None)

    member _.Namespace = namespaceName
    member _.Name = name
    member _.Meta = metadata
    member _.FullName =
        match namespaceName with
        | Some namespaceName -> namespaceName + "/" + name
        | None -> name

    member _.WithMeta(metadata: IPersistentMap option) = Symbol(namespaceName, name, metadata)

    override this.ToString() = this.FullName

    override _.Equals(other: obj) =
        match other with
        | :? Symbol as symbol ->
            String.Equals(namespaceName |> Option.defaultValue null, symbol.Namespace |> Option.defaultValue null, StringComparison.Ordinal)
            && String.Equals(name, symbol.Name, StringComparison.Ordinal)
        | _ -> false

    override _.GetHashCode() = HashCode.Combine(namespaceName, name)

    interface IMeta with
        member _.Meta = metadata

    interface IObj with
        member this.WithMeta(metadata) = this.WithMeta(metadata) :> IObj

/// Interned keyword. Keywords are reference-identical for the same full name,
/// making them cheap map keys and identity-style function values.
[<Sealed>]
type Keyword private (namespaceName: string option, name: string) =
    static let cache = ConcurrentDictionary<string, Keyword>(StringComparer.Ordinal)

    static let create (namespaceName: string option) (name: string) =
        let fullName =
            match namespaceName with
            | Some namespaceName -> namespaceName + "/" + name
            | None -> name
        let keyword = Keyword(namespaceName, name)
        cache.GetOrAdd(fullName, keyword)

    static member Intern(name: string) =
        if isNull name then nullArg (nameof name)
        let text = if name.StartsWith(":", StringComparison.Ordinal) then name.Substring 1 else name
        let slash = text.IndexOf '/'
        if slash > 0 && slash < text.Length - 1 then
            create (Some(text.Substring(0, slash))) (text.Substring(slash + 1))
        else
            create None text

    static member Intern(namespaceName: string, name: string) =
        if isNull name then nullArg (nameof name)
        create (Option.ofObj namespaceName) name

    member _.Namespace = namespaceName
    member _.Name = name
    member _.FullName =
        match namespaceName with
        | Some namespaceName -> namespaceName + "/" + name
        | None -> name

    override _.ToString() = ":" + (match namespaceName with Some namespaceName -> namespaceName + "/" + name | None -> name)

    override _.Equals(other: obj) =
        match other with
        | :? Keyword as keyword -> String.Equals(name, keyword.Name, StringComparison.Ordinal) && namespaceName = keyword.Namespace
        | _ -> false

    override _.GetHashCode() =
        match namespaceName with
        | Some namespaceName -> HashCode.Combine(namespaceName, name)
        | None -> name.GetHashCode()

    interface IFn with
        member this.Invoke(args: obj[]) =
            match args.Length with
            | 1 ->
                match args.[0] with
                | :? IPersistentMap as map -> map.ValAt(this)
                | _ -> null
            | 2 ->
                match args.[0] with
                | :? IPersistentMap as map ->
                    match map.TryGet(this) with
                    | Some value -> value
                    | None -> args.[1]
                | _ -> args.[1]
            | _ -> raise (ArityException("1 or 2", args.Length))

/// Result of reading an unknown tagged literal. Data readers can be layered on
/// later without changing the reader contract.
[<Sealed>]
type TaggedLiteral(tag: Symbol, form: obj) =
    member _.Tag = tag
    member _.Form = form

    override this.ToString() =
        let formText = if isNull form then "nil" else form.ToString()
        "#" + this.Tag.FullName + " " + formText

    override _.Equals(other: obj) =
        match other with
        | :? TaggedLiteral as literal -> tag.Equals(literal.Tag) && Object.Equals(form, literal.Form)
        | _ -> false

    override _.GetHashCode() = HashCode.Combine(tag, form)

/// Source locations attached by the reader to metadata-capable values. Keeping
/// these keys namespaced avoids colliding with user metadata such as :line.
module SourceMetadata =
    let startKey = Keyword.Intern("wisp", "start")
    let endKey = Keyword.Intern("wisp", "end")
    let lineKey = Keyword.Intern("wisp", "line")
    let columnKey = Keyword.Intern("wisp", "column")
    let pathKey = Keyword.Intern("wisp", "path")
    /// Source-ordered key/value forms for map literals. Map values themselves
    /// are unordered, but syntax-quote splicing needs the original order.
    let mapFormsKey = Keyword.Intern("wisp", "map-forms")

    let private tryInt (key: Keyword) (form: obj) =
        match form with
        | :? IMeta as meta ->
            meta.Meta
            |> Option.bind (fun metadata -> metadata.TryGet key)
            |> Option.bind (fun value ->
                try
                    Some(Convert.ToInt32(value, Globalization.CultureInfo.InvariantCulture))
                with _ ->
                    None)
        | _ -> None

    let private tryString (key: Keyword) (form: obj) =
        match form with
        | :? IMeta as meta ->
            meta.Meta
            |> Option.bind (fun metadata -> metadata.TryGet key)
            |> Option.bind (function
                | :? string as text -> Some text
                | _ -> None)
        | _ -> None

    let tryLineColumn (form: obj) =
        match tryInt lineKey form, tryInt columnKey form with
        | Some line, Some column -> Some(line, column)
        | _ -> None

    let trySpan (form: obj) : TextSpan option =
        match tryInt startKey form, tryInt endKey form with
        | Some start, Some finish when finish >= start ->
            let path = tryString pathKey form
            match tryLineColumn form with
            | Some(line, column) -> Some(TextSpan.Create(start, finish - start, path, line, column))
            | None -> Some(TextSpan.Create(start, finish - start))
        | _ -> None
