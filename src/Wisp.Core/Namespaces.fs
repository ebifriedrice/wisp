namespace Wisp.Core

open System
open System.Collections.Concurrent

/// A mutable namespace var. Clojure-ish identity plus a root binding; metadata
/// is where type hints and source information live.
[<Sealed>]
type Var(namespaceName: string, name: string, root: obj) =
    let mutable root = root
    let mutable metadata: IPersistentMap option = None

    member _.Namespace = namespaceName
    member _.Name = name
    member _.FullName = namespaceName + "/" + name

    member _.Root
        with get () = root
        and set value = root <- value

    member _.Meta
        with get () = metadata
        and set value = metadata <- value

    override this.ToString() = "#'" + this.FullName

/// A C#-style namespace identity, its alias table, and the vars interned in it.
[<Sealed>]
type Namespace(name: string) =
    let aliases = ConcurrentDictionary<string, string>(StringComparer.Ordinal)
    let imports = ConcurrentDictionary<string, string>(StringComparer.Ordinal)
    let referrals = ConcurrentDictionary<string, string * string>(StringComparer.Ordinal)
    let vars = ConcurrentDictionary<string, Var>(StringComparer.Ordinal)
    let mutable metadata: IPersistentMap option = None

    member _.Name = name
    member _.Vars = vars.Values :> seq<Var>

    member _.Meta
        with get () = metadata
        and set value = metadata <- value

    member _.AddImport(alias: string, fullName: string) =
        if String.IsNullOrEmpty alias then invalidArg (nameof alias) "An import alias cannot be empty."
        if String.IsNullOrEmpty fullName then invalidArg (nameof fullName) "An imported type name cannot be empty."
        imports.[alias] <- fullName

    member _.TryResolveImport(alias: string) =
        match imports.TryGetValue alias with
        | true, fullName -> Some fullName
        | false, _ -> None

    member _.AddReferral(localName: string, namespaceName: string, varName: string) =
        if String.IsNullOrEmpty localName then invalidArg (nameof localName) "A referral name cannot be empty."
        referrals.[localName] <- (namespaceName, varName)

    member _.TryResolveReferral(localName: string) =
        match referrals.TryGetValue localName with
        | true, target -> Some target
        | false, _ -> None

    member _.AddAlias(alias: string, target: string) =
        if String.IsNullOrEmpty alias then invalidArg (nameof alias) "A namespace alias cannot be empty."
        if String.IsNullOrEmpty target then invalidArg (nameof target) "A namespace alias target cannot be empty."
        aliases.[alias] <- target

    member _.TryResolveAlias(alias: string) =
        match aliases.TryGetValue alias with
        | true, target -> Some target
        | false, _ -> None

    member _.Intern(varName: string, root: obj, metadata: IPersistentMap option) =
        if String.IsNullOrEmpty varName then invalidArg (nameof varName) "A var name cannot be empty."
        let variable = vars.GetOrAdd(varName, fun _ -> Var(name, varName, root))
        variable.Root <- root
        variable.Meta <- metadata
        variable

    member _.FindVar(varName: string) =
        match vars.TryGetValue varName with
        | true, variable -> Some variable
        | false, _ -> None

    override _.ToString() = name

/// Global namespace table. Current namespace selection belongs to the session,
/// not the table, so the reader receives it explicitly.
[<Sealed>]
type NamespaceRegistry() =
    let namespaces = ConcurrentDictionary<string, Namespace>(StringComparer.Ordinal)

    member _.GetOrCreate(name: string) =
        if String.IsNullOrWhiteSpace name then invalidArg (nameof name) "A namespace name cannot be empty."
        namespaces.GetOrAdd(name, fun name -> Namespace name)

    member _.TryFind(name: string) =
        match namespaces.TryGetValue name with
        | true, namespaceValue -> Some namespaceValue
        | false, _ -> None

    member this.Intern(namespaceName: string, name: string, root: obj, metadata: IPersistentMap option) =
        (this.GetOrCreate namespaceName).Intern(name, root, metadata)

    member this.FindVar(namespaceName: string, name: string) =
        this.TryFind namespaceName |> Option.bind (fun namespaceValue -> namespaceValue.FindVar name)

    member this.ResolveAlias(currentNamespace: string, alias: string) =
        this.TryFind currentNamespace |> Option.bind (fun namespaceValue -> namespaceValue.TryResolveAlias alias)
