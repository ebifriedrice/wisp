namespace Wisp.Core

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Reflection

type TypeVariance =
    | Invariant
    | Covariant
    | Contravariant

/// A structural CLR type expression. It can represent open and constructed
/// generic types, arrays of every rank, pointers, byrefs, and generic parameters.
type TypeExpr =
    | NamedType of name: string * arity: int option * args: TypeExpr list
    | TypeParameter of name: string
    | ArrayType of element: TypeExpr * rank: int
    | PointerType of TypeExpr
    | ByRefType of TypeExpr

type TypeConstraint =
    | ReferenceTypeConstraint
    | NullableReferenceTypeConstraint
    | ValueTypeConstraint
    | NotNullConstraint
    | UnmanagedConstraint
    | DefaultConstructorConstraint
    | AllowsRefStructConstraint
    | DefaultConstraint
    | BaseTypeConstraint of TypeExpr
    | InterfaceConstraint of TypeExpr
    | TypeParameterConstraint of string

type GenericParameter =
    { Name: string
      Variance: TypeVariance
      Constraints: TypeConstraint list }

type TypeDefinition =
    { Name: string
      Arity: int
      TypeParameters: GenericParameter list
      BaseType: TypeExpr option
      Interfaces: TypeExpr list }

type TypeHint =
    | TypeReference of TypeExpr
    | TypeDefinitionHint of TypeDefinition

type TypeHintError =
    { Path: string
      Message: string }

/// Hiccup-shaped CLR type hints.
///
/// A type reference is a keyword/symbol/string or a vector:
///   :int
///   [:List [:List :int]]
///   [:Dictionary {:arity 2} :String [:List :int]]
///   [:array {:rank 2} :int]
///   [:byref :int]
///   [:param T]
///
/// A generic definition adds :def, :param, :where, :base, and :interface:
///   [:def {:name My.Box :arity 1}
///     [:param {:name T :variance :out}]
///     [:where T :class :new [:interface :IDisposable]]
///     [:base :object]
///     [:interface :System.IComparable]]
module TypeHints =
    let private tagKey = Keyword.Intern "tag"

    let private fail path message : Result<'T, TypeHintError list> =
        Error [ { Path = path; Message = message } ]

    let private bind (result: Result<'T, TypeHintError list>) (continuation: 'T -> Result<'U, TypeHintError list>) =
        match result with
        | Ok value -> continuation value
        | Error errors -> Error errors

    let private traverse (path: string) (items: 'Item list) (parser: int -> 'Item -> Result<'T, TypeHintError list>) =
        let rec loop index acc remaining =
            match remaining with
            | [] -> Ok(List.rev acc)
            | item :: rest ->
                match parser index item with
                | Ok value -> loop (index + 1) (value :: acc) rest
                | Error errors -> Error errors
        loop 0 [] items

    let private nameOf (value: obj) : string option =
        match value with
        | :? Keyword as keyword ->
            match keyword.Namespace with
            | Some namespaceName -> Some(namespaceName + "." + keyword.Name)
            | None -> Some keyword.Name
        | :? Symbol as symbol ->
            match symbol.Namespace with
            | Some namespaceName -> Some(namespaceName + "." + symbol.Name)
            | None -> Some symbol.Name
        | :? string as text -> Some text
        | _ -> None

    let private reservedTag (value: obj) =
        let tag name =
            match name with
            | "array" | "pointer" | "byref" | "param" | "where" | "def" | "base" | "interface" -> Some name
            | _ -> None
        match value with
        | :? Keyword as keyword when keyword.Namespace.IsNone || keyword.Namespace = Some "clr" -> tag keyword.Name
        | :? Symbol as symbol when symbol.Namespace.IsNone -> tag symbol.Name
        | _ -> None

    let private splitAttrs (rest: obj list) =
        match rest with
        | (:? IPersistentMap as attrs) :: children -> Some attrs, children
        | _ -> None, rest

    let private attrKey name = Keyword.Intern name

    let private attrValue name (attrs: IPersistentMap option) =
        attrs |> Option.bind (fun map -> map.TryGet(attrKey name))

    let private attrName (key: obj) =
        match key with
        | :? Keyword as keyword when keyword.Namespace.IsNone -> Some keyword.Name
        | _ -> None

    let private ensureKnownAttrs path (allowed: string list) (attrs: IPersistentMap option) =
        match attrs with
        | None -> Ok()
        | Some map ->
            match
                map.Entries
                |> Seq.tryFind (fun entry ->
                    match attrName entry.Key with
                    | Some name -> not (List.contains name allowed)
                    | None -> true)
            with
            | Some entry -> fail path (sprintf "Unknown type hint attribute %O." entry.Key)
            | None -> Ok()

    let private attrInt path name (attrs: IPersistentMap option) =
        match attrValue name attrs with
        | None -> Ok None
        | Some value ->
            try
                Ok(Some(Convert.ToInt32(value, Globalization.CultureInfo.InvariantCulture)))
            with _ ->
                fail path (sprintf "Attribute :%s must be an integer." name)

    let private attrString path name (attrs: IPersistentMap option) =
        match attrValue name attrs with
        | None -> Ok None
        | Some value ->
            match nameOf value with
            | Some text -> Ok(Some text)
            | None -> fail path (sprintf "Attribute :%s must be a name." name)

    /// Generic aliases whose arguments may be grouped in one vector, as in
    /// [:Dictionary [:String :Int]].
    let private genericArities =
        dict [
            "List", 1
            "IList", 1
            "IReadOnlyList", 1
            "IEnumerable", 1
            "ICollection", 1
            "Dictionary", 2
            "IDictionary", 2
            "IReadOnlyDictionary", 2
            "HashSet", 1
            "ISet", 1
            "Task", 1
            "ValueTask", 1
            "Nullable", 1
        ]

    let private groupGenericArguments name (children: obj list) =
        match children with
        | [ :? PersistentVector as group ] ->
            let grouped = group.Elements |> Seq.toList
            match genericArities.TryGetValue name with
            | true, arity when arity > 1 && grouped.Length = arity && grouped.Length <> 1 -> grouped
            | _ -> children
        | _ -> children

    let private parseNamedAtom (name: string) =
        let byRefIndex = name.IndexOf("&:", StringComparison.Ordinal)
        if byRefIndex > 0 then
            ByRefType(NamedType(name.Substring(0, byRefIndex), None, []))
        elif name.EndsWith("&", StringComparison.Ordinal) then
            ByRefType(NamedType(name.Substring(0, name.Length - 1), None, []))
        elif name.EndsWith("*", StringComparison.Ordinal) then
            PointerType(NamedType(name.Substring(0, name.Length - 1), None, []))
        else
            NamedType(name, None, [])

    let rec private parseTypeExpr path (form: obj) : Result<TypeExpr, TypeHintError list> =
        match form with
        | :? Keyword as keyword ->
            Ok(parseNamedAtom (nameOf keyword).Value)
        | :? Symbol as symbol ->
            Ok(parseNamedAtom (nameOf symbol).Value)
        | :? string as text ->
            Ok(parseNamedAtom text)
        | :? PersistentVector as vector ->
            parseTypeVector path vector
        | null ->
            fail path "nil is not a type hint."
        | _ ->
            fail path (sprintf "Expected a keyword, symbol, string, or vector; got %s." (form.GetType().Name))

    and parseTypeVector path (vector: PersistentVector) : Result<TypeExpr, TypeHintError list> =
        let items = vector.Elements |> Seq.toList
        match items with
        | [] -> fail path "A type hint vector cannot be empty."
        | head :: rest ->
            let attrs, children = splitAttrs rest
            match reservedTag head with
            | Some "array" ->
                bind (ensureKnownAttrs path [ "rank" ] attrs) (fun () ->
                    bind (attrInt path "rank" attrs) (fun rank ->
                        match children with
                        | [ element ] ->
                            let rank = defaultArg rank 1
                            if rank < 1 || rank > 32 then
                                fail path "Array rank must be between 1 and 32."
                            else
                                parseTypeExpr (path + ".element") element
                                |> Result.map (fun element -> ArrayType(element, rank))
                        | _ -> fail path "An array type requires exactly one element type."))
            | Some "pointer" ->
                bind (ensureKnownAttrs path [] attrs) (fun () ->
                    match children with
                    | [ element ] -> parseTypeExpr (path + ".element") element |> Result.map PointerType
                    | _ -> fail path "A pointer type requires exactly one element type.")
            | Some "byref" ->
                bind (ensureKnownAttrs path [] attrs) (fun () ->
                    match children with
                    | [ element ] -> parseTypeExpr (path + ".element") element |> Result.map ByRefType
                    | _ -> fail path "A byref type requires exactly one element type.")
            | Some "param" ->
                bind (ensureKnownAttrs path [] attrs) (fun () ->
                    match children with
                    | [ name ] ->
                        match nameOf name with
                        | Some name -> Ok(TypeParameter name)
                        | None -> fail (path + ".name") "A parameter reference needs a symbol, keyword, or string name."
                    | _ -> fail path "A parameter reference requires exactly one name.")
            | Some ("where" | "def" | "base" | "interface") ->
                fail path "That tag is only valid inside a type definition."
            | _ ->
                match nameOf head with
                | None -> fail path "A named type needs a keyword, symbol, or string head."
                | Some name ->
                    let children = groupGenericArguments name children
                    bind (ensureKnownAttrs path [ "arity" ] attrs) (fun () ->
                        bind (attrInt path "arity" attrs) (fun arity ->
                            if arity |> Option.exists (fun value -> value < 0) then
                                fail path "Generic arity cannot be negative."
                            else
                                bind (traverse path children (fun index form -> parseTypeExpr (sprintf "%s.args[%d]" path index) form)) (fun args ->
                                    match arity with
                                    | Some value when not args.IsEmpty && value <> args.Length ->
                                        fail path (sprintf "Generic arity %d does not match %d type arguments." value args.Length)
                                    | _ -> Ok(NamedType(name, arity, args)))))

    and parseConstraint path (form: obj) : Result<TypeConstraint, TypeHintError list> =
        match form with
        | :? Keyword as keyword when keyword.Namespace.IsNone || keyword.Namespace = Some "clr" ->
            match keyword.Name with
            | "class" -> Ok ReferenceTypeConstraint
            | "class?" -> Ok NullableReferenceTypeConstraint
            | "struct" -> Ok ValueTypeConstraint
            | "notnull" -> Ok NotNullConstraint
            | "unmanaged" -> Ok UnmanagedConstraint
            | "new" -> Ok DefaultConstructorConstraint
            | "allows-ref-struct" -> Ok AllowsRefStructConstraint
            | "default" -> Ok DefaultConstraint
            | _ -> fail path (sprintf "Unknown constraint :%s." keyword.Name)
        | :? PersistentVector as vector -> parseConstraintVector path vector
        | _ -> fail path "A constraint must be a constraint keyword or a [:base ...], [:interface ...], or [:param ...] vector."

    and parseConstraintVector path (vector: PersistentVector) : Result<TypeConstraint, TypeHintError list> =
        let items = vector.Elements |> Seq.toList
        match items with
        | [] -> fail path "A constraint vector cannot be empty."
        | head :: rest ->
            let attrs, children = splitAttrs rest
            match reservedTag head with
            | Some "base" ->
                bind (ensureKnownAttrs path [] attrs) (fun () ->
                    match children with
                    | [ value ] -> parseTypeExpr (path + ".base") value |> Result.map BaseTypeConstraint
                    | _ -> fail path "[:base ...] requires exactly one type.")
            | Some "interface" ->
                bind (ensureKnownAttrs path [] attrs) (fun () ->
                    match children with
                    | [ value ] -> parseTypeExpr (path + ".interface") value |> Result.map InterfaceConstraint
                    | _ -> fail path "[:interface ...] requires exactly one type.")
            | Some "param" ->
                bind (ensureKnownAttrs path [] attrs) (fun () ->
                    match children with
                    | [ value ] ->
                        match nameOf value with
                        | Some name -> Ok(TypeParameterConstraint name)
                        | None -> fail path "A type parameter constraint needs a name."
                    | _ -> fail path "[:param ...] requires exactly one name.")
            | _ -> fail path "Unknown constraint vector. Use [:base T], [:interface T], or [:param T]."

    and parseParam path (vector: PersistentVector) : Result<GenericParameter, TypeHintError list> =
        let items = vector.Elements |> Seq.toList
        match items with
        | head :: rest when reservedTag head = Some "param" ->
            let attrs, children = splitAttrs rest
            bind (ensureKnownAttrs path [ "name"; "variance" ] attrs) (fun () ->
                bind (attrString path "name" attrs) (fun attributeName ->
                    let name =
                        match attributeName, children with
                        | Some name, [] -> Ok name
                        | Some name, _ -> fail path "A parameter name was given twice."
                        | None, [ value ] ->
                            match nameOf value with
                            | Some name -> Ok name
                            | None -> fail path "A parameter needs a symbol, keyword, or string name."
                        | None, _ -> fail path "A parameter needs exactly one name."
                    bind name (fun name ->
                        let variance =
                            match attrValue "variance" attrs with
                            | None -> Ok Invariant
                            | Some (:? Keyword as keyword) ->
                                match keyword.Name with
                                | "in" -> Ok Contravariant
                                | "out" -> Ok Covariant
                                | "invariant" -> Ok Invariant
                                | other -> fail path (sprintf "Unknown variance :%s." other)
                            | Some _ -> fail path "Variance must be :in, :out, or :invariant."
                        variance |> Result.map (fun variance ->
                            { Name = name
                              Variance = variance
                              Constraints = [] }))))
        | _ -> fail path "Expected a [:param ...] declaration."

    and parseWhere path (vector: PersistentVector) : Result<string * TypeConstraint list, TypeHintError list> =
        let items = vector.Elements |> Seq.toList
        match items with
        | head :: rest when reservedTag head = Some "where" ->
            let attrs, children = splitAttrs rest
            bind (ensureKnownAttrs path [] attrs) (fun () ->
                match children with
                | [] -> fail path "[:where ...] needs a type parameter name."
                | nameForm :: constraintForms ->
                    match nameOf nameForm with
                    | None -> fail path "The first [:where ...] item must be a type parameter name."
                    | Some name ->
                        bind (traverse path constraintForms (fun index form -> parseConstraint (sprintf "%s.constraints[%d]" path index) form)) (fun constraints ->
                            Ok(name, constraints)))
        | _ -> fail path "Expected a [:where ...] declaration."

    and parseDefinition path (vector: PersistentVector) : Result<TypeDefinition, TypeHintError list> =
        let items = vector.Elements |> Seq.toList
        match items with
        | head :: rest when reservedTag head = Some "def" ->
            let attrs, children = splitAttrs rest
            bind (ensureKnownAttrs path [ "name"; "arity" ] attrs) (fun () ->
                bind (attrString path "name" attrs) (fun name ->
                    match name with
                    | None -> fail path "A type definition needs a :name."
                    | Some name ->
                        bind (attrInt path "arity" attrs) (fun declaredArity ->
                            let mutable parameters: GenericParameter list = []
                            let mutable constraints: Map<string, TypeConstraint list> = Map.empty
                            let mutable baseType: TypeExpr option = None
                            let mutable interfaces: TypeExpr list = []
                            let mutable index = 0
                            let mutable error: Result<TypeDefinition, TypeHintError list> option = None
                            for child in children do
                                if error.IsNone then
                                    match child with
                                    | :? PersistentVector as childVector ->
                                        let childPath = sprintf "%s.children[%d]" path index
                                        match childVector.Elements |> Seq.tryHead |> Option.bind reservedTag with
                                        | Some "param" ->
                                            match parseParam childPath childVector with
                                            | Ok parameter ->
                                                if parameters |> List.exists (fun existing -> existing.Name = parameter.Name) then
                                                    error <- Some(fail childPath (sprintf "Duplicate type parameter %s." parameter.Name))
                                                else
                                                    parameters <- parameters @ [ parameter ]
                                            | Error errors -> error <- Some(Error errors)
                                        | Some "where" ->
                                            match parseWhere childPath childVector with
                                            | Ok(name, parsedConstraints) ->
                                                let existing = constraints |> Map.tryFind name |> Option.defaultValue []
                                                constraints <- constraints |> Map.add name (existing @ parsedConstraints)
                                            | Error errors -> error <- Some(Error errors)
                                        | Some "base" ->
                                            let childAttrs, grandChildren = splitAttrs (childVector.Elements |> Seq.toList |> List.tail)
                                            match ensureKnownAttrs childPath [] childAttrs, grandChildren with
                                            | Error errors, _ -> error <- Some(Error errors)
                                            | _, [ value ] ->
                                                match parseTypeExpr (childPath + ".base") value with
                                                | Ok parsed when baseType.IsNone -> baseType <- Some parsed
                                                | Ok _ -> error <- Some(fail childPath "A type definition can have only one base type.")
                                                | Error errors -> error <- Some(Error errors)
                                            | _, _ -> error <- Some(fail childPath "[:base ...] requires exactly one type.")
                                        | Some "interface" ->
                                            let childAttrs, grandChildren = splitAttrs (childVector.Elements |> Seq.toList |> List.tail)
                                            match ensureKnownAttrs childPath [] childAttrs, grandChildren with
                                            | Error errors, _ -> error <- Some(Error errors)
                                            | _, [ value ] ->
                                                match parseTypeExpr (childPath + ".interface") value with
                                                | Ok parsed -> interfaces <- interfaces @ [ parsed ]
                                                | Error errors -> error <- Some(Error errors)
                                            | _, _ -> error <- Some(fail childPath "[:interface ...] requires exactly one type.")
                                        | _ -> error <- Some(fail childPath "Expected [:param ...], [:where ...], [:base ...], or [:interface ...].")
                                    | _ -> error <- Some(fail (sprintf "%s.children[%d]" path index) "A type definition child must be a vector.")
                                index <- index + 1
                            match error with
                            | Some result -> result
                            | None ->
                                let parameterNames = parameters |> List.map (fun parameter -> parameter.Name) |> Set.ofList
                                let unknown = constraints |> Map.toList |> List.filter (fun (name, _) -> not (parameterNames.Contains name))
                                match unknown with
                                | (name, _) :: _ -> fail path (sprintf "Constraint refers to unknown type parameter %s." name)
                                | [] ->
                                    let arity = parameters.Length
                                    match declaredArity with
                                    | Some value when value <> arity ->
                                        fail path (sprintf "Declared arity %d does not match %d type parameters." value arity)
                                    | _ ->
                                        let parameters =
                                            parameters
                                            |> List.map (fun parameter ->
                                                { parameter with
                                                    Constraints = constraints |> Map.tryFind parameter.Name |> Option.defaultValue [] })
                                        Ok(
                                            { Name = name
                                              Arity = arity
                                              TypeParameters = parameters
                                              BaseType = baseType
                                              Interfaces = interfaces }
                                        ))))
        | _ -> fail path "Expected a [:def ...] type definition."

    let parse (form: obj) : Result<TypeHint, TypeHintError list> =
        match form with
        | :? PersistentVector as vector ->
            match vector.Elements |> Seq.tryHead with
            | Some head when reservedTag head = Some "def" -> parseDefinition "$" vector |> Result.map TypeDefinitionHint
            | _ -> parseTypeExpr "$" form |> Result.map TypeReference
        | _ -> parseTypeExpr "$" form |> Result.map TypeReference

    let private aliases =
        dict [
            "object", typeof<obj>
            "Object", typeof<obj>
            "string", typeof<string>
            "String", typeof<string>
            "bool", typeof<bool>
            "Bool", typeof<bool>
            "Boolean", typeof<bool>
            "byte", typeof<byte>
            "Byte", typeof<byte>
            "sbyte", typeof<sbyte>
            "SByte", typeof<sbyte>
            "short", typeof<int16>
            "Int16", typeof<int16>
            "ushort", typeof<uint16>
            "UInt16", typeof<uint16>
            "int", typeof<int>
            "Int", typeof<int>
            "int32", typeof<int32>
            "Int32", typeof<int32>
            "uint", typeof<uint32>
            "UInt32", typeof<uint32>
            "long", typeof<int64>
            "Long", typeof<int64>
            "int64", typeof<int64>
            "Int64", typeof<int64>
            "ulong", typeof<uint64>
            "UInt64", typeof<uint64>
            "nint", typeof<nativeint>
            "nuint", typeof<unativeint>
            "float", typeof<single>
            "Single", typeof<single>
            "double", typeof<double>
            "Double", typeof<double>
            "decimal", typeof<decimal>
            "Decimal", typeof<decimal>
            "char", typeof<char>
            "Char", typeof<char>
            "void", typeof<Void>
            "Void", typeof<Void>
            "List", typedefof<List<_>>
            "IList", typedefof<IList<_>>
            "IReadOnlyList", typedefof<IReadOnlyList<_>>
            "IEnumerable", typedefof<IEnumerable<_>>
            "Dictionary", typedefof<Dictionary<_, _>>
            "IDictionary", typedefof<IDictionary<_, _>>
            "IReadOnlyDictionary", typedefof<IReadOnlyDictionary<_, _>>
            "IComparable", typeof<IComparable>
            "IComparable`1", typedefof<IComparable<_>>
            "IComparableGeneric", typedefof<IComparable<_>>
            "IDisposable", typeof<IDisposable>
            "HashSet", typedefof<HashSet<_>>
            "ISet", typedefof<ISet<_>>
            "Task", typedefof<Threading.Tasks.Task<_>>
            "ValueTask", typedefof<Threading.Tasks.ValueTask<_>>
            "Nullable", typedefof<Nullable<_>>
            "Vector", typeof<PersistentVector>
            "Map", typeof<PersistentMap>
            "Set", typeof<PersistentSet>
            "Exception", typeof<Exception>
            "SystemException", typeof<SystemException>
            "ApplicationException", typeof<ApplicationException>
            "ArgumentException", typeof<ArgumentException>
            "ArgumentNullException", typeof<ArgumentNullException>
            "ArgumentOutOfRangeException", typeof<ArgumentOutOfRangeException>
            "ArithmeticException", typeof<ArithmeticException>
            "DivideByZeroException", typeof<DivideByZeroException>
            "OverflowException", typeof<OverflowException>
            "FormatException", typeof<FormatException>
            "InvalidCastException", typeof<InvalidCastException>
            "InvalidOperationException", typeof<InvalidOperationException>
            "NotImplementedException", typeof<NotImplementedException>
            "NotSupportedException", typeof<NotSupportedException>
            "NullReferenceException", typeof<NullReferenceException>
            "IndexOutOfRangeException", typeof<IndexOutOfRangeException>
            "KeyNotFoundException", typeof<KeyNotFoundException>
            "ObjectDisposedException", typeof<ObjectDisposedException>
            "IOException", typeof<IO.IOException>
            "TimeoutException", typeof<TimeoutException>
            "OperationCanceledException", typeof<OperationCanceledException>
            "AggregateException", typeof<AggregateException>
        ]

    let private resolved = ConcurrentDictionary<string, Type option>(StringComparer.Ordinal)
    let private registeredTypes = ConcurrentDictionary<string, Type>(StringComparer.Ordinal)

    /// Makes a runtime-emitted type (for example a generic record definition)
    /// resolvable from later Hiccup hints.
    let registerType (name: string) (clrType: Type) =
        registeredTypes.[name] <- clrType

    let private findClrType (name: string) =
        let lookup (candidate: string) =
            try
                match Type.GetType(candidate, false, false) with
                | null -> None
                | found -> Some found
            with _ ->
                None

        let fromAssembly (candidate: string) =
            AppDomain.CurrentDomain.GetAssemblies()
            |> Array.tryPick (fun assembly ->
                try
                    match assembly.GetType(candidate, false, false) with
                    | null -> None
                    | found -> Some found
                with _ ->
                    None)

        match lookup name with
        | Some found -> Some found
        | None -> fromAssembly name

    let private resolveName (name: string) =
        match registeredTypes.TryGetValue name with
        | true, found -> Some found
        | false, _ ->
            resolved.GetOrAdd(
                name,
                fun name ->
                    match aliases.TryGetValue name with
                    | true, found -> Some found
                    | false, _ -> findClrType name
            )

    let rec private resolveTypeIn
        (resolver: string -> Type option)
        (typeParameters: Map<string, Type>)
        (path: string)
        (expression: TypeExpr)
        : Result<Type, TypeHintError list> =
        let resolveType path expression = resolveTypeIn resolver typeParameters path expression
        match expression with
        | NamedType(name, arity, args) ->
            bind (traverse path args (fun index arg -> resolveType (sprintf "%s.args[%d]" path index) arg)) (fun resolvedArgs ->
                let resolvedName =
                    match resolver name with
                    | Some found -> Some found
                    | None when not resolvedArgs.IsEmpty -> resolver (sprintf "%s`%d" name resolvedArgs.Length)
                    | None ->
                        match arity with
                        | Some count -> resolver (sprintf "%s`%d" name count)
                        | None -> None
                match resolvedName with
                | None -> fail path (sprintf "Unknown CLR type %s." name)
                | Some candidate ->
                    if resolvedArgs.IsEmpty then
                        match arity with
                        | Some expected when candidate.IsGenericTypeDefinition && candidate.GetGenericArguments().Length <> expected ->
                            fail path (sprintf "%s has arity %d, not %d." name (candidate.GetGenericArguments().Length) expected)
                        | _ -> Ok candidate
                    elif candidate.IsGenericTypeDefinition then
                        let expected = candidate.GetGenericArguments().Length
                        match arity with
                        | Some declared when declared <> resolvedArgs.Length ->
                            fail path (sprintf "Generic arity %d does not match %d type arguments." declared resolvedArgs.Length)
                        | _ when expected <> resolvedArgs.Length ->
                            fail path (sprintf "%s needs %d type arguments, got %d." name expected resolvedArgs.Length)
                        | _ ->
                            try
                                Ok(candidate.MakeGenericType(resolvedArgs |> Array.ofList))
                            with error ->
                                fail path (sprintf "Cannot construct %s: %s" name error.Message)
                    else
                        // A plain name like IComparable may be used with
                        // C#-style arguments; fall back to the arity-suffixed
                        // generic definition.
                        match resolver (sprintf "%s`%d" name resolvedArgs.Length) with
                        | Some generic when generic.IsGenericTypeDefinition ->
                            try
                                Ok(generic.MakeGenericType(resolvedArgs |> Array.ofList))
                            with error ->
                                fail path (sprintf "Cannot construct %s: %s" name error.Message)
                        | _ -> fail path (sprintf "%s is not a generic type definition." name))
        | TypeParameter name ->
            match typeParameters.TryFind name with
            | Some bound -> Ok bound
            | None -> fail path (sprintf "Type parameter %s is unbound in this context." name)
        | ArrayType(element, rank) ->
            bind (resolveType (path + ".element") element) (fun resolvedElement ->
                try
                    if rank = 1 then Ok(resolvedElement.MakeArrayType())
                    else Ok(resolvedElement.MakeArrayType rank)
                with error ->
                    fail path (sprintf "Cannot construct array type: %s" error.Message))
        | PointerType element ->
            bind (resolveType (path + ".element") element) (fun resolvedElement ->
                try Ok(resolvedElement.MakePointerType())
                with error -> fail path (sprintf "Cannot construct pointer type: %s" error.Message))
        | ByRefType element ->
            bind (resolveType (path + ".element") element) (fun resolvedElement ->
                try Ok(resolvedElement.MakeByRefType())
                with error -> fail path (sprintf "Cannot construct byref type: %s" error.Message))

    let private resolveType path expression = resolveTypeIn resolveName Map.empty path expression

    let resolve (hint: TypeHint) : Result<Type, TypeHintError list> =
        match hint with
        | TypeReference expression -> resolveType "$" expression
        | TypeDefinitionHint definition ->
            fail "$" (sprintf "Type definition %s is a declaration, not a runtime type reference." definition.Name)

    let resolveWithTypeParameters
        (typeParameters: Map<string, Type>)
        (hint: TypeHint)
        : Result<Type, TypeHintError list> =
        match hint with
        | TypeReference expression -> resolveTypeIn resolveName typeParameters "$" expression
        | TypeDefinitionHint definition ->
            fail "$" (sprintf "Type definition %s is a declaration, not a runtime type reference." definition.Name)

    let resolveWithTypeParametersAndResolver
        (resolver: string -> Type option)
        (typeParameters: Map<string, Type>)
        (hint: TypeHint)
        : Result<Type, TypeHintError list> =
        match hint with
        | TypeReference expression -> resolveTypeIn resolver typeParameters "$" expression
        | TypeDefinitionHint definition ->
            fail "$" (sprintf "Type definition %s is a declaration, not a runtime type reference." definition.Name)

    let tryResolveName (name: string) : Type option = resolveName name

    let tryResolveForm (form: obj) : Result<Type option, TypeHintError list> =
        match parse form with
        | Ok(TypeReference expression) -> resolveType "$" expression |> Result.map Some
        | Ok(TypeDefinitionHint definition) ->
            fail "$" (sprintf "Type definition %s cannot be used as a runtime type." definition.Name)
        | Error errors -> Error errors

    let tryResolveFormWithResolver
        (resolver: string -> Type option)
        (form: obj)
        : Result<Type option, TypeHintError list> =
        match parse form with
        | Ok(TypeReference expression) ->
            resolveTypeIn resolver Map.empty "$" expression |> Result.map Some
        | Ok(TypeDefinitionHint definition) ->
            fail "$" (sprintf "Type definition %s cannot be used as a runtime type." definition.Name)
        | Error errors -> Error errors

    let private tryResolveFormWith
        (resolver: string -> Type option)
        (form: obj)
        : Result<Type option, TypeHintError list> =
        match form with
        | :? Symbol as symbol ->
            Ok(resolver (symbol.Namespace |> Option.map (fun ns -> ns + "." + symbol.Name) |> Option.defaultValue symbol.Name))
        | :? string as text -> Ok(resolver text)
        | :? Keyword as keyword ->
            match keyword.Namespace with
            | Some namespaceName -> Ok(resolver (namespaceName + "." + keyword.Name))
            | None -> Ok(resolver keyword.Name)
        | :? PersistentVector -> tryResolveFormWithResolver resolver form
        | _ -> Ok None

    /// Resolves a type hint form that may be a symbol, keyword, string, or a
    /// C#-shaped vector. Used by runtime type tests and other deferred hints.
    let tryResolveFormOrName (form: obj) : Result<Type option, TypeHintError list> =
        tryResolveFormWith resolveName form

    let tryResolveFormOrNameWithResolver
        (resolver: string -> Type option)
        (form: obj)
        : Result<Type option, TypeHintError list> =
        tryResolveFormWith resolver form

    let tryParseMetadata (metadata: IPersistentMap) : Result<TypeHint option, TypeHintError list> =
        match metadata.TryGet tagKey with
        | None -> Ok None
        | Some form -> parse form |> Result.map Some

    let tryResolveMetadata (metadata: IPersistentMap) : Result<Type option, TypeHintError list> =
        match tryParseMetadata metadata with
        | Ok None -> Ok None
        | Ok(Some hint) -> resolve hint |> Result.map Some
        | Error errors -> Error errors

    let tryResolveMetadataWithTypeParameters
        (typeParameters: Map<string, Type>)
        (metadata: IPersistentMap)
        : Result<Type option, TypeHintError list> =
        match tryParseMetadata metadata with
        | Ok None -> Ok None
        | Ok(Some hint) -> resolveWithTypeParameters typeParameters hint |> Result.map Some
        | Error errors -> Error errors
