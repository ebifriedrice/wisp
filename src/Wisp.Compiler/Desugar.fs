namespace Wisp.Compiler

open System
open System.Collections.Generic
open System.Reflection
open Wisp.Core

type RecurTarget =
    { Arity: int
      HasRest: bool }

type BindingPattern =
    { Form: obj
      Kind: BindingPatternKind }

and BindingPatternKind =
    | PatternSymbol of string
    | PatternIgnore
    | PatternVector of elements: BindingPattern list * rest: BindingPattern option * asPattern: BindingPattern option
    | PatternMap of entries: MapEntryPattern list * asPattern: BindingPattern option * defaults: Map<string, obj>

and MapEntryPattern =
    { LookupKey: obj
      Pattern: BindingPattern }

type private MatchStep =
    | MatchTest of CoreExpr
    | MatchBind of string * CoreExpr

type DesugarContext =
    { CurrentNamespace: string
      Locals: Set<string>
      RecurTarget: RecurTarget option
      KnownTypes: System.Collections.Generic.HashSet<string>
      KnownInterfaces: System.Collections.Generic.HashSet<string>
      ResolveTypeName: string -> Type option
      Gensym: unit -> string }

module Desugar =
    let createContext (currentNamespace: string) (resolveTypeName: string -> Type option) =
        let mutable counter = 0
        { CurrentNamespace = currentNamespace
          Locals = Set.empty
          RecurTarget = None
          KnownTypes = System.Collections.Generic.HashSet<string>(StringComparer.Ordinal)
          KnownInterfaces = System.Collections.Generic.HashSet<string>(StringComparer.Ordinal)
          ResolveTypeName = resolveTypeName
          Gensym =
            fun () ->
                counter <- counter + 1
                sprintf "__wisp_sym_%d" counter }

    let private spanOf (form: obj) = SourceMetadata.trySpan form

    /// A type hint may name a record defined earlier in the same top-level
    /// form. Those are deferred to evaluation; malformed hints stay compile
    /// errors.
    let private hasUnknownType (errors: TypeHintError list) =
        errors
        |> List.exists (fun error -> error.Message.StartsWith("Unknown CLR type", StringComparison.Ordinal))

    let private hasDeferredTypeError (errors: TypeHintError list) =
        errors
        |> List.exists (fun error ->
            error.Message.StartsWith("Unknown CLR type", StringComparison.Ordinal)
            || error.Message.Contains("unbound"))

    let private clrTypeName (clrType: Type) =
        if isNull clrType.FullName then clrType.Name else clrType.FullName

    let rec private describeTypeExpr expression =
        match expression with
        | NamedType(name, _, _) -> name
        | TypeParameter name -> name
        | ArrayType(element, rank) ->
            if rank = 1 then describeTypeExpr element + "[]"
            else sprintf "%s[%s]" (describeTypeExpr element) (String(',', rank - 1))
        | PointerType element -> describeTypeExpr element + "*"
        | ByRefType element -> describeTypeExpr element + "&"

    let private hintNotes (errors: TypeHintError list) =
        errors |> List.map (fun error -> sprintf "%s: %s" error.Path error.Message)

    let private validateTypeDefinition
        (context: DesugarContext)
        (nameForm: obj)
        (definition: TypeDefinition option)
        =
        match definition with
        | None -> Ok None
        | Some definition ->
            let errors = ResizeArray<CompilerError>()

            let add code message notes =
                errors.Add(
                    { CompilerError.create code message (spanOf nameForm) with
                        Notes = notes }
                )

            match definition.BaseType with
            | Some baseExpression ->
                match TypeHints.resolveWithTypeParametersAndResolver context.ResolveTypeName Map.empty (TypeReference baseExpression) with
                | Error hintErrors when hasDeferredTypeError hintErrors -> ()
                | Error hintErrors ->
                    add
                        "WISP3032"
                        (sprintf "Cannot resolve base type %s." (describeTypeExpr baseExpression))
                        (hintNotes hintErrors)
                | Ok resolved when resolved = typeof<obj> -> ()
                | Ok resolved when typeof<WispRecordBase>.IsAssignableFrom resolved ->
                    let constructor =
                        resolved.GetConstructor([| typeof<string>; typeof<string[]>; typeof<obj[]> |])
                    if isNull constructor then
                        add
                            "WISP3032"
                            (sprintf
                                "Base type %s must expose a (string, string[], obj[]) constructor."
                                (clrTypeName resolved))
                            []
                | Ok resolved ->
                    add
                        "WISP3032"
                        (sprintf "Base type %s must derive from WispRecordBase." (clrTypeName resolved))
                        []
            | None -> ()

            for interfaceExpression in definition.Interfaces do
                match TypeHints.resolveWithTypeParametersAndResolver context.ResolveTypeName Map.empty (TypeReference interfaceExpression) with
                | Error hintErrors when hasDeferredTypeError hintErrors -> ()
                | Error hintErrors ->
                    add
                        "WISP3033"
                        (sprintf "Cannot resolve interface %s." (describeTypeExpr interfaceExpression))
                        (hintNotes hintErrors)
                | Ok resolved when not resolved.IsInterface ->
                    add
                        "WISP3033"
                        (sprintf "%s is not an interface." (clrTypeName resolved))
                        []
                | Ok resolved ->
                    let hasStaticAbstract =
                        resolved.GetMethods()
                        |> Array.exists (fun methodInfo -> methodInfo.IsStatic && methodInfo.IsAbstract)
                    if hasStaticAbstract then
                        add
                            "WISP3033"
                            (sprintf "Interface %s has static abstract members, which records cannot implement." (clrTypeName resolved))
                            []

            if errors.Count = 0 then Ok(Some definition) else Error(List.ofSeq errors)

    let private fail code message form =
        Error [ CompilerError.create code message (spanOf form) ]

    let private make form node = Core.make (spanOf form) node

    let private asSymbol (form: obj) =
        match form with
        | :? Symbol as symbol -> Some symbol
        | _ -> None

    let private asList (form: obj) =
        match form with
        | :? PersistentList as list -> Some(list.Items |> Seq.toList)
        | _ -> None

    let private asVector (form: obj) =
        match form with
        | :? PersistentVector as vector -> Some(vector.Elements |> Seq.toList)
        | _ -> None

    let private asMap (form: obj) =
        match form with
        | :? PersistentMap as map -> Some(map.Entries |> Seq.toList)
        | _ -> None

    let private asSet (form: obj) =
        match form with
        | :? PersistentSet as set -> Some(set.Elements |> Seq.toList)
        | _ -> None

    let private unqualifiedName (symbol: Symbol) =
        if symbol.Namespace.IsNone then Some symbol.Name else None

    let private isNamed name (symbol: Symbol) =
        symbol.Namespace.IsNone && symbol.Name = name

    /// Definition forms take an optional docstring immediately after the name.
    let private splitDocstring (forms: obj list) =
        match forms with
        | (:? string as doc) :: rest -> Some doc, rest
        | _ -> None, forms

    let private attachDoc (symbol: Symbol) (doc: string option) =
        match doc with
        | None -> symbol
        | Some text ->
            let metadata = PersistentMap.Empty.Assoc(Keyword.Intern "doc", box text)
            symbol.WithMeta(Meta.merge symbol.Meta (Some metadata))

    let private builtinRef name =
        Core.make None (VarRef(Symbol.Create name))

    let private resolveSymbol (context: DesugarContext) (symbol: Symbol) =
        match unqualifiedName symbol with
        | Some name when context.Locals.Contains name -> make symbol (Local name)
        | _ -> make symbol (VarRef symbol)

    let private isKeywordNamed name (form: obj) =
        match form with
        | :? Keyword as keyword -> keyword.Namespace.IsNone && keyword.Name = name
        | _ -> false

    let private patternErrors form message =
        [ CompilerError.create "WISP3029" message (spanOf form) ]

    let private patternFail form message = Error(patternErrors form message)

    let rec private parsePattern (form: obj) : Result<BindingPattern, CompilerError list> =
        match form with
        | :? Symbol as symbol ->
            if isNamed "&" symbol then
                patternFail form "& is only valid as a rest marker inside a vector binding."
            else
                let kind = if symbol.Name = "_" then PatternIgnore else PatternSymbol symbol.Name
                Ok { Form = form; Kind = kind }
        | :? PersistentVector as vector -> parseVectorPattern vector
        | :? PersistentMap as map -> parseMapPattern map
        | _ -> patternFail form "Binding forms must be symbols, vectors, maps, or _."

    and private parseVectorPattern (vector: PersistentVector) : Result<BindingPattern, CompilerError list> =
        let items = vector.Elements |> Seq.toList
        let build elements rest asPattern =
            { Form = vector :> obj
              Kind = PatternVector(List.rev elements, rest, asPattern) }

        let rec loop
            (elements: BindingPattern list)
            (rest: BindingPattern option)
            (asPattern: BindingPattern option)
            (remaining: obj list)
            =
            match remaining with
            | [] -> Ok(build elements rest asPattern)
            | head :: tail ->
                match head with
                | :? Symbol as symbol when isNamed "&" symbol ->
                    if rest.IsSome then
                        patternFail head "A vector binding can contain only one & rest marker."
                    else
                        match tail with
                        | restForm :: after ->
                            match parsePattern restForm with
                            | Error errors -> Error errors
                            | Ok restPattern ->
                                match after with
                                | [] -> Ok(build elements (Some restPattern) asPattern)
                                | [ asKeyword; asForm ] when isKeywordNamed "as" asKeyword ->
                                    if asPattern.IsSome then
                                        patternFail asKeyword "A vector binding can contain only one :as."
                                    else
                                        parsePattern asForm
                                        |> Result.map (fun asPattern -> build elements (Some restPattern) (Some asPattern))
                                | _ -> patternFail head "Only :as <pattern> may follow a rest binding."
                        | [] -> patternFail head "& must be followed by a binding pattern."
                | :? Keyword as keyword when isKeywordNamed "as" keyword ->
                    if asPattern.IsSome then
                        patternFail head "A vector binding can contain only one :as."
                    else
                        match tail with
                        | [ asForm ] -> parsePattern asForm |> Result.map (fun asPattern -> build elements rest (Some asPattern))
                        | _ -> patternFail head ":as must be the last item in a vector binding."
                | _ ->
                    if rest.IsSome then
                        patternFail head "Only :as <pattern> may follow a rest binding."
                    else
                        match parsePattern head with
                        | Ok pattern -> loop (pattern :: elements) rest asPattern tail
                        | Error errors -> Error errors

        loop [] None None items

    and private parseMapPattern (map: PersistentMap) : Result<BindingPattern, CompilerError list> =
        let mutable entries: MapEntryPattern list = []
        let mutable asPattern: BindingPattern option = None
        let mutable defaults: Map<string, obj> = Map.empty
        let mutable failure: CompilerError list option = None

        let setFailure errors =
            if failure.IsNone then failure <- Some errors

        let addEntry lookupKey pattern =
            entries <- entries @ [ { LookupKey = lookupKey; Pattern = pattern } ]

        let bindingName (value: obj) =
            match value with
            | :? Symbol as symbol -> Some symbol.Name
            | :? Keyword as keyword -> Some keyword.Name
            | _ -> None

        for entry in map.Entries do
            if failure.IsNone then
                match entry.Key with
                | :? Keyword as keyword when keyword.Namespace.IsNone ->
                    match keyword.Name with
                    | "keys"
                    | "strs"
                    | "syms" ->
                        match entry.Value with
                        | :? PersistentVector as names ->
                            for nameForm in names.Elements do
                                if failure.IsNone then
                                    match nameForm with
                                    | :? Symbol as symbol when keyword.Name = "keys" ->
                                        let lookupKeyword =
                                            match symbol.Namespace with
                                            | Some namespaceName -> Keyword.Intern(namespaceName, symbol.Name)
                                            | None -> Keyword.Intern symbol.Name
                                        addEntry
                                            (lookupKeyword :> obj)
                                            { Form = nameForm; Kind = PatternSymbol symbol.Name }
                                    | :? Symbol as symbol when keyword.Name = "strs" ->
                                        addEntry (box symbol.Name) { Form = nameForm; Kind = PatternSymbol symbol.Name }
                                    | :? Symbol as symbol when keyword.Name = "syms" ->
                                        addEntry (symbol :> obj) { Form = nameForm; Kind = PatternSymbol symbol.Name }
                                    | :? Keyword as nameKeyword when keyword.Name = "keys" ->
                                        let symbol = Symbol.Create nameKeyword.Name
                                        addEntry
                                            (nameKeyword :> obj)
                                            { Form = nameForm; Kind = PatternSymbol symbol.Name }
                                    | _ -> setFailure (patternErrors nameForm (sprintf ":%s entries must be symbols." keyword.Name))
                        | _ -> setFailure (patternErrors entry.Value (sprintf ":%s requires a vector." keyword.Name))
                    | "as" ->
                        if asPattern.IsSome then
                            setFailure (patternErrors entry.Key "A map binding can contain only one :as.")
                        else
                            match parsePattern entry.Value with
                            | Ok pattern -> asPattern <- Some pattern
                            | Error errors -> setFailure errors
                    | "or" ->
                        match entry.Value with
                        | :? PersistentMap as defaultsMap ->
                            for defaultEntry in defaultsMap.Entries do
                                if failure.IsNone then
                                    match bindingName defaultEntry.Key with
                                    | Some name -> defaults <- defaults.Add(name, defaultEntry.Value)
                                    | None -> setFailure (patternErrors defaultEntry.Key ":or keys must be symbols or keywords.")
                        | _ -> setFailure (patternErrors entry.Value ":or requires a map.")
                    | other -> setFailure (patternErrors entry.Key (sprintf "Invalid map destructuring directive :%s." other))
                | _ ->
                    match parsePattern entry.Key with
                    | Ok pattern -> addEntry entry.Value pattern
                    | Error errors -> setFailure errors

        match failure with
        | Some errors -> Error errors
        | None ->
            Ok
                { Form = map :> obj
                  Kind = PatternMap(entries, asPattern, defaults) }

    and private parseParameterPatterns (parameters: PersistentVector) : Result<BindingPattern list * BindingPattern option, CompilerError list> =
        let items = parameters.Elements |> Seq.toList
        let rec loop (patterns: BindingPattern list) (rest: BindingPattern option) (remaining: obj list) =
            match remaining with
            | [] -> Ok(List.rev patterns, rest)
            | head :: tail ->
                match head with
                | :? Symbol as symbol when isNamed "&" symbol ->
                    if rest.IsSome then
                        patternFail head "Function parameters can contain only one & rest marker."
                    else
                        match tail with
                        | [ restForm ] ->
                            parsePattern restForm |> Result.map (fun restPattern -> List.rev patterns, Some restPattern)
                        | _ -> patternFail head "& must be followed by exactly one binding pattern."
                | _ ->
                    if rest.IsSome then
                        patternFail head "Only a rest binding may follow &."
                    else
                        match parsePattern head with
                        | Ok pattern -> loop (pattern :: patterns) rest tail
                        | Error errors -> Error errors
        loop [] None items

    let private typeHintTag = Keyword.Intern "tag"

    let private resolveHintNow (context: DesugarContext) (form: obj) : Result<Type option, TypeHintError list> =
        let resolveName (name: string) =
            context.ResolveTypeName name |> Option.orElse (TypeHints.tryResolveName name)
        match form with
        | :? Symbol
        | :? string
        | :? Keyword
        | :? PersistentVector -> TypeHints.tryResolveFormWithResolver resolveName form
        | _ -> Ok None

    let private parseTypeExprHint (form: obj) : Result<TypeExpr, string list> =
        match TypeHints.parse form with
        | Ok(TypeReference expression) -> Ok expression
        | Ok(TypeDefinitionHint _) -> Error [ "type definition hints are not allowed here" ]
        | Error errors -> Error(errors |> List.map (fun error -> sprintf "%s: %s" error.Path error.Message))

    let rec private rewriteTypeParameters (names: Set<string>) (expression: TypeExpr) : TypeExpr =
        match expression with
        | NamedType(name, None, []) when names.Contains name -> TypeParameter name
        | NamedType(name, arity, args) ->
            NamedType(name, arity, args |> List.map (rewriteTypeParameters names))
        | TypeParameter name -> TypeParameter name
        | ArrayType(element, rank) -> ArrayType(rewriteTypeParameters names element, rank)
        | PointerType element -> PointerType(rewriteTypeParameters names element)
        | ByRefType element -> ByRefType(rewriteTypeParameters names element)

    let rec private collectTypeParameters (expression: TypeExpr) : Set<string> =
        match expression with
        | TypeParameter name -> Set.singleton name
        | NamedType(_, _, args) ->
            args |> List.map collectTypeParameters |> Set.unionMany
        | ArrayType(element, _)
        | PointerType element
        | ByRefType element -> collectTypeParameters element

    let private parseGenericHeader (form: obj) : Result<TypeDefinition, string list> =
        let nameResult =
            match form with
            | :? Symbol as symbol -> Ok(Clr.symbolTypeName symbol, [])
            | :? string as text -> Ok(text, [])
            | :? PersistentVector as vector ->
                match vector.Elements |> Seq.toList with
                | head :: rest ->
                    match asSymbol head with
                    | Some symbol -> Ok(Clr.symbolTypeName symbol, rest)
                    | None -> Error [ "A generic definition header must start with a name symbol." ]
                | [] -> Error [ "A generic definition header cannot be empty." ]
            | _ -> Error [ "A definition name must be a symbol or a [Name T ...] header." ]

        match nameResult with
        | Error errors -> Error errors
        | Ok(name, items) ->
            let parameterNames = ResizeArray<string>()
            let whereForms = ResizeArray<obj list>()
            let errors = ResizeArray<string>()

            for item in items do
                match item with
                | :? Symbol as symbol -> parameterNames.Add symbol.Name
                | :? Keyword as keyword -> parameterNames.Add keyword.Name
                | :? string as text -> parameterNames.Add text
                | :? PersistentList as list ->
                    match list.Items |> Seq.toList with
                    | head :: rest when (asSymbol head |> Option.exists (fun symbol -> symbol.Name = "where")) ->
                        whereForms.Add rest
                    | _ -> errors.Add "Generic header entries must be type parameter names or (where ...) clauses."
                | _ -> errors.Add "Generic header entries must be type parameter names or (where ...) clauses."

            let constraints = Dictionary<string, ResizeArray<TypeConstraint>>()
            let addConstraint parameterName constraintValue =
                let entries =
                    match constraints.TryGetValue parameterName with
                    | true, existing -> existing
                    | false, _ ->
                        let created = ResizeArray<TypeConstraint>()
                        constraints.[parameterName] <- created
                        created
                entries.Add constraintValue

            let names = parameterNames |> Seq.toList |> Set.ofList

            for whereRest in whereForms do
                let explicitParameter, constraintForms =
                    match whereRest with
                    | (:? Symbol as symbol) :: rest when names.Contains symbol.Name -> Some symbol.Name, rest
                    | _ -> None, whereRest
                if constraintForms.IsEmpty then
                    errors.Add "A (where ...) clause needs at least one constraint."
                let soleParameter =
                    if parameterNames.Count = 1 then Some parameterNames.[0] else None
                for constraintForm in constraintForms do
                    match constraintForm with
                    | :? Keyword as keyword when keyword.Namespace.IsNone ->
                        let parameterName =
                            match explicitParameter with
                            | Some name -> Some name
                            | None -> soleParameter
                        let constraintValue =
                            match keyword.Name with
                            | "class" -> Some ReferenceTypeConstraint
                            | "struct" -> Some ValueTypeConstraint
                            | "new" -> Some DefaultConstructorConstraint
                            | _ -> None
                        match parameterName, constraintValue with
                        | Some name, Some value when names.Contains name -> addConstraint name value
                        | _ -> errors.Add(sprintf "Unknown or ambiguous constraint :%s." keyword.Name)
                    | _ ->
                        match parseTypeExprHint constraintForm with
                        | Error notes -> errors.AddRange notes
                        | Ok expression ->
                            let rewritten = rewriteTypeParameters names expression
                            let referenced = collectTypeParameters rewritten
                            let parameterName =
                                match explicitParameter with
                                | Some name -> Some name
                                | None when referenced.Count = 1 -> referenced |> Set.toList |> List.tryHead
                                | None -> soleParameter
                            match parameterName with
                            | Some name when names.Contains name -> addConstraint name (InterfaceConstraint rewritten)
                            | _ -> errors.Add "A (where ...) constraint must refer to exactly one type parameter."

            if errors.Count > 0 then
                Error(List.ofSeq errors)
            else
                let parameters =
                    parameterNames
                    |> Seq.map (fun name ->
                        { Name = name
                          Variance = Invariant
                          Constraints =
                            match constraints.TryGetValue name with
                            | true, entries -> List.ofSeq entries
                            | false, _ -> [] })
                    |> Seq.toList
                Ok(
                    { Name = name
                      Arity = parameters.Length
                      TypeParameters = parameters
                      BaseType = None
                      Interfaces = [] }
                )

    let private normalizeFieldHint (parameterNames: Set<string>) (form: obj) =
        let parameterReference name =
            PersistentVector.OfSeq [ Keyword.Intern "param" :> obj; Symbol.Create name :> obj ] :> obj
        match form with
        | :? Symbol as symbol when parameterNames.Contains symbol.FullName ->
            parameterReference symbol.Name
        | :? Keyword as keyword when parameterNames.Contains keyword.Name ->
            parameterReference keyword.Name
        | :? PersistentVector as vector when vector.Count = 1 ->
            match vector.Nth 0 with
            | :? Symbol as symbol when parameterNames.Contains symbol.FullName ->
                parameterReference symbol.Name
            | _ -> form
        | _ -> form

    let private parseNewRecordFields
        (parameterNames: Set<string>)
        (fieldsForm: obj)
        : Result<RecordField list, CompilerError list> =
        match fieldsForm with
        | :? PersistentVector as vector ->
            let fields = ResizeArray<RecordField>()
            let mutable error = None
            for fieldForm in vector.Elements do
                if error.IsNone then
                    match asSymbol fieldForm with
                    | None -> error <- Some [ CompilerError.create "WISP3023" "Fields must all be symbols." (spanOf fieldForm) ]
                    | Some fieldSymbol ->
                        let hintForm =
                            match fieldSymbol.Meta with
                            | Some metadata -> metadata.TryGet typeHintTag |> Option.map (normalizeFieldHint parameterNames)
                            | None -> None
                        match hintForm with
                        | None ->
                            fields.Add(
                                { Name = fieldSymbol.Name
                                  FieldType = typeof<obj>
                                  Hint = None }
                            )
                        | Some hintForm ->
                            match TypeHints.parse hintForm with
                            | Ok(TypeReference _) ->
                                fields.Add(
                                    { Name = fieldSymbol.Name
                                      FieldType = typeof<obj>
                                      Hint = Some hintForm }
                                )
                            | Ok(TypeDefinitionHint _) ->
                                error <-
                                    Some
                                        [ CompilerError.create
                                              "WISP3028"
                                              (sprintf "Field %s cannot use a type definition as a field type." fieldSymbol.Name)
                                              (spanOf fieldForm) ]
                            | Error errors ->
                                let notes =
                                    errors |> List.map (fun error -> sprintf "%s: %s" error.Path error.Message)
                                error <-
                                    Some
                                        [ { CompilerError.create
                                                "WISP3028"
                                                (sprintf "Invalid type hint for field %s." fieldSymbol.Name)
                                                (spanOf fieldForm) with
                                              Notes = notes } ]
            match error with
            | Some errors -> Error errors
            | None ->
                let names = fields |> Seq.map (fun field -> field.Name) |> Seq.toList
                if names |> List.distinct |> List.length <> names.Length then
                    Error [ CompilerError.create "WISP3028" "Field names must be unique." (spanOf fieldsForm) ]
                else
                    Ok(List.ofSeq fields)
        | _ ->
            Error [ CompilerError.create "WISP3023" "A definition needs a field vector." (spanOf fieldsForm) ]

    let private parseTypeDefinitionClauses
        (isStruct: bool)
        (clauses: obj list)
        : Result<TypeExpr option * TypeExpr list, CompilerError list> =
        let mutable baseType = None
        let interfaces = ResizeArray<TypeExpr>()
        let mutable error = None

        for clause in clauses do
            if error.IsNone then
                match asList clause with
                | Some(head :: rest) ->
                    match asSymbol head with
                    | Some symbol when symbol.Namespace.IsNone && symbol.Name = "inherits" ->
                        if isStruct then
                            error <- Some [ CompilerError.create "WISP3023" "defstruct cannot inherit; use defclass." (spanOf clause) ]
                        else
                            match rest with
                            | [ baseForm ] ->
                                match parseTypeExprHint baseForm with
                                | Ok expression ->
                                    if baseType.IsSome then
                                        error <- Some [ CompilerError.create "WISP3032" "A definition can have only one (inherits ...) clause." (spanOf clause) ]
                                    else
                                        baseType <- Some expression
                                | Error notes ->
                                    error <-
                                        Some
                                            [ { CompilerError.create "WISP3032" "Invalid base type." (spanOf baseForm) with
                                                  Notes = notes } ]
                            | _ -> error <- Some [ CompilerError.create "WISP3032" "(inherits ...) expects exactly one base type." (spanOf clause) ]
                    | Some symbol when symbol.Namespace.IsNone && symbol.Name = "implements" ->
                        for interfaceForm in rest do
                            match parseTypeExprHint interfaceForm with
                            | Ok expression -> interfaces.Add expression
                            | Error notes ->
                                error <-
                                    Some
                                        [ { CompilerError.create "WISP3033" "Invalid interface." (spanOf interfaceForm) with
                                              Notes = notes } ]
                    | _ -> error <- Some [ CompilerError.create "WISP3023" "Definition clauses must be (inherits ...) or (implements ...)." (spanOf clause) ]
                | _ -> error <- Some [ CompilerError.create "WISP3023" "Invalid definition clause." (spanOf clause) ]

        match error with
        | Some errors -> Error errors
        | None -> Ok(baseType, List.ofSeq interfaces)

    let private localExpr name = Core.make None (Local name)

    let private builtinCall name arguments span =
        Core.make span (Call(Core.make None (VarRef(Symbol.Create name)), arguments))

    let private constantExpr value = Core.make None (Const value)

    let private literalLookupExpr (value: obj) =
        match value with
        | :? Symbol -> Core.make None (Quote value)
        | _ -> Core.make None (Const value)

    let private wrapBindings span (bindings: (string * CoreExpr) list) (body: CoreExpr) =
        bindings
        |> List.foldBack (fun (name, value) acc -> Core.make span (Let(name, value, acc))) <| body

    let rec private desugarBody context (forms: obj list) =
        match forms with
        | [] -> Ok(Core.nil None)
        | [ form ] -> desugarForm context form
        | forms ->
            let rec loop acc remaining =
                match remaining with
                | [] -> Ok(Core.make (forms |> List.tryHead |> Option.bind spanOf) (Do(List.rev acc)))
                | form :: rest ->
                    match desugarForm context form with
                    | Ok expression -> loop (expression :: acc) rest
                    | Error errors -> Error errors
            loop [] forms

    and private desugarArguments context (forms: obj list) =
        let rec loop acc remaining =
            match remaining with
            | [] -> Ok(List.rev acc)
            | form :: rest ->
                match desugarForm context form with
                | Ok expression -> loop (expression :: acc) rest
                | Error errors -> Error errors
        loop [] forms

    and private lowerPattern
        (context: DesugarContext)
        (pattern: BindingPattern)
        (valueExpr: CoreExpr)
        : Result<(string * CoreExpr) list * DesugarContext, CompilerError list> =
        let span = spanOf pattern.Form

        let lowerPending current (bindings: ResizeArray<string * CoreExpr>) pending =
            let rec loop current remaining =
                match remaining with
                | [] -> Ok(List.ofSeq bindings, current)
                | (child: BindingPattern, expression: CoreExpr) :: rest ->
                    match lowerPattern current child expression with
                    | Ok(childBindings, nextContext) ->
                        bindings.AddRange childBindings
                        loop nextContext rest
                    | Error errors -> Error errors
            loop current pending

        match pattern.Kind with
        | PatternSymbol name ->
            if name = "_" then
                let ignored = context.Gensym()
                Ok([ (ignored, valueExpr) ], context)
            else
                Ok([ (name, valueExpr) ], { context with Locals = context.Locals.Add name })
        | PatternIgnore ->
            let ignored = context.Gensym()
            Ok([ (ignored, valueExpr) ], context)
        | PatternVector(elements, rest, asPattern) ->
            let temp = context.Gensym()
            let bindings = ResizeArray<string * CoreExpr>()
            bindings.Add((temp, valueExpr))
            let pending =
                let asPending =
                    asPattern
                    |> Option.map (fun child -> child, localExpr temp)
                    |> Option.toList
                let elementPending =
                    elements
                    |> List.mapi (fun index child ->
                        let nth = builtinCall "nth" [ localExpr temp; constantExpr (box index); constantExpr null ] span
                        child, nth)
                let restPending =
                    rest
                    |> Option.map (fun child ->
                        let dropped =
                            builtinCall "drop" [ constantExpr (box elements.Length); localExpr temp ] span
                        child, dropped)
                    |> Option.toList
                asPending @ elementPending @ restPending
            lowerPending context bindings pending
        | PatternMap(entries, asPattern, defaults) ->
            let temp = context.Gensym()
            let bindings = ResizeArray<string * CoreExpr>()
            bindings.Add((temp, valueExpr))
            let mutable current = context
            let mutable failure: CompilerError list option = None

            match asPattern with
            | Some child ->
                match lowerPattern current child (localExpr temp) with
                | Ok(childBindings, nextContext) ->
                    bindings.AddRange childBindings
                    current <- nextContext
                | Error errors -> failure <- Some errors
            | None -> ()

            for entry in entries do
                if failure.IsNone then
                    let defaultResult =
                        match entry.Pattern.Kind with
                        | PatternSymbol name when name <> "_" ->
                            match defaults.TryFind name with
                            | Some form -> desugarForm current form |> Result.map Some
                            | None -> Ok None
                        | _ -> Ok None
                    match defaultResult with
                    | Error errors -> failure <- Some errors
                    | Ok defaultExpr ->
                        let lookup = literalLookupExpr entry.LookupKey
                        let arguments =
                            match defaultExpr with
                            | Some value -> [ localExpr temp; lookup; value ]
                            | None -> [ localExpr temp; lookup ]
                        let getExpr = builtinCall "get" arguments span
                        match lowerPattern current entry.Pattern getExpr with
                        | Ok(childBindings, nextContext) ->
                            bindings.AddRange childBindings
                            current <- nextContext
                        | Error errors -> failure <- Some errors

            match failure with
            | Some errors -> Error errors
            | None -> Ok(List.ofSeq bindings, current)

    and private buildFunction
        (context: DesugarContext)
        (functionName: string option)
        (parameters: PersistentVector)
        (bodyForms: obj list)
        (span: TextSpan option)
        : Result<string list * string option * CoreExpr, CompilerError list> =
        match parseParameterPatterns parameters with
        | Error errors -> Error errors
        | Ok(parameterPatterns, restPattern) ->
            let bindings = ResizeArray<string * CoreExpr>()
            let slots = ResizeArray<string>()
            let mutable current = context
            match functionName with
            | Some name -> current <- { current with Locals = current.Locals.Add name }
            | None -> ()
            let mutable failure: CompilerError list option = None

            for pattern in parameterPatterns do
                if failure.IsNone then
                    let slot = current.Gensym()
                    slots.Add slot
                    match lowerPattern current pattern (localExpr slot) with
                    | Ok(childBindings, nextContext) ->
                        bindings.AddRange childBindings
                        current <- nextContext
                    | Error errors -> failure <- Some errors

            let restSlot =
                match restPattern with
                | Some pattern when failure.IsNone ->
                    let slot = current.Gensym()
                    match lowerPattern current pattern (localExpr slot) with
                    | Ok(childBindings, nextContext) ->
                        bindings.AddRange childBindings
                        current <- nextContext
                        Some slot
                    | Error errors ->
                        failure <- Some errors
                        None
                | _ -> None

            match failure with
            | Some errors -> Error errors
            | None ->
                let bodyContext =
                    { current with
                        RecurTarget =
                            Some
                                { Arity = slots.Count
                                  HasRest = restPattern.IsSome } }
                match desugarBody bodyContext bodyForms with
                | Error errors -> Error errors
                | Ok body ->
                    Ok(List.ofSeq slots, restSlot, wrapBindings span (List.ofSeq bindings) body)

    and private desugarTypeResult
        (context: DesugarContext)
        (form: obj)
        : Result<Type option, TypeHintError list> =
        resolveHintNow context form

    and private desugarType (context: DesugarContext) (form: obj) =
        desugarTypeResult context form |> Result.toOption |> Option.bind id

    and private desugarSymbol (context: DesugarContext) (symbol: Symbol) =
        resolveSymbol context symbol

    and private desugarSpecial (context: DesugarContext) (form: obj) (items: obj list) (name: string) =
        let arguments = items.Tail
        match name with
        | "quote" ->
            match arguments with
            | [ value ] -> Ok(make form (Quote value))
            | _ -> fail "WISP3003" "quote expects exactly one form." form
        | "if" ->
            match arguments with
            | [ condition; whenTrue ] ->
                match desugarForm context condition, desugarForm context whenTrue with
                | Ok conditionExpr, Ok trueExpr -> Ok(make form (If(conditionExpr, trueExpr, Core.nil (spanOf form))))
                | Error errors, _ -> Error errors
                | _, Error errors -> Error errors
            | [ condition; whenTrue; whenFalse ] ->
                match desugarForm context condition, desugarForm context whenTrue, desugarForm context whenFalse with
                | Ok conditionExpr, Ok trueExpr, Ok falseExpr -> Ok(make form (If(conditionExpr, trueExpr, falseExpr)))
                | Error errors, _, _ -> Error errors
                | _, Error errors, _ -> Error errors
                | _, _, Error errors -> Error errors
            | _ -> fail "WISP3004" "if expects two or three forms." form
        | "do" ->
            desugarBody context arguments
        | "def" ->
            match arguments with
            | [] -> fail "WISP3005" "def expects a name." form
            | nameForm :: rest ->
                match asSymbol nameForm with
                | None -> fail "WISP3005" "def expects a symbol name." nameForm
                | Some symbol ->
                    let doc, body = splitDocstring rest
                    let symbol = attachDoc symbol doc
                    match body with
                    | [] -> Ok(make form (Def(symbol, Core.nil (spanOf form))))
                    | [ value ] ->
                        match desugarForm context value with
                        | Ok valueExpr -> Ok(make form (Def(symbol, valueExpr)))
                        | Error errors -> Error errors
                    | _ -> fail "WISP3006" "def expects at most one value form." form
        | "defn"
        | "defactive" ->
            match arguments with
            | [] -> fail "WISP3007" "defn expects a name." form
            | nameForm :: rest ->
                match asSymbol nameForm with
                | None -> fail "WISP3007" "defn expects a symbol name." nameForm
                | Some symbol ->
                    let doc, rest = splitDocstring rest
                    let symbol = attachDoc symbol doc
                    match rest with
                    | parametersForm :: bodyForms ->
                        match parametersForm with
                        | :? PersistentVector as parameters ->
                            match buildFunction context (Some symbol.Name) parameters bodyForms (spanOf form) with
                            | Error errors -> Error errors
                            | Ok(slots, restSlot, body) ->
                                let functionExpr =
                                    make form (Lambda(Some symbol.Name, slots, restSlot, body))
                                Ok(make form (Def(symbol, functionExpr)))
                        | _ -> fail "WISP3008" "defn parameters must be a vector." parametersForm
                    | [] -> fail "WISP3008" "defn needs a parameter vector." form
        | "deftype"
        | "defclass"
        | "defstruct" ->
            let isStruct = name = "defstruct"
            match arguments with
            | nameOrHeader :: rest ->
                let doc, rest = splitDocstring rest
                match rest with
                | fieldsForm :: clauses ->
                    match parseGenericHeader nameOrHeader with
                    | Error notes ->
                        Error
                            [ { CompilerError.create
                                    "WISP3023"
                                    "Invalid definition header."
                                    (spanOf nameOrHeader) with
                                  Notes = notes } ]
                    | Ok header ->
                        context.KnownTypes.Add(header.Name) |> ignore
                        match parseTypeDefinitionClauses isStruct clauses with
                        | Error errors -> Error errors
                        | Ok(baseType, interfaces) ->
                            let parameterNames =
                                header.TypeParameters |> List.map (fun parameter -> parameter.Name) |> Set.ofList
                            match parseNewRecordFields parameterNames fieldsForm with
                            | Error errors -> Error errors
                            | Ok fields ->
                                let definition =
                                    { header with
                                        BaseType = baseType
                                        Interfaces = interfaces }
                                match validateTypeDefinition context nameOrHeader (Some definition) with
                                | Error errors -> Error errors
                                | Ok _ ->
                                    DefinitionDocs.registerOption (DefinitionDocs.typeKey header.Name) doc
                                    Ok(make form (DefineType(header.Name, fields, Some definition, isStruct)))
                | _ ->
                    fail "WISP3023" (sprintf "%s expects a name, field vector, and optional clauses." name) form
            | [] -> fail "WISP3023" (sprintf "%s expects a name." name) form
        | "defdelegate" ->
            match arguments with
            | nameForm :: rest ->
                let doc, rest = splitDocstring rest
                match rest with
                | [ parametersForm ] ->
                    match asSymbol nameForm, parametersForm with
                    | Some symbol, (:? PersistentVector as parameters) ->
                        let returnResult =
                            match symbol.Meta with
                            | Some metadata ->
                                match metadata.TryGet typeHintTag with
                                | Some hint -> resolveHintNow context hint
                                | None -> Ok(Some typeof<Void>)
                            | None -> Ok(Some typeof<Void>)
                        match returnResult with
                        | Error errors ->
                            let notes =
                                errors |> List.map (fun error -> sprintf "%s: %s" error.Path error.Message)
                            Error
                                [ { CompilerError.create
                                        "WISP3040"
                                        "Invalid delegate return type."
                                        (spanOf nameForm) with
                                      Notes = notes } ]
                        | Ok None -> fail "WISP3040" "Delegate return type is unknown." nameForm
                        | Ok(Some returnType) ->
                            let parameterResults =
                                parameters.Elements
                                |> Seq.toList
                                |> List.map (fun parameterForm ->
                                    match asSymbol parameterForm with
                                    | None ->
                                        Error
                                            [ CompilerError.create
                                                  "WISP3040"
                                                  "Delegate parameter names must be symbols."
                                                  (spanOf parameterForm) ]
                                    | Some parameterSymbol ->
                                        let hasMetadata (name: string) =
                                            match parameterSymbol.Meta with
                                            | Some metadata ->
                                                match metadata.TryGet(Keyword.Intern name) with
                                                | Some value -> Object.Equals(value, box true)
                                                | None -> false
                                            | None -> false
                                        let hint =
                                            parameterSymbol.Meta
                                            |> Option.bind (fun metadata -> metadata.TryGet typeHintTag)
                                        let hintMode =
                                            match hint with
                                            | Some (:? Symbol as symbol) ->
                                                if symbol.Name.Contains "&:readonly" then Some ParameterAttributes.In
                                                elif symbol.Name.Contains "&:out" then Some ParameterAttributes.Out
                                                else None
                                            | Some (:? string as text) ->
                                                if text.Contains "&:readonly" then Some ParameterAttributes.In
                                                elif text.Contains "&:out" then Some ParameterAttributes.Out
                                                else None
                                            | _ -> None
                                        let attributes =
                                            if hasMetadata "readonly" then ParameterAttributes.In
                                            elif hasMetadata "out" then ParameterAttributes.Out
                                            elif hasMetadata "in" then ParameterAttributes.In
                                            elif hasMetadata "ref" then ParameterAttributes.None
                                            else Option.defaultValue ParameterAttributes.None hintMode
                                        let typeResult =
                                            match hint with
                                            | None -> Ok typeof<obj>
                                            | Some hint ->
                                                match resolveHintNow context hint with
                                                | Ok(Some parameterType) -> Ok parameterType
                                                | Ok None ->
                                                    Error
                                                        [ CompilerError.create
                                                              "WISP3040"
                                                              "Unknown delegate parameter type."
                                                              (spanOf parameterForm) ]
                                                | Error errors ->
                                                    let notes =
                                                        errors
                                                        |> List.map (fun error -> sprintf "%s: %s" error.Path error.Message)
                                                    Error
                                                        [ { CompilerError.create
                                                                "WISP3040"
                                                                "Invalid delegate parameter type."
                                                                (spanOf parameterForm) with
                                                              Notes = notes } ]
                                        typeResult
                                        |> Result.map (fun resolvedType ->
                                            let parameterType =
                                                if resolvedType.IsByRef then resolvedType
                                                elif attributes <> ParameterAttributes.None then resolvedType.MakeByRefType()
                                                else resolvedType
                                            { ParameterType = parameterType
                                              Attributes = attributes }))
                            match parameterResults |> List.tryPick (function Error errors -> Some errors | Ok _ -> None) with
                            | Some errors -> Error errors
                            | None ->
                                let methodParameters = parameterResults |> List.choose (function Ok parameter -> Some parameter | Error _ -> None)
                                if returnType.IsByRef then
                                    fail "WISP3040" "Delegate return-byref signatures are not supported." nameForm
                                else
                                    let delegateName = Clr.symbolTypeName symbol
                                    DefinitionDocs.registerOption (DefinitionDocs.typeKey delegateName) doc
                                    context.KnownTypes.Add delegateName |> ignore
                                    Ok(
                                        make form (
                                            DefineDelegate
                                                { Name = delegateName
                                                  ReturnType = returnType
                                                  Parameters = methodParameters }
                                        )
                                    )
                    | _ -> fail "WISP3040" "defdelegate expects a name and parameter vector." form
                | _ -> fail "WISP3040" "defdelegate expects a name and parameter vector." form
            | [] -> fail "WISP3040" "defdelegate expects a name." form
        | "delegate" ->
            match arguments with
            | [ typeForm; callbackForm ] ->
                match desugarTypeResult context typeForm with
                | Error errors ->
                    let notes = errors |> List.map (fun error -> sprintf "%s: %s" error.Path error.Message)
                    Error
                        [ { CompilerError.create
                                "WISP3040"
                                "Invalid delegate type."
                                (spanOf typeForm) with
                              Notes = notes } ]
                | Ok None -> fail "WISP3040" "Unknown delegate type." typeForm
                | Ok(Some delegateType) when not (typeof<Delegate>.IsAssignableFrom delegateType) ->
                    fail "WISP3040" (sprintf "%O is not a delegate type." delegateType) typeForm
                | Ok(Some delegateType) ->
                    match desugarForm context callbackForm with
                    | Ok callback -> Ok(make form (CreateDelegate(delegateType, callback)))
                    | Error errors -> Error errors
            | _ -> fail "WISP3040" "delegate expects a delegate type and function." form
        | "set!" ->
            match arguments with
            | [ targetForm; valueForm ] ->
                match asSymbol targetForm with
                | None -> fail "WISP3041" "set! expects a var symbol target." targetForm
                | Some symbol ->
                    if context.Locals.Contains symbol.Name then
                        fail "WISP3041" "set! currently supports vars only, not locals." targetForm
                    else
                        match desugarForm context valueForm with
                        | Ok value -> Ok(make form (SetVar(symbol, value)))
                        | Error errors -> Error errors
            | _ -> fail "WISP3041" "set! expects a target and value." form
        | "definterface" ->
            match arguments with
            | nameForm :: rest ->
                let doc, declarations = splitDocstring rest
                match asSymbol nameForm with
                | None -> fail "WISP3035" "definterface expects a name symbol." nameForm
                | Some symbol ->
                    let interfaceName = Clr.symbolTypeName symbol
                    context.KnownInterfaces.Add interfaceName |> ignore
                    let methods = ResizeArray<InterfaceMethod>()
                    let mutable error = None

                    let resolveMemberType (spanForm: obj) (hint: obj) =
                        match resolveHintNow context hint with
                        | Ok(Some resolved) -> Ok resolved
                        | Ok None ->
                            Error
                                [ CompilerError.create
                                      "WISP3035"
                                      (sprintf "Unknown CLR type %s." (Print.printString hint))
                                      (spanOf spanForm) ]
                        | Error errors ->
                            let notes =
                                errors |> List.map (fun error -> sprintf "%s: %s" error.Path error.Message)
                            Error
                                [ { CompilerError.create
                                        "WISP3035"
                                        (sprintf "Cannot resolve CLR type %s." (Print.printString hint))
                                        (spanOf spanForm) with
                                      Notes = notes } ]

                    let setError result =
                        if error.IsNone then error <- Some result

                    for declaration in declarations do
                        if error.IsNone then
                            match asList declaration with
                            | Some(head :: rest) ->
                                match asSymbol head, rest with
                                | Some methodSymbol, [ (:? PersistentVector as parameters) ] ->
                                    let returnResult =
                                        match methodSymbol.Meta with
                                        | Some metadata ->
                                            match metadata.TryGet typeHintTag with
                                            | Some hint -> resolveMemberType methodSymbol hint
                                            | None -> Ok typeof<obj>
                                        | None -> Ok typeof<obj>
                                    match returnResult with
                                    | Error errors -> setError (Error errors)
                                    | Ok returnType ->
                                        let parameterForms =
                                            match parameters.Elements |> Seq.toList with
                                            | _receiver :: rest -> rest
                                            | [] -> []
                                        let parameterResults =
                                            parameterForms
                                            |> List.map (fun parameterForm ->
                                                match asSymbol parameterForm with
                                                | None ->
                                                    Error
                                                        [ CompilerError.create
                                                              "WISP3035"
                                                              "Interface parameter names must be symbols."
                                                              (spanOf parameterForm) ]
                                                | Some parameterSymbol ->
                                                    let hasMetadata (name: string) =
                                                        match parameterSymbol.Meta with
                                                        | Some metadata ->
                                                            match metadata.TryGet(Keyword.Intern name) with
                                                            | Some value -> Object.Equals(value, box true)
                                                            | None -> false
                                                        | None -> false
                                                    let hintForm =
                                                        match parameterSymbol.Meta with
                                                        | Some metadata -> metadata.TryGet typeHintTag
                                                        | None -> None
                                                    let hintMode =
                                                        match hintForm with
                                                        | Some (:? Symbol as symbol) ->
                                                            if symbol.Name.Contains "&:readonly" then Some ParameterAttributes.In
                                                            elif symbol.Name.Contains "&:out" then Some ParameterAttributes.Out
                                                            else None
                                                        | Some (:? string as text) ->
                                                            if text.Contains "&:readonly" then Some ParameterAttributes.In
                                                            elif text.Contains "&:out" then Some ParameterAttributes.Out
                                                            else None
                                                        | _ -> None
                                                    let attributes =
                                                        if hasMetadata "readonly" then ParameterAttributes.In
                                                        elif hasMetadata "out" then ParameterAttributes.Out
                                                        elif hasMetadata "in" then ParameterAttributes.In
                                                        elif hasMetadata "ref" then ParameterAttributes.None
                                                        else Option.defaultValue ParameterAttributes.None hintMode
                                                    let typeResult =
                                                        match hintForm with
                                                        | Some hint -> resolveMemberType parameterForm hint
                                                        | None -> Ok typeof<obj>
                                                    typeResult
                                                    |> Result.map (fun resolvedType ->
                                                        let parameterType =
                                                            if resolvedType.IsByRef then resolvedType
                                                            elif attributes <> ParameterAttributes.None then resolvedType.MakeByRefType()
                                                            else resolvedType
                                                        { ParameterType = parameterType
                                                          Attributes = attributes }))
                                        match parameterResults |> List.tryPick (function Error errors -> Some errors | Ok _ -> None) with
                                        | Some errors -> setError (Error errors)
                                        | None ->
                                            let methodParameters =
                                                parameterResults |> List.choose (function Ok resolved -> Some resolved | Error _ -> None)
                                            methods.Add(
                                                { Name = methodSymbol.Name
                                                  ReturnType = returnType
                                                  Parameters = methodParameters }
                                            )
                                | _ ->
                                    setError (
                                        fail
                                            "WISP3035"
                                            "Interface members must be (name [parameters]) declarations."
                                            declaration
                                    )
                            | _ ->
                                setError (
                                    fail "WISP3035" "Interface members must be (name [parameters]) declarations." declaration
                                )

                    match error with
                    | Some result -> result
                    | None ->
                        DefinitionDocs.registerOption (DefinitionDocs.typeKey interfaceName) doc
                        Ok(
                            make form (
                                DefineInterface
                                    { Name = interfaceName
                                      Methods = List.ofSeq methods }
                            )
                        )
            | [] -> fail "WISP3035" "definterface expects a name." form
        | "defunion" ->
            match arguments with
            | unionNameForm :: rest ->
                let doc, caseForms = splitDocstring rest
                match asSymbol unionNameForm with
                | None -> fail "WISP3036" "defunion expects a name symbol." unionNameForm
                | Some unionSymbol ->
                    let unionName = Clr.symbolTypeName unionSymbol
                    DefinitionDocs.registerOption (DefinitionDocs.typeKey unionName) doc
                    context.KnownInterfaces.Add unionName |> ignore
                    let nodes = ResizeArray<CoreExpr>()
                    nodes.Add(
                        make form (DefineInterface { Name = unionName; Methods = [] })
                    )
                    let caseNames = ResizeArray<string>()
                    let mutable error = None

                    for caseForm in caseForms do
                        if error.IsNone then
                            match asList caseForm with
                            | Some(caseNameForm :: caseRest) ->
                                let caseDoc, caseRest = splitDocstring caseRest
                                match asSymbol caseNameForm with
                                | None -> error <- Some(fail "WISP3036" "Union case names must be symbols." caseNameForm)
                                | Some caseSymbol ->
                                    let fieldsResult =
                                        match caseRest with
                                        | [] -> Ok []
                                        | [ fieldsForm ] -> parseNewRecordFields Set.empty fieldsForm
                                        | _ -> fail "WISP3036" "A union case is (Name [fields...]) or (Name)." caseForm
                                    match fieldsResult with
                                    | Error errors -> error <- Some(Error errors)
                                    | Ok fields ->
                                        let caseName = Clr.symbolTypeName caseSymbol
                                        caseNames.Add caseName
                                        DefinitionDocs.registerOption (DefinitionDocs.typeKey caseName) caseDoc
                                        context.KnownTypes.Add caseName |> ignore
                                        UnionRegistry.register caseName (fields |> List.map (fun field -> field.Name))
                                        let definition =
                                            { Name = caseName
                                              Arity = 0
                                              TypeParameters = []
                                              BaseType = None
                                              Interfaces = [ NamedType(unionName, None, []) ] }
                                        nodes.Add(
                                            make caseForm (DefineType(caseName, fields, Some definition, false))
                                        )
                            | _ -> error <- Some(fail "WISP3036" "A union case is (Name [fields...])." caseForm)

                    match error with
                    | Some result -> result
                    | None ->
                        UnionRegistry.registerUnion unionName (List.ofSeq caseNames)
                        Ok(make form (Do(List.ofSeq nodes)))
            | [] -> fail "WISP3036" "defunion expects a name." form
        | "defmethod" ->
            let privateKey = Keyword.Intern "private"

            let isPrivateSymbol (symbol: Symbol) =
                symbol.Meta
                |> Option.exists (fun metadata ->
                    match metadata.TryGet privateKey with
                    | Some value -> Object.Equals(value, box true)
                    | None -> false)

            let receiverHint (parameters: PersistentVector) =
                match parameters.Elements |> Seq.tryHead with
                | Some form ->
                    match asSymbol form with
                    | Some symbol ->
                        symbol.Meta
                        |> Option.bind (fun metadata -> metadata.TryGet typeHintTag)
                        |> Option.bind (fun hint ->
                            match hint with
                            | :? Symbol as tag -> Some(Clr.symbolTypeName tag)
                            | :? Keyword as tag ->
                                Some(
                                    match tag.Namespace with
                                    | Some namespaceName -> namespaceName + "." + tag.Name
                                    | None -> tag.Name
                                )
                            | :? string as text -> Some text
                            | _ -> None)
                    | None -> None
                | None -> None

            let parameterHints (parameters: PersistentVector) =
                parameters.Elements
                |> Seq.toList
                |> List.map (fun form ->
                    match asSymbol form with
                    | Some symbol ->
                        match symbol.Meta with
                        | Some metadata ->
                            match metadata.TryGet typeHintTag with
                            | Some hint ->
                                match resolveHintNow context hint with
                                | Ok(Some resolved) -> Some resolved
                                | _ -> None
                            | None -> None
                        | None -> None
                    | None -> None)

            let isInterfaceOwner (owner: string) =
                context.KnownInterfaces.Contains owner
                || (context.ResolveTypeName owner |> Option.exists (fun clrType -> clrType.IsInterface))

            let selectOwner (explicitOwner: string option) (receiver: string option) =
                match explicitOwner with
                | Some owner when isInterfaceOwner owner ->
                    match receiver with
                    | Some receiverName -> Ok receiverName
                    | None -> Error "Interface methods need a ^Class receiver hint on the first parameter."
                | Some owner -> Ok owner
                | None ->
                    match receiver with
                    | Some receiverName -> Ok receiverName
                    | None -> Error "Method has no owning class; add a ^Class hint to the first parameter."

            let registerMethod owner methodName (parameters: PersistentVector) bodyForms isPrivate doc =
                match buildFunction context None parameters bodyForms (spanOf form) with
                | Error errors -> Error errors
                | Ok(slots, restSlot, body) ->
                    DefinitionDocs.registerOption (DefinitionDocs.methodKey owner methodName) doc
                    let lambda = make form (Lambda(None, slots, restSlot, body))
                    Ok(make form (DefineMethod(owner, methodName, lambda, isPrivate, context.CurrentNamespace)))

            let registerConstructor owner (parameters: PersistentVector) bodyForms isPrivate doc =
                let hints = parameterHints parameters
                match buildFunction context None parameters bodyForms (spanOf form) with
                | Error errors -> Error errors
                | Ok(slots, restSlot, body) ->
                    DefinitionDocs.registerOption
                        (DefinitionDocs.constructorKey owner parameters.Count)
                        doc
                    let lambda = make form (Lambda(None, slots, restSlot, body))
                    Ok(make form (DefineConstructor(owner, lambda, hints, isPrivate, context.CurrentNamespace)))

            let splitDotted (text: string) =
                let index = text.LastIndexOf '.'
                if index > 0 && index < text.Length - 1 then
                    Some(text.Substring(0, index), text.Substring(index + 1))
                else
                    None

            let isKnownRecordType name =
                context.KnownTypes.Contains name
                || (context.ResolveTypeName name
                    |> Option.exists (fun clrType -> typeof<Wisp.Core.IWispRecord>.IsAssignableFrom clrType))

            let isMethodNameForm (form: obj) =
                match form with
                | :? Keyword
                | :? Symbol -> true
                | _ -> false

            match arguments with
            | targetForm :: rest ->
                match asSymbol targetForm, rest with
                | Some targetSymbol, methodNameForm :: rest when isMethodNameForm methodNameForm ->
                    let methodName =
                        match methodNameForm with
                        | :? Keyword as keyword -> keyword.Name
                        | :? Symbol as symbol -> symbol.Name
                        | :? string as text -> text
                        | _ -> failwith "unreachable"
                    let doc, rest = splitDocstring rest
                    match rest with
                    | parametersForm :: bodyForms when (parametersForm :? PersistentVector) ->
                        let parameters = parametersForm :?> PersistentVector
                        let receiver = receiverHint parameters
                        match selectOwner (Some(Clr.symbolTypeName targetSymbol)) receiver with
                        | Error message -> fail "WISP3024" message form
                        | Ok owner ->
                            registerMethod
                                owner
                                methodName
                                parameters
                                bodyForms
                                (isPrivateSymbol targetSymbol)
                                doc
                    | _ -> fail "WISP3024" "defmethod expects a parameter vector." form
                | Some targetSymbol, rest ->
                    let doc, rest = splitDocstring rest
                    match rest with
                    | parametersForm :: bodyForms when (parametersForm :? PersistentVector) ->
                        let parameters = parametersForm :?> PersistentVector
                        let isPrivate = isPrivateSymbol targetSymbol
                        let targetName = Clr.symbolTypeName targetSymbol
                        let receiver = receiverHint parameters
                        match splitDotted targetName with
                        | Some(owner, methodName) when not (isKnownRecordType targetName) ->
                            match selectOwner (Some owner) receiver with
                            | Error message -> fail "WISP3024" message form
                            | Ok registerOwner ->
                                registerMethod registerOwner methodName parameters bodyForms isPrivate doc
                        | _ when isKnownRecordType targetName ->
                            registerConstructor targetName parameters bodyForms isPrivate doc
                        | _ ->
                            match selectOwner None receiver with
                            | Error message -> fail "WISP3024" message form
                            | Ok owner -> registerMethod owner targetName parameters bodyForms isPrivate doc
                    | _ -> fail "WISP3024" "defmethod expects a parameter vector." form
                | _ -> fail "WISP3024" "defmethod expects a target, parameter vector, and body." form
            | [] -> fail "WISP3024" "defmethod expects a target." form
        | "match" ->
            match arguments with
            | valueForm :: clauseForms ->
                match desugarForm context valueForm with
                | Error errors -> Error errors
                | Ok valueExpr ->
                    let span = spanOf form
                    let valueTemp = context.Gensym()
                    let valueLocal = localExpr valueTemp
                    let failExpr = Core.make span (Throw(constantExpr "no matching pattern"))
                    let boolConst value = Core.make span (Const(box value))
                    let truthy (expression: CoreExpr) = builtinCall "boolean" [ expression ] span
                    let sequentialTest expression = builtinCall "sequential?" [ expression ] span
                    let mapTest expression = builtinCall "map?" [ expression ] span
                    let countTest operatorName expression count =
                        builtinCall operatorName [ builtinCall "count" [ expression ] span; constantExpr (box count) ] span
                    let equalityTest expression literal =
                        builtinCall "=" [ Core.make (spanOf literal) (Const literal); expression ] span
                    let nthExpr expression index =
                        builtinCall "nth" [ expression; constantExpr (box index); constantExpr null ] span

                    let rec compilePattern
                        (patternContext: DesugarContext)
                        (valueExpression: CoreExpr)
                        (pattern: obj)
                        : Result<MatchStep list * DesugarContext, CompilerError list> =
                        let bind name expression = MatchBind(name, expression)
                        let test expression = MatchTest expression

                        match pattern with
                        | null
                        | :? string
                        | :? Keyword
                        | :? bool
                        | :? char ->
                            Ok([ test (equalityTest valueExpression pattern) ], patternContext)
                        | :? Symbol as symbol ->
                            if symbol.Name = "_" then
                                Ok([], patternContext)
                            else
                                let nextContext =
                                    { patternContext with
                                        Locals = patternContext.Locals.Add symbol.Name }
                                match symbol.Meta with
                                | Some metadata ->
                                    match metadata.TryGet typeHintTag with
                                    | Some hint ->
                                        Ok(
                                            [ test (Core.make (spanOf pattern) (TypeTestHint(hint, valueExpression)))
                                              bind symbol.Name valueExpression ],
                                            nextContext
                                        )
                                    | None -> Ok([ bind symbol.Name valueExpression ], nextContext)
                                | None -> Ok([ bind symbol.Name valueExpression ], nextContext)
                        | :? PersistentVector as vector ->
                            match parsePattern vector with
                            | Error errors -> Error errors
                            | Ok parsed ->
                                let elementCount, hasRest =
                                    match parsed.Kind with
                                    | PatternVector(elements, rest, _) -> elements.Length, rest.IsSome
                                    | _ -> 0, true
                                let tests =
                                    [ test (sequentialTest valueExpression)
                                      test (countTest (if hasRest then ">=" else "=") valueExpression elementCount) ]
                                match lowerPattern patternContext parsed valueExpression with
                                | Error errors -> Error errors
                                | Ok(bindings, nextContext) ->
                                    Ok(tests @ (bindings |> List.map (fun (name, expression) -> bind name expression)), nextContext)
                        | :? PersistentMap ->
                            match parsePattern pattern with
                            | Error errors -> Error errors
                            | Ok parsed ->
                                match lowerPattern patternContext parsed valueExpression with
                                | Error errors -> Error errors
                                | Ok(bindings, nextContext) ->
                                    Ok(
                                        test (mapTest valueExpression)
                                        :: (bindings |> List.map (fun (name, expression) -> bind name expression)),
                                        nextContext
                                    )
                        | :? PersistentList as list ->
                            match list.Items |> Seq.toList with
                            | head :: rest ->
                                match asSymbol head with
                                | Some symbol when symbol.Namespace.IsNone && symbol.Name = "active" ->
                                    match rest with
                                    | fnForm :: subpatterns ->
                                        match desugarForm patternContext fnForm with
                                        | Error errors -> Error errors
                                        | Ok fnExpr ->
                                            let resultTemp = patternContext.Gensym()
                                            let resultExpression = Core.make span (Call(fnExpr, [ valueExpression ]))
                                            let steps = ResizeArray<MatchStep>()
                                            steps.Add(bind resultTemp resultExpression)
                                            steps.Add(test (truthy (localExpr resultTemp)))
                                            let mutable current =
                                                { patternContext with
                                                    Locals = patternContext.Locals.Add resultTemp }
                                            let mutable error = None
                                            if subpatterns.Length = 1 then
                                                match compilePattern current (localExpr resultTemp) subpatterns.Head with
                                                | Ok(childSteps, nextContext) ->
                                                    steps.AddRange childSteps
                                                    current <- nextContext
                                                | Error errors -> error <- Some errors
                                            elif subpatterns.Length > 1 then
                                                steps.Add(test (sequentialTest (localExpr resultTemp)))
                                                steps.Add(test (countTest ">=" (localExpr resultTemp) subpatterns.Length))
                                                for index in 0 .. subpatterns.Length - 1 do
                                                    if error.IsNone then
                                                        match
                                                            compilePattern
                                                                current
                                                                (nthExpr (localExpr resultTemp) index)
                                                                subpatterns.[index]
                                                        with
                                                        | Ok(childSteps, nextContext) ->
                                                            steps.AddRange childSteps
                                                            current <- nextContext
                                                        | Error errors -> error <- Some errors
                                            match error with
                                            | Some errors -> Error errors
                                            | None -> Ok(List.ofSeq steps, current)
                                    | [] -> fail "WISP3037" "active pattern needs a function name." pattern
                                | Some symbol ->
                                    let caseName = Clr.symbolTypeName symbol
                                    if symbol.Namespace.IsNone && symbol.Name = "or" then
                                        let results = rest |> List.map (compilePattern patternContext valueExpression)
                                        match results |> List.tryPick (function Error errors -> Some errors | Ok _ -> None) with
                                        | Some errors -> Error errors
                                        | None ->
                                            let branches =
                                                results
                                                |> List.choose (function
                                                    | Ok(steps, _) ->
                                                        Some(
                                                            steps
                                                            |> List.choose (function MatchTest expression -> Some expression | MatchBind _ -> None)
                                                        )
                                                    | Error _ -> None)
                                            let hasBindings =
                                                results
                                                |> List.exists (function
                                                    | Ok(steps, _) -> steps |> List.exists (function MatchBind _ -> true | _ -> false)
                                                    | Error _ -> false)
                                            if hasBindings then
                                                fail "WISP3037" "or-patterns cannot bind names." pattern
                                            else
                                                let rec allTests tests =
                                                    match tests with
                                                    | [] -> boolConst true
                                                    | testExpression :: remaining ->
                                                        Core.make span (If(testExpression, allTests remaining, boolConst false))
                                                let mutable combined = boolConst false
                                                for tests in List.rev branches do
                                                    combined <- Core.make span (If(allTests tests, boolConst true, combined))
                                                Ok([ test combined ], patternContext)
                                    else
                                        match UnionRegistry.tryFind caseName with
                                        | None -> fail "WISP3037" (sprintf "Unknown match pattern %s." caseName) pattern
                                        | Some fieldNames ->
                                            if rest.Length > fieldNames.Length then
                                                fail
                                                    "WISP3037"
                                                    (sprintf "Union case %s has %d field(s), but the pattern has %d." caseName fieldNames.Length rest.Length)
                                                    pattern
                                            else
                                                let steps = ResizeArray<MatchStep>()
                                                steps.Add(test (Core.make (spanOf pattern) (TypeTestHint(head, valueExpression))))
                                                let mutable current = patternContext
                                                let mutable error = None
                                                for index in 0 .. rest.Length - 1 do
                                                    if error.IsNone then
                                                        let fieldTemp = current.Gensym()
                                                        let fieldExpression =
                                                            Core.make span (InstanceProperty(valueExpression, fieldNames.[index]))
                                                        steps.Add(bind fieldTemp fieldExpression)
                                                        match compilePattern current (localExpr fieldTemp) rest.[index] with
                                                        | Ok(childSteps, nextContext) ->
                                                            steps.AddRange childSteps
                                                            current <- nextContext
                                                        | Error errors -> error <- Some errors
                                                match error with
                                                | Some errors -> Error errors
                                                | None -> Ok(List.ofSeq steps, current)
                                | None -> fail "WISP3037" "A list match pattern must start with a symbol." pattern
                            | [] -> fail "WISP3037" "An empty list is not a match pattern." pattern
                        | _ -> Ok([ test (equalityTest valueExpression pattern) ], patternContext)

                    let clauses = ResizeArray<MatchStep list * CoreExpr option * CoreExpr>()
                    let mutable error = None

                    for clauseForm in clauseForms do
                        if error.IsNone then
                            match asList clauseForm with
                            | Some(patternForm :: rest) ->
                                let guardForm, bodyForms =
                                    match rest with
                                    | (:? Keyword as keyword) :: guardForm :: bodyForms when keyword.Name = "when" ->
                                        Some guardForm, bodyForms
                                    | _ -> None, rest
                                if bodyForms.IsEmpty then
                                    error <- Some(fail "WISP3038" "A match clause needs a body." clauseForm)
                                else
                                    match compilePattern context valueLocal patternForm with
                                    | Error errors -> error <- Some(Error errors)
                                    | Ok(steps, patternContext) ->
                                        let guardResult =
                                            match guardForm with
                                            | None -> Ok None
                                            | Some guardForm ->
                                                match desugarForm patternContext guardForm with
                                                | Ok guardExpr -> Ok(Some guardExpr)
                                                | Error errors -> Error errors
                                        match guardResult with
                                        | Error errors -> error <- Some(Error errors)
                                        | Ok guardExpr ->
                                            match desugarBody patternContext bodyForms with
                                            | Error errors -> error <- Some(Error errors)
                                            | Ok body -> clauses.Add(steps, guardExpr, body)
                            | _ -> error <- Some(fail "WISP3038" "A match clause must be (pattern body...)." clauseForm)

                    let checkExhaustiveness () =
                        let mutable unionCases = Set.empty
                        let mutable unionDomain = Set.empty
                        let mutable booleans = Set.empty
                        let mutable booleanDomain = Set.empty
                        let coveredTypes = ResizeArray<Type>()
                        let mutable catchAll = false

                        let rec collect covers (pattern: obj) =
                            match pattern with
                            | :? Symbol as symbol ->
                                if symbol.Name = "_" then
                                    if covers then catchAll <- true
                                else
                                    let hint =
                                        symbol.Meta
                                        |> Option.bind (fun metadata -> metadata.TryGet typeHintTag)
                                    match hint with
                                    | Some hint ->
                                        match resolveHintNow context hint with
                                        | Ok(Some clrType) when clrType = typeof<bool> ->
                                            booleanDomain <- booleanDomain.Add(true).Add(false)
                                            if covers then booleans <- booleans.Add(true).Add(false)
                                        | Ok(Some clrType) when clrType = typeof<obj> ->
                                            if covers then catchAll <- true
                                        | Ok(Some clrType) ->
                                            if covers && not (coveredTypes.Contains clrType) then
                                                coveredTypes.Add clrType
                                        | _ -> ()
                                    | None -> if covers then catchAll <- true
                            | :? bool as value ->
                                booleanDomain <- booleanDomain.Add value
                                if covers then booleans <- booleans.Add value
                            | :? PersistentList as list ->
                                match list.Items |> Seq.toList with
                                | head :: rest ->
                                    match asSymbol head with
                                    | Some symbol when symbol.Namespace.IsNone && symbol.Name = "or" ->
                                        rest |> List.iter (collect covers)
                                    | Some symbol ->
                                        let caseName = Clr.symbolTypeName symbol
                                        match UnionRegistry.tryFind caseName with
                                        | Some _ ->
                                            unionDomain <- unionDomain.Add caseName
                                            if covers then unionCases <- unionCases.Add caseName
                                        | None -> ()
                                    | None -> ()
                                | [] -> ()
                            | _ -> ()

                        for clauseForm in clauseForms do
                            match asList clauseForm with
                            | Some(patternForm :: rest) ->
                                let guarded =
                                    match rest with
                                    | (:? Keyword as keyword) :: _ when keyword.Name = "when" -> true
                                    | _ -> false
                                collect (not guarded) patternForm
                            | _ -> ()

                        if catchAll then
                            Ok()
                        else
                            let errors = ResizeArray<CompilerError>()
                            let unions =
                                unionDomain
                                |> Seq.choose UnionRegistry.unionOf
                                |> Set.ofSeq
                            match Set.toList unions with
                            | [ unionName ] ->
                                let allCases =
                                    UnionRegistry.casesOf unionName
                                    |> Option.defaultValue []
                                let missing =
                                    allCases |> List.filter (fun caseName -> not (unionCases.Contains caseName))
                                let coveredByType =
                                    match context.ResolveTypeName unionName with
                                    | Some unionType ->
                                        coveredTypes
                                        |> Seq.exists (fun clrType ->
                                            clrType = unionType || clrType.IsAssignableFrom unionType)
                                    | None -> false
                                if not coveredByType && not missing.IsEmpty then
                                    errors.Add(
                                        CompilerError.create
                                            "WISP3044"
                                            (sprintf "match is not exhaustive for union %s." unionName)
                                            (spanOf form)
                                        |> CompilerError.withNote (
                                            "missing cases: " + String.Join(", ", missing)
                                        )
                                    )
                            | _ -> ()

                            if not booleanDomain.IsEmpty then
                                let missing =
                                    [ false; true ] |> List.filter (fun value -> not (booleans.Contains value))
                                if not missing.IsEmpty then
                                    errors.Add(
                                        CompilerError.create
                                            "WISP3044"
                                            "match is not exhaustive for Boolean."
                                            (spanOf form)
                                        |> CompilerError.withNote (
                                            "missing values: "
                                            + String.Join(
                                                ", ",
                                                missing
                                                |> List.map (fun value -> if value then "true" else "false")
                                            )
                                        )
                                    )

                            if errors.Count = 0 then Ok() else Error(List.ofSeq errors)

                    match error with
                    | Some errors -> errors
                    | None ->
                        match checkExhaustiveness () with
                        | Error errors -> Error errors
                        | Ok () ->
                        let rec buildClauses index =
                            if index >= clauses.Count then
                                failExpr
                            else
                                let steps, guardExpr, body = clauses.[index]
                                let otherwise = buildClauses (index + 1)
                                let guarded =
                                    match guardExpr with
                                    | Some guard -> Core.make span (If(guard, body, otherwise))
                                    | None -> body
                                let rec buildSteps remaining =
                                    match remaining with
                                    | [] -> guarded
                                    | MatchTest test :: rest -> Core.make span (If(test, buildSteps rest, otherwise))
                                    | MatchBind(name, expression) :: rest ->
                                        Core.make span (Let(name, expression, buildSteps rest))
                                buildSteps steps

                        Ok(Core.make span (Let(valueTemp, valueExpr, buildClauses 0)))
            | [] -> fail "WISP3038" "match expects a value and clauses." form
        | "defmacro" ->
            match arguments with
            | [] -> fail "WISP3021" "defmacro expects a name." form
            | nameForm :: rest ->
                match asSymbol nameForm with
                | None -> fail "WISP3021" "defmacro expects a symbol name." nameForm
                | Some symbol ->
                    let doc, rest = splitDocstring rest
                    let symbol = attachDoc symbol doc
                    match rest with
                    | parametersForm :: bodyForms ->
                        match parametersForm with
                        | :? PersistentVector as parameters ->
                            match buildFunction context (Some symbol.Name) parameters bodyForms (spanOf form) with
                            | Error errors -> Error errors
                            | Ok(slots, restSlot, body) ->
                                let macroMetadata = PersistentMap.Empty.Assoc(Keyword.Intern "macro", box true)
                                let macroSymbol = symbol.WithMeta(Meta.merge symbol.Meta (Some macroMetadata))
                                let functionExpr =
                                    make form (Lambda(Some symbol.Name, slots, restSlot, body))
                                Ok(make form (Def(macroSymbol, functionExpr)))
                        | _ -> fail "WISP3022" "defmacro parameters must be a vector." parametersForm
                    | [] -> fail "WISP3022" "defmacro needs a parameter vector." form
        | "fn"
        | "fn*" ->
            let functionName, rest =
                match arguments with
                | nameForm :: remaining ->
                    match asSymbol nameForm with
                    | Some symbol when remaining.Length > 0 -> Some symbol, remaining
                    | _ -> None, arguments
                | [] -> None, []
            match rest with
            | parametersForm :: bodyForms ->
                match parametersForm with
                | :? PersistentVector as parameters ->
                    match
                        buildFunction
                            context
                            (functionName |> Option.map (fun symbol -> symbol.Name))
                            parameters
                            bodyForms
                            (spanOf form)
                    with
                    | Error errors -> Error errors
                    | Ok(slots, restSlot, body) ->
                        Ok(make form (Lambda(functionName |> Option.map (fun symbol -> symbol.Name), slots, restSlot, body)))
                | _ -> fail "WISP3009" "fn parameters must be a vector." parametersForm
            | [] -> fail "WISP3009" "fn needs a parameter vector." form
        | "let"
        | "let*" ->
            match arguments with
            | bindingsForm :: bodyForms ->
                match bindingsForm with
                | :? PersistentVector as bindings ->
                    let items = bindings.Elements |> Seq.toList
                    if items.Length % 2 <> 0 then
                        fail "WISP3010" "let bindings must contain an even number of forms." bindingsForm
                    else
                        let rec buildBindings currentContext (accumulated: ResizeArray<string * CoreExpr>) remaining =
                            match remaining with
                            | [] ->
                                match desugarBody currentContext bodyForms with
                                | Error errors -> Error errors
                                | Ok body ->
                                    Ok(wrapBindings (spanOf form) (List.ofSeq accumulated) body)
                            | patternForm :: bindingValue :: rest ->
                                match parsePattern patternForm with
                                | Error errors -> Error errors
                                | Ok pattern ->
                                    match desugarForm currentContext bindingValue with
                                    | Error errors -> Error errors
                                    | Ok value ->
                                        match lowerPattern currentContext pattern value with
                                        | Ok(patternBindings, nextContext) ->
                                            accumulated.AddRange patternBindings
                                            buildBindings nextContext accumulated rest
                                        | Error errors -> Error errors
                            | _ -> fail "WISP3010" "let bindings must contain pairs." bindingsForm
                        buildBindings context (ResizeArray<string * CoreExpr>()) items
                | _ -> fail "WISP3010" "let bindings must be a vector." bindingsForm
            | [] -> fail "WISP3010" "let needs a binding vector." form
        | "loop" ->
            match arguments with
            | bindingsForm :: bodyForms ->
                match bindingsForm with
                | :? PersistentVector as bindings ->
                    let items = bindings.Elements |> Seq.toList
                    if items.Length % 2 <> 0 then
                        fail "WISP3025" "loop bindings must contain an even number of forms." bindingsForm
                    else
                        let rec buildLoop
                            currentContext
                            (setupBindings: ResizeArray<string * CoreExpr>)
                            (bodyBindings: ResizeArray<string * CoreExpr>)
                            (loopSlots: ResizeArray<string * CoreExpr>)
                            remaining
                            =
                            match remaining with
                            | [] ->
                                let bodyContext =
                                    { currentContext with
                                        RecurTarget =
                                            Some
                                                { Arity = loopSlots.Count
                                                  HasRest = false } }
                                match desugarBody bodyContext bodyForms with
                                | Error errors -> Error errors
                                | Ok body ->
                                    let body =
                                        wrapBindings (spanOf form) (List.ofSeq bodyBindings) body
                                    let loopExpression =
                                        make form (Loop(List.ofSeq loopSlots, body))
                                    Ok(wrapBindings (spanOf form) (List.ofSeq setupBindings) loopExpression)
                            | patternForm :: bindingValue :: rest ->
                                match parsePattern patternForm with
                                | Error errors -> Error errors
                                | Ok pattern ->
                                    match desugarForm currentContext bindingValue with
                                    | Error errors -> Error errors
                                    | Ok value ->
                                        let slot = currentContext.Gensym()
                                        setupBindings.Add((slot, value))
                                        match lowerPattern currentContext pattern (localExpr slot) with
                                        | Ok(patternBindings, nextContext) ->
                                            setupBindings.AddRange patternBindings
                                            bodyBindings.AddRange patternBindings
                                            loopSlots.Add((slot, localExpr slot))
                                            buildLoop nextContext setupBindings bodyBindings loopSlots rest
                                        | Error errors -> Error errors
                            | _ -> fail "WISP3025" "loop bindings must contain pairs." bindingsForm
                        buildLoop
                            context
                            (ResizeArray<string * CoreExpr>())
                            (ResizeArray<string * CoreExpr>())
                            (ResizeArray<string * CoreExpr>())
                            items
                | _ -> fail "WISP3025" "loop bindings must be a vector." bindingsForm
            | [] -> fail "WISP3025" "loop needs a binding vector." form
        | "recur" ->
            match context.RecurTarget with
            | None -> fail "WISP3026" "recur requires an enclosing loop or function." form
            | Some target ->
                let expected = target.Arity + (if target.HasRest then 1 else 0)
                if arguments.Length <> expected then
                    fail
                        "WISP3026"
                        (sprintf "recur expects %d arguments for this target, got %d." expected arguments.Length)
                        form
                else
                    match desugarArguments context arguments with
                    | Ok argumentExprs -> Ok(make form (Recur argumentExprs))
                    | Error errors -> Error errors
        | "try" ->
            let isClause name (candidate: obj) =
                match asList candidate with
                | Some(head :: _) ->
                    match asSymbol head with
                    | Some symbol -> symbol.Namespace.IsNone && symbol.Name = name
                    | None -> false
                | _ -> false

            let rec splitBody acc remaining =
                match remaining with
                | [] -> Ok(List.rev acc, [])
                | clause :: rest when isClause "catch" clause || isClause "finally" clause ->
                    Ok(List.rev acc, clause :: rest)
                | bodyForm :: rest -> splitBody (bodyForm :: acc) rest

            // A single-symbol body is a valid untyped catch body, so only
            // qualified or PascalCase symbols are treated as CLR type names.
            let isTypeForm (candidate: obj) =
                match candidate with
                | :? string
                | :? PersistentVector -> true
                | :? Symbol as symbol ->
                    symbol.Namespace.IsSome
                    || (symbol.Name.Length > 0 && Char.IsUpper symbol.Name.[0])
                | _ -> false

            let catchTypeError form message =
                [ CompilerError.create "WISP3030" message (spanOf form) ]

            match splitBody [] arguments with
            | Error errors -> Error errors
            | Ok(bodyForms, clauseForms) ->
                match desugarBody { context with RecurTarget = None } bodyForms with
                | Error errors -> Error errors
                | Ok tryBody ->
                    let catches = ResizeArray<CatchClause>()
                    let mutable finallyBody = None
                    let mutable sawFinally = false
                    let mutable sawUntypedCatch = false
                    let mutable clauseError = None

                    let setError result = clauseError <- Some result

                    for clause in clauseForms do
                        if clauseError.IsNone then
                            match asList clause with
                            | Some(head :: rest) ->
                                match asSymbol head with
                                | Some symbol when symbol.Namespace.IsNone && symbol.Name = "catch" ->
                                    if sawFinally then
                                        setError (fail "WISP3027" "catch cannot follow finally." clause)
                                    elif sawUntypedCatch then
                                        setError (fail "WISP3031" "An untyped catch must be the last catch clause." clause)
                                    else
                                        let typeForm, bindingForm, catchForms =
                                            match rest with
                                            | typeForm :: bindingForm :: catchForms
                                                when isTypeForm typeForm
                                                     && (match bindingForm with
                                                         | :? Symbol -> true
                                                         | _ -> false) ->
                                                Some typeForm, Some bindingForm, catchForms
                                            | bindingForm :: catchForms -> None, Some bindingForm, catchForms
                                            | [] -> None, None, []

                                        match bindingForm with
                                        | None -> setError (fail "WISP3027" "catch expects a binding and body." clause)
                                        | Some bindingForm ->
                                            match asSymbol bindingForm with
                                            | None -> setError (fail "WISP3027" "catch expects a symbol binding." bindingForm)
                                            | Some catchSymbol ->
                                                let resolvedType =
                                                    match typeForm with
                                                    | None -> Ok None
                                                    | Some typeForm ->
                                                        match desugarType context typeForm with
                                                        | None ->
                                                            Error(
                                                                catchTypeError
                                                                    typeForm
                                                                    (sprintf "Unknown CLR exception type %s." (Print.printString typeForm))
                                                            )
                                                        | Some clrType ->
                                                            if typeof<Exception>.IsAssignableFrom clrType then
                                                                Ok(Some clrType)
                                                            else
                                                                let typeName =
                                                                    if isNull clrType.FullName then clrType.Name else clrType.FullName
                                                                Error(
                                                                    catchTypeError
                                                                        typeForm
                                                                        (sprintf "%s is not an exception type." typeName)
                                                                )

                                                match resolvedType with
                                                | Error errors -> setError (Error errors)
                                                | Ok exceptionType ->
                                                    let catchContext =
                                                        { context with
                                                            Locals = context.Locals.Add catchSymbol.Name
                                                            RecurTarget = None }
                                                    match desugarBody catchContext catchForms with
                                                    | Error errors -> setError (Error errors)
                                                    | Ok body ->
                                                        if exceptionType.IsNone then sawUntypedCatch <- true
                                                        catches.Add(
                                                            { ExceptionType = exceptionType
                                                              Binding = catchSymbol.Name
                                                              Body = body }
                                                        )
                                | Some symbol when symbol.Namespace.IsNone && symbol.Name = "finally" ->
                                    if sawFinally then
                                        setError (fail "WISP3027" "Only one finally clause is supported." clause)
                                    else
                                        match desugarBody { context with RecurTarget = None } rest with
                                        | Ok body ->
                                            sawFinally <- true
                                            finallyBody <- Some body
                                        | Error errors -> setError (Error errors)
                                | _ -> setError (fail "WISP3027" "try clauses must be catch or finally." clause)
                            | _ -> setError (fail "WISP3027" "Invalid try clause." clause)

                    match clauseError with
                    | Some result -> result
                    | None ->
                        if catches.Count = 0 && finallyBody.IsNone then
                            Ok tryBody
                        else
                            Ok(make form (Try(tryBody, List.ofSeq catches, finallyBody)))
        | "ns" ->
            let nameOf (form: obj) =
                match form with
                | :? Symbol as symbol -> Some(Clr.symbolTypeName symbol)
                | :? string as text -> Some text
                | :? Keyword as keyword ->
                    Some(
                        match keyword.Namespace with
                        | Some namespaceName -> namespaceName + "." + keyword.Name
                        | None -> keyword.Name
                    )
                | _ -> None

            let lastSegment (name: string) =
                let index = name.LastIndexOf '.'
                if index >= 0 && index < name.Length - 1 then name.Substring(index + 1) else name

            match arguments with
            | nameForm :: rest ->
                let doc, clauses = splitDocstring rest
                match nameOf nameForm with
                | None -> fail "WISP3012" "ns expects a namespace symbol or string." nameForm
                | Some namespaceName ->
                    let directives = ResizeArray<CoreExpr>()
                    let mutable error = None

                    let addImport alias fullName =
                        directives.Add(make form (ImportType(alias, fullName)))

                    for clause in clauses do
                        if error.IsNone then
                            match asList clause with
                            | Some((:? Keyword as keyword) :: specs) when keyword.Namespace.IsNone && keyword.Name = "import" ->
                                for spec in specs do
                                    match spec with
                                    | :? Symbol
                                    | :? string ->
                                        match nameOf spec with
                                        | Some fullName -> addImport (lastSegment fullName) fullName
                                        | None -> error <- Some(fail "WISP3012" "Invalid import." spec)
                                    | :? PersistentVector as vector ->
                                        match vector.Elements |> Seq.toList with
                                        | namespaceForm :: types ->
                                            match nameOf namespaceForm with
                                            | Some namespacePart ->
                                                for typeForm in types do
                                                    match nameOf typeForm with
                                                    | Some typeName -> addImport (lastSegment typeName) (namespacePart + "." + typeName)
                                                    | None -> error <- Some(fail "WISP3012" "Invalid imported type name." typeForm)
                                            | None -> error <- Some(fail "WISP3012" "Invalid import namespace." namespaceForm)
                                        | [] -> error <- Some(fail "WISP3012" "An import vector cannot be empty." spec)
                                    | _ -> error <- Some(fail "WISP3012" "Invalid import specification." spec)
                            | Some((:? Keyword as keyword) :: specs) when keyword.Namespace.IsNone && keyword.Name = "require" ->
                                for spec in specs do
                                    let addRequire target alias referNames useAll =
                                        directives.Add(make form (RequireNamespace(alias, target)))
                                        if useAll then directives.Add(make form (UseNamespace target))
                                        for name in referNames do
                                            directives.Add(make form (ReferVar(name, target, name)))

                                    match spec with
                                    | :? Symbol
                                    | :? string ->
                                        match nameOf spec with
                                        | Some target -> addRequire target (lastSegment target) [] false
                                        | None -> error <- Some(fail "WISP3012" "Invalid require specification." spec)
                                    | :? PersistentVector as vector ->
                                        match vector.Elements |> Seq.toList with
                                        | targetForm :: rest ->
                                            match nameOf targetForm with
                                            | Some target ->
                                                let mutable alias = lastSegment target
                                                let referNames = ResizeArray<string>()
                                                let mutable useAll = false
                                                let mutable index = 0
                                                while index < rest.Length do
                                                    match rest.[index] with
                                                    | :? Keyword as optionKeyword when optionKeyword.Namespace.IsNone ->
                                                        match optionKeyword.Name with
                                                        | "as" when index + 1 < rest.Length ->
                                                            alias <- nameOf rest.[index + 1] |> Option.defaultValue alias
                                                            index <- index + 2
                                                        | "refer" when index + 1 < rest.Length ->
                                                            match rest.[index + 1] with
                                                            | :? PersistentVector as names ->
                                                                for nameForm in names.Elements do
                                                                    match nameOf nameForm with
                                                                    | Some name -> referNames.Add name
                                                                    | None -> error <- Some(fail "WISP3012" "Invalid :refer name." nameForm)
                                                            | :? Keyword as referKeyword when referKeyword.Name = "all" -> useAll <- true
                                                            | _ -> error <- Some(fail "WISP3012" ":refer expects a vector or :all." rest.[index + 1])
                                                            index <- index + 2
                                                        | _ ->
                                                            error <- Some(fail "WISP3012" "Unknown require option." rest.[index])
                                                            index <- rest.Length
                                                    | _ ->
                                                        error <- Some(fail "WISP3012" "Unknown require option." rest.[index])
                                                        index <- rest.Length
                                                if error.IsNone then
                                                    addRequire target alias (List.ofSeq referNames) useAll
                                            | None -> error <- Some(fail "WISP3012" "Invalid required namespace." targetForm)
                                        | [] -> error <- Some(fail "WISP3012" "A require vector cannot be empty." spec)
                                    | _ -> error <- Some(fail "WISP3012" "Invalid require specification." spec)
                            | Some((:? Keyword as keyword) :: specs) when keyword.Namespace.IsNone && keyword.Name = "use" ->
                                for spec in specs do
                                    match spec with
                                    | :? Symbol
                                    | :? string ->
                                        match nameOf spec with
                                        | Some target -> directives.Add(make form (UseNamespace target))
                                        | None -> error <- Some(fail "WISP3012" "Invalid use specification." spec)
                                    | :? PersistentVector as vector ->
                                        match vector.Elements |> Seq.toList with
                                        | targetForm :: rest ->
                                            match nameOf targetForm with
                                            | Some target ->
                                                match rest with
                                                | (:? Keyword as onlyKeyword) :: namesForm :: _
                                                    when onlyKeyword.Namespace.IsNone && onlyKeyword.Name = "only" ->
                                                    match namesForm with
                                                    | :? PersistentVector as names ->
                                                        let selected =
                                                            names.Elements
                                                            |> Seq.choose nameOf
                                                            |> Seq.toList
                                                        directives.Add(make form (UseNamespaceOnly(target, selected)))
                                                    | _ -> error <- Some(fail "WISP3012" ":only expects a vector of names." namesForm)
                                                | _ -> directives.Add(make form (UseNamespace target))
                                            | None -> error <- Some(fail "WISP3012" "Invalid used namespace." targetForm)
                                        | [] -> error <- Some(fail "WISP3012" "A use vector cannot be empty." spec)
                                    | _ -> error <- Some(fail "WISP3012" "Invalid use specification." spec)
                            | _ -> error <- Some(fail "WISP3012" "ns clauses must be (:import ...), (:require ...), or (:use ...)." clause)

                    match error with
                    | Some result -> result
                    | None ->
                        let nodes = [ make form (SetNamespace(namespaceName, doc)) ] @ List.ofSeq directives
                        Ok(make form (Do nodes))
            | [] -> fail "WISP3012" "ns expects a namespace name." form
        | "new" ->
            match arguments with
            | typeForm :: constructorArguments ->
                match desugarTypeResult context typeForm with
                | Error hintErrors when hasUnknownType hintErrors && (typeForm :? PersistentVector) ->
                    match desugarArguments context constructorArguments with
                    | Ok arguments -> Ok(make form (ConstructHint(typeForm, arguments)))
                    | Error errors -> Error errors
                | Error hintErrors ->
                    let notes =
                        hintErrors |> List.map (fun error -> sprintf "%s: %s" error.Path error.Message)
                    Error
                        [ { CompilerError.create
                                "WISP3013"
                                (sprintf "Cannot resolve CLR type %s." (Print.printString typeForm))
                                (spanOf typeForm) with
                              Notes = notes } ]
                | Ok None -> fail "WISP3013" (sprintf "Unknown CLR type %s." (Print.printString typeForm)) typeForm
                | Ok(Some clrType) ->
                    match desugarArguments context constructorArguments with
                    | Ok arguments -> Ok(make form (Construct(clrType, arguments)))
                    | Error errors -> Error errors
            | [] -> fail "WISP3013" "new expects a type and constructor arguments." form
        | "when" ->
            match arguments with
            | condition :: bodyForms ->
                match desugarForm context condition with
                | Error errors -> Error errors
                | Ok conditionExpr ->
                    match desugarBody context bodyForms with
                    | Error errors -> Error errors
                    | Ok body -> Ok(make form (If(conditionExpr, body, Core.nil (spanOf form))))
            | [] -> fail "WISP3014" "when expects a condition." form
        | "cond" ->
            let rec buildClauses clauses =
                match clauses with
                | [] -> Ok(Core.nil (spanOf form))
                | testForm :: resultForm :: rest ->
                    match desugarForm context testForm, desugarForm context resultForm with
                    | Ok testExpr, Ok resultExpr ->
                        match buildClauses rest with
                        | Ok otherwise -> Ok(make form (If(testExpr, resultExpr, otherwise)))
                        | Error errors -> Error errors
                    | Error errors, _ -> Error errors
                    | _, Error errors -> Error errors
                | _ -> fail "WISP3015" "cond expects an even number of forms." form
            buildClauses arguments
        | "comment" -> Ok(Core.nil (spanOf form))
        | "throw" ->
            match arguments with
            | [ value ] ->
                match desugarForm context value with
                | Ok valueExpr -> Ok(make form (Throw valueExpr))
                | Error errors -> Error errors
            | _ -> fail "WISP3016" "throw expects one form." form
        | "and" ->
            let rec buildAnd remaining =
                match remaining with
                | [] -> Ok(make form (Const(box true)))
                | [ value ] -> desugarForm context value
                | value :: rest ->
                    match desugarForm context value with
                    | Error errors -> Error errors
                    | Ok valueExpr ->
                        let name = context.Gensym()
                        match buildAnd rest with
                        | Error errors -> Error errors
                        | Ok restExpr ->
                            Ok(
                                make form (
                                    Let(
                                        name,
                                        valueExpr,
                                        make form (If(make form (Local name), restExpr, make form (Local name)))
                                    )
                                )
                            )
            buildAnd arguments
        | "or" ->
            let rec buildOr remaining =
                match remaining with
                | [] -> Ok(Core.nil (spanOf form))
                | [ value ] -> desugarForm context value
                | value :: rest ->
                    match desugarForm context value with
                    | Error errors -> Error errors
                    | Ok valueExpr ->
                        let name = context.Gensym()
                        match buildOr rest with
                        | Error errors -> Error errors
                        | Ok restExpr ->
                            Ok(
                                make form (
                                    Let(
                                        name,
                                        valueExpr,
                                        make form (If(make form (Local name), make form (Local name), restExpr))
                                    )
                                )
                            )
            buildOr arguments
        | "async" ->
            let asyncReturnKind =
                match asSymbol items.Head with
                | Some symbol ->
                    let hint =
                        symbol.Meta
                        |> Option.bind (fun metadata -> metadata.TryGet typeHintTag)
                    match hint with
                    | None -> Ok AsyncTask
                    | Some hint ->
                        match resolveHintNow context hint with
                        | Ok(Some clrType) when clrType = typeof<System.Threading.Tasks.ValueTask> ->
                            Ok AsyncValueTask
                        | Ok(Some clrType) when clrType = typedefof<System.Threading.Tasks.ValueTask<_>> ->
                            Ok AsyncValueTask
                        | Ok(Some clrType) when clrType.IsGenericType
                                                 && clrType.GetGenericTypeDefinition() = typedefof<System.Threading.Tasks.ValueTask<_>> ->
                            let arguments = clrType.GetGenericArguments()
                            if arguments.Length = 1 && not arguments.[0].IsGenericParameter then
                                Ok(AsyncValueTaskOf arguments.[0])
                            else
                                Error
                                    [ CompilerError.create
                                          "WISP3043"
                                          "ValueTask needs a concrete result type: use ^[ValueTask T]."
                                          (spanOf items.Head) ]
                        | Ok(Some _) ->
                            Error
                                [ CompilerError.create
                                      "WISP3043"
                                      "async return type must be ValueTask or [ValueTask T]."
                                      (spanOf items.Head) ]
                        | Ok None ->
                            Error
                                [ CompilerError.create
                                      "WISP3043"
                                      "Unknown async return type."
                                      (spanOf items.Head) ]
                        | Error errors ->
                            let notes =
                                errors |> List.map (fun error -> sprintf "%s: %s" error.Path error.Message)
                            Error
                                [ { CompilerError.create
                                        "WISP3043"
                                        "Invalid async return type."
                                        (spanOf items.Head) with
                                      Notes = notes } ]
                | None -> Ok AsyncTask
            match asyncReturnKind with
            | Error errors -> Error errors
            | Ok returnKind ->
                match desugarBody context arguments with
                | Ok body -> Ok(make form (Async(returnKind, body)))
                | Error errors -> Error errors
        | "await" ->
            match arguments with
            | [ value ] ->
                match desugarForm context value with
                | Ok valueExpr -> Ok(make form (Await valueExpr))
                | Error errors -> Error errors
            | _ -> fail "WISP3042" "await expects one form." form
        | "doc"
        | "meta" ->
            match arguments with
            | [ (:? Symbol as symbol) ] when not (context.Locals.Contains symbol.Name) ->
                Ok(make form (Call(builtinRef name, [ make symbol (Quote symbol) ])))
            | _ -> desugarCall context form items
        | _ -> fail "WISP3017" (sprintf "Unknown special form %s." name) form

    and private desugarCall (context: DesugarContext) (form: obj) (items: obj list) =
        let head = items.Head
        let arguments = items.Tail
        match asSymbol head with
        | Some symbol ->
            match unqualifiedName symbol with
            | Some name when name.StartsWith(".-", StringComparison.Ordinal) ->
                match arguments with
                | [ target ] ->
                    match desugarForm context target with
                    | Ok targetExpr -> Ok(make form (InstanceProperty(targetExpr, name.Substring 2)))
                    | Error errors -> Error errors
                | _ -> fail "WISP3018" "Property access expects exactly one target." form
            | Some name when name.StartsWith(".", StringComparison.Ordinal) && name.Length > 1 ->
                match arguments with
                | target :: methodArguments ->
                    match desugarForm context target, desugarArguments context methodArguments with
                    | Ok targetExpr, Ok argumentExprs -> Ok(make form (InstanceCall(targetExpr, name.Substring 1, argumentExprs)))
                    | Error errors, _ -> Error errors
                    | _, Error errors -> Error errors
                | [] -> fail "WISP3019" "Instance method call expects a target." form
            | Some name when name.EndsWith(".", StringComparison.Ordinal) && name.Length > 1 ->
                let typeName = name.Substring(0, name.Length - 1)
                match context.ResolveTypeName typeName with
                | None -> fail "WISP3020" (sprintf "Unknown CLR type %s." typeName) head
                | Some clrType ->
                    match desugarArguments context arguments with
                    | Ok argumentExprs -> Ok(make form (Construct(clrType, argumentExprs)))
                    | Error errors -> Error errors
            | _ ->
                match symbol.Namespace with
                | Some namespaceName ->
                    match context.ResolveTypeName namespaceName with
                    | Some clrType ->
                        match desugarArguments context arguments with
                        | Ok argumentExprs -> Ok(make form (StaticCall(clrType, symbol.Name, argumentExprs)))
                        | Error errors -> Error errors
                    | None ->
                        match desugarArguments context arguments with
                        | Ok argumentExprs -> Ok(make form (Call(resolveSymbol context symbol, argumentExprs)))
                        | Error errors -> Error errors
                | None ->
                    match desugarArguments context arguments with
                    | Ok argumentExprs -> Ok(make form (Call(resolveSymbol context symbol, argumentExprs)))
                    | Error errors -> Error errors
        | None ->
            match desugarForm context head, desugarArguments context arguments with
            | Ok callee, Ok argumentExprs -> Ok(make form (Call(callee, argumentExprs)))
            | Error errors, _ -> Error errors
            | _, Error errors -> Error errors

    and private desugarList (context: DesugarContext) (form: obj) (items: obj list) =
        match items with
        | [] -> Ok(make form (Const(PersistentList.Empty)))
        | head :: _ ->
            match asSymbol head with
            | Some symbol ->
                match unqualifiedName symbol with
                | Some name when
                    List.contains name
                        [ "quote"; "if"; "do"; "def"; "defn"; "defactive"; "defmacro"; "deftype"; "defstruct"; "defclass"; "definterface"; "defdelegate"; "defunion"; "defmethod"; "delegate"; "set!"; "async"; "await"; "doc"; "meta"; "match"; "fn"; "fn*"; "let"; "let*"; "loop"; "recur"; "try"; "ns"; "new"; "when"; "cond"; "comment"; "throw"; "and"; "or" ] ->
                    desugarSpecial context form items name
                | _ -> desugarCall context form items
            | None -> desugarCall context form items

    and desugarForm (context: DesugarContext) (form: obj) : Result<CoreExpr, CompilerError list> =
        match form with
        | null -> Ok(make form (Const null))
        | :? Symbol as symbol -> Ok(desugarSymbol context symbol)
        | :? PersistentList as list ->
            let items = list.Items |> Seq.toList
            desugarList context form items
        | :? ISeq as sequence ->
            let items = Seqs.toList (sequence :> obj)
            desugarList context form items
        | :? PersistentVector as vector ->
            let items = vector.Elements |> Seq.toList
            match desugarArguments context items with
            | Ok expressions -> Ok(make form (Call(builtinRef "vector", expressions)))
            | Error errors -> Error errors
        | :? PersistentMap as map ->
            let entries = map.Entries |> Seq.toList
            let forms = entries |> List.collect (fun entry -> [ entry.Key; entry.Value ])
            match desugarArguments context forms with
            | Ok expressions -> Ok(make form (Call(builtinRef "hash-map", expressions)))
            | Error errors -> Error errors
        | :? PersistentSet as set ->
            let elements = set.Elements |> Seq.toList
            match desugarArguments context elements with
            | Ok expressions -> Ok(make form (Call(builtinRef "hash-set", expressions)))
            | Error errors -> Error errors
        | _ -> Ok(make form (Const form))

    let desugar (context: DesugarContext) (form: obj) = desugarForm context form
