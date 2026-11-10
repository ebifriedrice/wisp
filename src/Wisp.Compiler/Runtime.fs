namespace Wisp.Compiler

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Reflection
open Wisp.Core

type RuntimeMethod =
    { Implementation: obj
      IsPrivate: bool
      DefiningNamespace: string
      Documentation: string option }

type RuntimeConstructor =
    { Implementation: obj
      ParameterHints: Type option list
      IsPrivate: bool
      DefiningNamespace: string
      Documentation: string option }

type RuntimeState(registry: NamespaceRegistry, output: TextWriter) =
    let constants = ResizeArray<obj>()
    let generatedAssemblies = ResizeArray<Assembly>()
    let methods = ConcurrentDictionary<string, RuntimeMethod>(StringComparer.Ordinal)
    let constructors = ConcurrentDictionary<string, ResizeArray<RuntimeConstructor>>(StringComparer.Ordinal)
    let gate = obj ()
    let mutable recordTypes: RecordRegistry option = None
    let mutable interfaceTypes: InterfaceRegistry option = None
    let mutable delegateTypes: DelegateRegistry option = None

    member _.Registry = registry
    member _.Output = output
    member val CurrentNamespace = "user" with get, set
    member val Builtins: Map<string, obj> = Map.empty with get, set
    member val RenderMarkdown = false with get, set
    member val ForceCompilerAsync = false with get, set

    member _.RecordTypes =
        lock gate (fun () ->
            match recordTypes with
            | Some records -> records
            | None ->
                let records = RecordRegistry()
                recordTypes <- Some records
                generatedAssemblies.Add records.Assembly
                records)

    member _.InterfaceTypes =
        lock gate (fun () ->
            match interfaceTypes with
            | Some interfaces -> interfaces
            | None ->
                let interfaces = InterfaceRegistry()
                interfaceTypes <- Some interfaces
                generatedAssemblies.Add interfaces.Assembly
                interfaces)

    member _.DelegateTypes =
        lock gate (fun () ->
            match delegateTypes with
            | Some delegates -> delegates
            | None ->
                let delegates = DelegateRegistry()
                delegateTypes <- Some delegates
                generatedAssemblies.Add delegates.Assembly
                delegates)

    member _.RegisterMethod
        (
            typeName: string,
            methodName: string,
            implementation: obj,
            isPrivate: bool,
            definingNamespace: string,
            documentation: string option
        )
        =
        methods.[typeName + "\u0000" + methodName] <-
            { Implementation = implementation
              IsPrivate = isPrivate
              DefiningNamespace = definingNamespace
              Documentation = documentation }

    member _.TryFindMethod(typeName: string, methodName: string) =
        match methods.TryGetValue(typeName + "\u0000" + methodName) with
        | true, implementation -> Some implementation
        | false, _ -> None

    member _.RegisterConstructor(typeName: string, constructor: RuntimeConstructor) =
        let entries = constructors.GetOrAdd(typeName, fun _ -> ResizeArray<RuntimeConstructor>())
        lock entries (fun () -> entries.Add constructor)

    member _.HasConstructors(typeName: string) = constructors.ContainsKey typeName

    member _.TryFindConstructor(typeName: string, arguments: obj[], currentNamespace: string) =
        match constructors.TryGetValue typeName with
        | false, _ -> None
        | true, entries ->
            lock entries (fun () ->
                entries
                |> Seq.filter (fun constructor ->
                    not constructor.IsPrivate || constructor.DefiningNamespace = currentNamespace)
                |> Seq.tryFind (fun constructor ->
                    if constructor.ParameterHints.Length <> arguments.Length then
                        false
                    else
                        List.forall2
                            (fun hint argument ->
                                match hint with
                                | None -> true
                                | Some parameterType -> (Clr.tryConvertValue argument parameterType).IsSome)
                            constructor.ParameterHints
                            (List.ofArray arguments))
                |> Option.map id)

    member _.RegisterConstant(value: obj) =
        lock gate (fun () ->
            constants.Add value
            constants.Count - 1)

    member _.GetConstant(index: int) =
        lock gate (fun () -> constants.[index])

    member _.RegisterAssembly(assembly: Assembly) =
        lock gate (fun () -> generatedAssemblies.Add assembly)

    member _.GeneratedAssemblies = lock gate (fun () -> generatedAssemblies |> Seq.toList)

    new() = RuntimeState(NamespaceRegistry(), Console.Out)

module VarOps =
    let declare (runtime: RuntimeState) (symbol: Symbol) =
        let namespaceName =
            match symbol.Namespace with
            | Some namespaceName ->
                runtime.Registry.ResolveAlias(runtime.CurrentNamespace, namespaceName)
                |> Option.defaultValue namespaceName
            | None -> runtime.CurrentNamespace
        let namespaceValue = runtime.Registry.GetOrCreate namespaceName
        match namespaceValue.FindVar symbol.Name with
        | Some variable -> variable
        | None ->
            runtime.Registry.Intern(namespaceName, symbol.Name, null, symbol.Meta)

module NamespaceResolution =
    let resolveTypeName (runtime: RuntimeState) (name: string) =
        match runtime.Registry.TryFind runtime.CurrentNamespace with
        | Some namespaceValue ->
            match namespaceValue.TryResolveImport name with
            | Some fullName -> TypeHints.tryResolveName fullName
            | None -> TypeHints.tryResolveName name
        | None -> TypeHints.tryResolveName name

    let resolveHint (runtime: RuntimeState) (form: obj) =
        TypeHints.tryResolveFormOrNameWithResolver (resolveTypeName runtime) form

module RecordOps =
    let private docMetadata (name: string) =
        DefinitionDocs.tryFind (DefinitionDocs.typeKey name)
        |> Option.map (fun doc ->
            PersistentMap.Empty.Assoc(Keyword.Intern "doc", box doc) :> IPersistentMap)

    let defineType
        (runtime: RuntimeState)
        (name: string)
        (fields: RecordField[])
        (definition: TypeDefinition option)
        (isStruct: bool)
        : obj =
        let info =
            runtime.RecordTypes.Define(
                name,
                fields |> Array.toList,
                definition,
                isStruct,
                NamespaceResolution.resolveTypeName runtime
            )
        TypeHints.registerType name info.ClrType
        match definition with
        | Some definition -> TypeHints.registerType definition.Name info.ClrType
        | None -> ()

        let invoke (implementation: obj) (args: obj[]) =
            match implementation with
            | :? IFn as functionValue -> functionValue.Invoke args
            | _ -> raise (EvaluationException(sprintf "%s constructor is not callable." name, None))

        let buildFromResult (result: obj) =
            match result with
            | :? IPersistentMap as map -> info.ConstructFromMap map
            | instance when info.ClrType.IsInstanceOfType instance -> instance
            | _ ->
                raise (
                    EvaluationException(
                        sprintf "Constructor %s must return a field map or an instance." name,
                        None
                    )
                )

        let metadata = docMetadata name
        if info.ClrType.IsGenericTypeDefinition then
            runtime.Registry.Intern(runtime.CurrentNamespace, name, info.ClrType :> obj, metadata) |> ignore
        else
            let construct (args: obj[]) =
                if runtime.HasConstructors name then
                    match runtime.TryFindConstructor(name, args, runtime.CurrentNamespace) with
                    | Some constructor -> buildFromResult (invoke constructor.Implementation args)
                    | None ->
                        raise (
                            EvaluationException(
                                sprintf "No public constructor for %s matches %d argument(s)." name args.Length,
                                None
                            )
                        )
                else
                    match args with
                    | [| :? IPersistentMap as map |] -> info.ConstructFromMap map
                    | _ -> info.Constructor args

            let constructor = Fn construct
            let positional = Fn(fun args -> info.Constructor args)
            runtime.Registry.Intern(runtime.CurrentNamespace, name, constructor :> obj, metadata) |> ignore
            runtime.Registry.Intern(runtime.CurrentNamespace, "->" + name, positional :> obj, None) |> ignore
        info.ClrType :> obj

    let defineInterface (runtime: RuntimeState) (definition: InterfaceDefinition) : obj =
        let clrType = runtime.InterfaceTypes.Define definition
        runtime.Registry.Intern(runtime.CurrentNamespace, definition.Name, clrType :> obj, docMetadata definition.Name)
        |> ignore
        clrType :> obj

    let defineDelegate (runtime: RuntimeState) (definition: DelegateDefinition) : obj =
        let clrType = runtime.DelegateTypes.Define definition
        runtime.Registry.Intern(runtime.CurrentNamespace, definition.Name, clrType :> obj, docMetadata definition.Name)
        |> ignore
        clrType :> obj

    let defineMethod
        (runtime: RuntimeState)
        (typeName: string)
        (methodName: string)
        (implementation: obj)
        (isPrivate: bool)
        (definingNamespace: string)
        : obj =
        let documentation = DefinitionDocs.tryFind (DefinitionDocs.methodKey typeName methodName)
        runtime.RegisterMethod(typeName, methodName, implementation, isPrivate, definingNamespace, documentation)
        implementation

    let defineConstructor
        (runtime: RuntimeState)
        (typeName: string)
        (implementation: obj)
        (parameterHints: Type option list)
        (isPrivate: bool)
        (definingNamespace: string)
        : obj =
        runtime.RegisterConstructor(
            typeName,
            { Implementation = implementation
              ParameterHints = parameterHints
              IsPrivate = isPrivate
              DefiningNamespace = definingNamespace
              Documentation =
                DefinitionDocs.tryFind (DefinitionDocs.constructorKey typeName parameterHints.Length) }
        )
        implementation

    let invokeInstance (runtime: RuntimeState) (target: obj) (memberName: string) (arguments: obj[]) : obj =
        match target with
        | :? IWispRecord as record ->
            match runtime.TryFindMethod(record.TypeName, memberName) with
            | Some methodInfo ->
                if methodInfo.IsPrivate && methodInfo.DefiningNamespace <> runtime.CurrentNamespace then
                    raise (InteropException(sprintf "%s.%s is private." record.TypeName memberName))
                match methodInfo.Implementation with
                | :? IFn as functionValue -> functionValue.Invoke(Array.append [| target |] arguments)
                | _ -> raise (InteropException(sprintf "%s.%s is not callable." record.TypeName memberName))
            | None -> Clr.invokeInstance target memberName arguments
        | _ -> Clr.invokeInstance target memberName arguments
