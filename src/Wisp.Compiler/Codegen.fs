namespace Wisp.Compiler

open System
open System.Reflection
open System.Reflection.Emit
open Wisp.Core

/// Helpers called from generated IL. They keep dynamic language services in one
/// place while emitted methods supply control flow and value flow.
[<AbstractClass; Sealed>]
type RuntimeOps private () =
    static member Constant(runtime: RuntimeState, id: int) : obj = runtime.GetConstant id

    static member EmptyEnv() : obj = box (Map.empty<string, obj>)

    static member BindEnv(environment: obj, name: string, value: obj) : obj =
        box ((unbox<Env> environment).Add(name, value))

    static member LookupEnv(environment: obj, name: string) : obj =
        match (unbox<Env> environment).TryFind name with
        | Some value -> value
        | None -> raise (EvaluationException(sprintf "Local %s is not bound." name, None))

    static member IsTruthy(value: obj) : bool = Truth.isTruthy value

    static member BindClosure
        (
            captured: obj,
            parameterNames: string[],
            restName: string,
            arguments: obj[],
            selfName: string,
            selfValue: obj,
            initial: bool
        ) : obj =
        let required = parameterNames.Length
        let mutable environment = unbox<Env> captured
        if not (isNull selfName) then
            environment <- environment.Add(selfName, selfValue)

        if initial then
            if arguments.Length < required || (isNull restName && arguments.Length <> required) then
                let expected = if isNull restName then string required else sprintf "at least %d" required
                raise (ArityException(expected, arguments.Length))
            for index in 0 .. required - 1 do
                environment <- environment.Add(parameterNames.[index], arguments.[index])
            if not (isNull restName) then
                let restValue =
                    if arguments.Length > required then
                        PersistentList.OfSeq(arguments.[required..]) :> obj
                    else
                        null
                environment <- environment.Add(restName, restValue)
        else
            let expected = required + (if isNull restName then 0 else 1)
            if arguments.Length <> expected then
                raise (EvaluationException(sprintf "recur expected %d arguments, got %d." expected arguments.Length, None))
            for index in 0 .. required - 1 do
                environment <- environment.Add(parameterNames.[index], arguments.[index])
            if not (isNull restName) then
                environment <- environment.Add(restName, arguments.[required])

        environment :> obj

    static member Invoke(callee: obj, arguments: obj[]) : obj =
        match callee with
        | :? Delegate as delegateValue -> DelegateOps.Invoke(delegateValue, arguments)
        | :? IFn as functionValue -> functionValue.Invoke arguments
        | _ -> raise (EvaluationException(sprintf "%s is not callable." (Print.printString callee), None))

    static member ResolveVar(runtime: RuntimeState, symbolId: int, currentNamespace: string) : obj =
        let symbol = runtime.GetConstant symbolId :?> Symbol
        Eval.resolveVar runtime symbol currentNamespace

    static member Def(runtime: RuntimeState, symbolId: int, value: obj) : obj =
        let symbol = runtime.GetConstant symbolId :?> Symbol
        let namespaceName =
            match symbol.Namespace with
            | Some namespaceName ->
                runtime.Registry.ResolveAlias(runtime.CurrentNamespace, namespaceName)
                |> Option.defaultValue namespaceName
            | None -> runtime.CurrentNamespace
        let variable = runtime.Registry.Intern(namespaceName, symbol.Name, value, symbol.Meta)
        variable :> obj

    static member SetNamespace(runtime: RuntimeState, namespaceName: string, doc: string) : obj =
        if String.IsNullOrWhiteSpace namespaceName then
            raise (EvaluationException("Namespace name cannot be empty.", None))
        runtime.CurrentNamespace <- namespaceName
        let namespaceValue = runtime.Registry.GetOrCreate namespaceName
        namespaceValue.Meta <-
            doc
            |> Option.ofObj
            |> Option.map (fun text ->
                PersistentMap.Empty.Assoc(Keyword.Intern "doc", box text) :> IPersistentMap)
        null

    static member ImportType(runtime: RuntimeState, alias: string, fullName: string) : obj =
        let namespaceValue = runtime.Registry.GetOrCreate runtime.CurrentNamespace
        namespaceValue.AddImport(alias, fullName)
        null

    static member RequireNamespace(runtime: RuntimeState, alias: string, namespaceName: string) : obj =
        let namespaceValue = runtime.Registry.GetOrCreate runtime.CurrentNamespace
        namespaceValue.AddAlias(alias, namespaceName)
        null

    static member ReferVar(runtime: RuntimeState, localName: string, namespaceName: string, varName: string) : obj =
        let namespaceValue = runtime.Registry.GetOrCreate runtime.CurrentNamespace
        namespaceValue.AddReferral(localName, namespaceName, varName)
        null

    static member UseNamespace(runtime: RuntimeState, namespaceName: string) : obj =
        let target = runtime.Registry.GetOrCreate namespaceName
        let current = runtime.Registry.GetOrCreate runtime.CurrentNamespace
        let privateKey = Keyword.Intern "private"
        for variable in target.Vars do
            let isPrivate =
                variable.Meta
                |> Option.exists (fun metadata ->
                    match metadata.TryGet privateKey with
                    | Some value -> Object.Equals(value, box true)
                    | None -> false)
            if not isPrivate then
                current.AddReferral(variable.Name, namespaceName, variable.Name)
        null

    static member UseNamespaceOnly(runtime: RuntimeState, namespaceName: string, names: string[]) : obj =
        let current = runtime.Registry.GetOrCreate runtime.CurrentNamespace
        for name in names do
            current.AddReferral(name, namespaceName, name)
        null

    static member StaticCall(runtime: RuntimeState, typeId: int, memberName: string, arguments: obj[]) : obj =
        let clrType = runtime.GetConstant typeId :?> Type
        try Clr.invokeStatic clrType memberName arguments
        with InteropException message -> raise (EvaluationException(message, None))

    static member InstanceCall(runtime: RuntimeState, target: obj, memberName: string, arguments: obj[]) : obj =
        try RecordOps.invokeInstance runtime target memberName arguments
        with InteropException message -> raise (EvaluationException(message, None))

    static member InstanceProperty(target: obj, memberName: string) : obj =
        try Clr.getInstanceProperty target memberName
        with InteropException message -> raise (EvaluationException(message, None))

    static member Construct(runtime: RuntimeState, typeId: int, arguments: obj[]) : obj =
        let clrType = runtime.GetConstant typeId :?> Type
        try Clr.construct clrType arguments
        with InteropException message -> raise (EvaluationException(message, None))

    static member ConstructHint(runtime: RuntimeState, hintId: int, arguments: obj[]) : obj =
        let hintForm = runtime.GetConstant hintId
        match NamespaceResolution.resolveHint runtime hintForm with
        | Ok(Some clrType) ->
            try Clr.construct clrType arguments
            with InteropException message -> raise (EvaluationException(message, None))
        | Ok None ->
            raise (EvaluationException(sprintf "Cannot resolve CLR type %s." (Print.printString hintForm), None))
        | Error errors ->
            let notes =
                errors |> List.map (fun error -> sprintf "%s: %s" error.Path error.Message) |> String.concat "; "
            raise (
                EvaluationException(
                    sprintf "Cannot resolve CLR type %s: %s" (Print.printString hintForm) notes,
                    None
                )
            )

    static member TypeTestHint(runtime: RuntimeState, hintId: int, value: obj) : obj =
        let hintForm = runtime.GetConstant hintId
        match NamespaceResolution.resolveHint runtime hintForm with
        | Ok(Some clrType) -> box (clrType.IsInstanceOfType value)
        | Ok None ->
            raise (EvaluationException(sprintf "Cannot resolve type %s." (Print.printString hintForm), None))
        | Error errors ->
            let notes =
                errors |> List.map (fun error -> sprintf "%s: %s" error.Path error.Message) |> String.concat "; "
            raise (
                EvaluationException(
                    sprintf "Cannot resolve type %s: %s" (Print.printString hintForm) notes,
                    None
                )
            )

    static member DefineType
        (runtime: RuntimeState, name: string, fieldsId: int, definitionId: int, isStruct: bool)
        :  obj =
        let fields = runtime.GetConstant fieldsId :?> RecordField[]
        let definition =
            if definitionId >= 0 then Some(runtime.GetConstant definitionId :?> TypeDefinition) else None
        try RecordOps.defineType runtime name fields definition isStruct
        with error -> raise (EvaluationException(error.Message, None))

    static member DefineInterface(runtime: RuntimeState, definitionId: int) : obj =
        let definition = runtime.GetConstant definitionId :?> InterfaceDefinition
        try RecordOps.defineInterface runtime definition
        with error -> raise (EvaluationException(error.Message, None))

    static member DefineDelegate(runtime: RuntimeState, definitionId: int) : obj =
        let definition = runtime.GetConstant definitionId :?> DelegateDefinition
        try RecordOps.defineDelegate runtime definition
        with error -> raise (EvaluationException(error.Message, None))

    static member CreateDelegate(runtime: RuntimeState, typeId: int, callback: obj) : obj =
        let delegateType = runtime.GetConstant typeId :?> Type
        try DelegateOps.Create(delegateType, callback) :> obj
        with error -> raise (EvaluationException(error.Message, None))

    static member Declare(runtime: RuntimeState, symbolId: int) : obj =
        let symbol = runtime.GetConstant symbolId :?> Symbol
        VarOps.declare runtime symbol |> ignore
        null

    static member SetVar(runtime: RuntimeState, symbolId: int, value: obj) : obj =
        let symbol = runtime.GetConstant symbolId :?> Symbol
        let namespaceName =
            match symbol.Namespace with
            | Some namespaceName ->
                runtime.Registry.ResolveAlias(runtime.CurrentNamespace, namespaceName)
                |> Option.defaultValue namespaceName
            | None -> runtime.CurrentNamespace
        match runtime.Registry.FindVar(namespaceName, symbol.Name) with
        | Some variable ->
            variable.Root <- value
            value
        | None ->
            raise (EvaluationException(sprintf "Cannot set unbound var %s." symbol.FullName, None))

    static member DefineMethod
        (
            runtime: RuntimeState,
            typeName: string,
            methodName: string,
            implementation: obj,
            isPrivate: bool,
            namespaceName: string
        ) : obj =
        RecordOps.defineMethod runtime typeName methodName implementation isPrivate namespaceName

    static member DefineConstructor
        (
            runtime: RuntimeState,
            typeName: string,
            implementation: obj,
            parameterHintsId: int,
            isPrivate: bool,
            namespaceName: string
        ) : obj =
        let parameterHints =
            if parameterHintsId < 0 then []
            else runtime.GetConstant parameterHintsId :?> Type option list
        RecordOps.defineConstructor runtime typeName implementation parameterHints isPrivate namespaceName

    static member InvokeRecordMethod(runtime: RuntimeState, target: obj, memberName: string, arguments: obj[]) : obj =
        if isNull (box runtime) then
            raise (NotSupportedException(sprintf "Record has no runtime for interface method %s." memberName))
        match target with
        | :? IWispRecord as record ->
            match runtime.TryFindMethod(record.TypeName, memberName) with
            | Some methodInfo ->
                match methodInfo.Implementation with
                | :? IFn as implementation -> implementation.Invoke(Array.append [| target |] arguments)
                | _ ->
                    raise (EvaluationException(sprintf "%s.%s is not callable." record.TypeName memberName, None))
            | None ->
                raise (NotSupportedException(sprintf "Record %s has no method %s." record.TypeName memberName))
        | _ -> raise (NotSupportedException(sprintf "Cannot invoke interface method %s on a non-record value." memberName))

    static member ConvertResult(value: obj, targetType: Type) : obj =
        match Clr.tryConvertValue value targetType with
        | Some converted -> converted
        | None ->
            raise (
                EvaluationException(
                    sprintf "Cannot convert interface result %s to %O." (Print.printString value) targetType,
                    None
                )
            )

    static member Recur(values: obj[]) : obj = raise (RecurSignal values)

    static member Throw(value: obj) : obj =
        match value with
        | :? Exception as error -> raise error
        | _ ->
            let message =
                match value with
                | :? string as text -> text
                | _ -> Print.printString value
            raise (EvaluationException(message, None))

module Codegen =
    let private runtimeOpsMethod (name: string) =
        match typeof<RuntimeOps>.GetMethod(name, BindingFlags.Public ||| BindingFlags.Static) with
        | null -> failwithf "Missing RuntimeOps.%s" name
        | methodInfo -> methodInfo

    let private constantMethod = runtimeOpsMethod "Constant"
    let private emptyEnvMethod = runtimeOpsMethod "EmptyEnv"
    let private bindEnvMethod = runtimeOpsMethod "BindEnv"
    let private lookupEnvMethod = runtimeOpsMethod "LookupEnv"
    let private isTruthyMethod = runtimeOpsMethod "IsTruthy"
    let private bindClosureMethod = runtimeOpsMethod "BindClosure"
    let private invokeMethod = runtimeOpsMethod "Invoke"
    let private resolveVarMethod = runtimeOpsMethod "ResolveVar"
    let private defMethod = runtimeOpsMethod "Def"
    let private setNamespaceMethod = runtimeOpsMethod "SetNamespace"
    let private importTypeMethod = runtimeOpsMethod "ImportType"
    let private requireNamespaceMethod = runtimeOpsMethod "RequireNamespace"
    let private referVarMethod = runtimeOpsMethod "ReferVar"
    let private useNamespaceMethod = runtimeOpsMethod "UseNamespace"
    let private useNamespaceOnlyMethod = runtimeOpsMethod "UseNamespaceOnly"
    let private staticCallMethod = runtimeOpsMethod "StaticCall"
    let private instanceCallMethod = runtimeOpsMethod "InstanceCall"
    let private instancePropertyMethod = runtimeOpsMethod "InstanceProperty"
    let private constructMethod = runtimeOpsMethod "Construct"
    let private constructHintMethod = runtimeOpsMethod "ConstructHint"
    let private typeTestHintMethod = runtimeOpsMethod "TypeTestHint"
    let private defineTypeMethod = runtimeOpsMethod "DefineType"
    let private defineInterfaceMethod = runtimeOpsMethod "DefineInterface"
    let private defineDelegateMethod = runtimeOpsMethod "DefineDelegate"
    let private createDelegateMethod = runtimeOpsMethod "CreateDelegate"
    let private setVarMethod = runtimeOpsMethod "SetVar"
    let private declareMethod = runtimeOpsMethod "Declare"
    let private defineMethodMethod = runtimeOpsMethod "DefineMethod"
    let private defineConstructorMethod = runtimeOpsMethod "DefineConstructor"
    let private recurMethod = runtimeOpsMethod "Recur"
    let private throwMethod = runtimeOpsMethod "Throw"
    let private asTaskMethod =
        typeof<AsyncOps>.GetMethod("AsTask", [| typeof<obj> |])
    let private awaitSyncMethod =
        typeof<AsyncOps>.GetMethod("AwaitSync", [| typeof<obj> |])
    let private toValueTaskMethod =
        typeof<AsyncOps>.GetMethod("ToValueTask", [| typeof<obj> |])
    let private toValueTaskOfMethod =
        typeof<AsyncOps>.GetMethod("ToValueTaskOf", [| typeof<obj>; typeof<Type> |])
    let private convertResultMethod = runtimeOpsMethod "ConvertResult"
    let private getTypeFromHandle =
        typeof<Type>.GetMethod("GetTypeFromHandle", [| typeof<RuntimeTypeHandle> |])
    let private recurSignalValues = typeof<RecurSignal>.GetProperty("Values").GetGetMethod()
    let private afnConstructor = typeof<AFn>.GetConstructor(Type.EmptyTypes)
    let private afnInvoke = typeof<AFn>.GetMethod("Invoke", [| typeof<obj[]> |])

    /// Compiles a Core expression into a static evaluator method. Lambda nodes
    /// are compiled into real AFn subclasses whose Invoke methods contain IL,
    /// not interpreter callbacks.
    let private compileInModule
        (moduleBuilder: ModuleBuilder)
        (typeName: string)
        (runtime: RuntimeState)
        (expression: CoreExpr)
        (currentNamespace: string)
        : Func<RuntimeState, obj> =
        let typeBuilder =
            moduleBuilder.DefineType(
                typeName,
                TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Abstract
            )
        let methodBuilder =
            typeBuilder.DefineMethod(
                "Evaluate",
                MethodAttributes.Public ||| MethodAttributes.Static,
                typeof<obj>,
                [| typeof<RuntimeState> |]
            )
        let il = methodBuilder.GetILGenerator()

        let emitConstant (il: ILGenerator) (runtimeLocal: LocalBuilder) (value: obj) =
            if isNull value then
                il.Emit OpCodes.Ldnull
            else
                let constantId = runtime.RegisterConstant value
                il.Emit(OpCodes.Ldloc, runtimeLocal)
                il.Emit(OpCodes.Ldc_I4, constantId)
                il.Emit(OpCodes.Call, constantMethod)

        let useRuntimeAsync =
            not runtime.ForceCompilerAsync && AsyncSupport.runtimeAsyncAvailable

        let rec emitIn
            (isAsync: bool)
            (il: ILGenerator)
            (runtimeLocal: LocalBuilder)
            (environment: LocalBuilder)
            (expression: CoreExpr)
            : unit =
            match expression.Node with
            | Const value -> emitConstant il runtimeLocal value
            | Local name ->
                il.Emit(OpCodes.Ldloc, environment)
                il.Emit(OpCodes.Ldstr, name)
                il.Emit(OpCodes.Call, lookupEnvMethod)
            | VarRef symbol ->
                let symbolId = runtime.RegisterConstant symbol
                il.Emit(OpCodes.Ldloc, runtimeLocal)
                il.Emit(OpCodes.Ldc_I4, symbolId)
                il.Emit(OpCodes.Ldstr, currentNamespace)
                il.Emit(OpCodes.Call, resolveVarMethod)
            | If(condition, whenTrue, whenFalse) ->
                let falseLabel = il.DefineLabel()
                let endLabel = il.DefineLabel()
                emitIn isAsync il runtimeLocal environment condition
                il.Emit(OpCodes.Call, isTruthyMethod)
                il.Emit(OpCodes.Brfalse, falseLabel)
                emitIn isAsync il runtimeLocal environment whenTrue
                il.Emit(OpCodes.Br, endLabel)
                il.MarkLabel falseLabel
                emitIn isAsync il runtimeLocal environment whenFalse
                il.MarkLabel endLabel
            | Do expressions ->
                match expressions with
                | [] -> il.Emit OpCodes.Ldnull
                | _ ->
                    expressions
                    |> List.iteri (fun index item ->
                        emitIn isAsync il runtimeLocal environment item
                        if index < expressions.Length - 1 then il.Emit OpCodes.Pop)
            | Let(name, value, body) ->
                let valueLocal = il.DeclareLocal(typeof<obj>)
                emitIn isAsync il runtimeLocal environment value
                il.Emit(OpCodes.Stloc, valueLocal)
                il.Emit(OpCodes.Ldloc, environment)
                il.Emit(OpCodes.Ldstr, name)
                il.Emit(OpCodes.Ldloc, valueLocal)
                il.Emit(OpCodes.Call, bindEnvMethod)
                il.Emit(OpCodes.Stloc, environment)
                emitIn isAsync il runtimeLocal environment body
            | Lambda _ ->
                let closureType = defineClosure expression
                let constructor = closureType.GetConstructor([| typeof<RuntimeState>; typeof<obj> |])
                il.Emit(OpCodes.Ldloc, runtimeLocal)
                il.Emit(OpCodes.Ldloc, environment)
                il.Emit(OpCodes.Newobj, constructor)
            | Call(callee, arguments) ->
                let calleeLocal = il.DeclareLocal(typeof<obj>)
                emitIn isAsync il runtimeLocal environment callee
                il.Emit(OpCodes.Stloc, calleeLocal)
                let argumentsLocal = emitArguments isAsync il runtimeLocal environment arguments
                il.Emit(OpCodes.Ldloc, calleeLocal)
                il.Emit(OpCodes.Ldloc, argumentsLocal)
                il.Emit(OpCodes.Call, invokeMethod)
            | Def(symbol, value) ->
                let symbolId = runtime.RegisterConstant symbol
                let valueLocal = il.DeclareLocal(typeof<obj>)
                emitIn isAsync il runtimeLocal environment value
                il.Emit(OpCodes.Stloc, valueLocal)
                il.Emit(OpCodes.Ldloc, runtimeLocal)
                il.Emit(OpCodes.Ldc_I4, symbolId)
                il.Emit(OpCodes.Ldloc, valueLocal)
                il.Emit(OpCodes.Call, defMethod)
            | Quote value -> emitConstant il runtimeLocal value
            | SetNamespace(namespaceName, doc) ->
                il.Emit(OpCodes.Ldloc, runtimeLocal)
                il.Emit(OpCodes.Ldstr, namespaceName)
                match doc with
                | Some text -> il.Emit(OpCodes.Ldstr, text)
                | None -> il.Emit OpCodes.Ldnull
                il.Emit(OpCodes.Call, setNamespaceMethod)
            | ImportType(alias, fullName) ->
                il.Emit(OpCodes.Ldloc, runtimeLocal)
                il.Emit(OpCodes.Ldstr, alias)
                il.Emit(OpCodes.Ldstr, fullName)
                il.Emit(OpCodes.Call, importTypeMethod)
            | RequireNamespace(alias, namespaceName) ->
                il.Emit(OpCodes.Ldloc, runtimeLocal)
                il.Emit(OpCodes.Ldstr, alias)
                il.Emit(OpCodes.Ldstr, namespaceName)
                il.Emit(OpCodes.Call, requireNamespaceMethod)
            | ReferVar(localName, namespaceName, varName) ->
                il.Emit(OpCodes.Ldloc, runtimeLocal)
                il.Emit(OpCodes.Ldstr, localName)
                il.Emit(OpCodes.Ldstr, namespaceName)
                il.Emit(OpCodes.Ldstr, varName)
                il.Emit(OpCodes.Call, referVarMethod)
            | UseNamespace namespaceName ->
                il.Emit(OpCodes.Ldloc, runtimeLocal)
                il.Emit(OpCodes.Ldstr, namespaceName)
                il.Emit(OpCodes.Call, useNamespaceMethod)
            | UseNamespaceOnly(namespaceName, names) ->
                il.Emit(OpCodes.Ldloc, runtimeLocal)
                il.Emit(OpCodes.Ldstr, namespaceName)
                il.Emit(OpCodes.Ldc_I4, names.Length)
                il.Emit(OpCodes.Newarr, typeof<string>)
                for index in 0 .. names.Length - 1 do
                    il.Emit(OpCodes.Dup)
                    il.Emit(OpCodes.Ldc_I4, index)
                    il.Emit(OpCodes.Ldstr, names.[index])
                    il.Emit(OpCodes.Stelem_Ref)
                il.Emit(OpCodes.Call, useNamespaceOnlyMethod)
            | StaticCall(clrType, memberName, arguments) ->
                let typeId = runtime.RegisterConstant clrType
                let argumentsLocal = emitArguments isAsync il runtimeLocal environment arguments
                il.Emit(OpCodes.Ldloc, runtimeLocal)
                il.Emit(OpCodes.Ldc_I4, typeId)
                il.Emit(OpCodes.Ldstr, memberName)
                il.Emit(OpCodes.Ldloc, argumentsLocal)
                il.Emit(OpCodes.Call, staticCallMethod)
            | InstanceCall(target, memberName, arguments) ->
                let targetLocal = il.DeclareLocal(typeof<obj>)
                emitIn isAsync il runtimeLocal environment target
                il.Emit(OpCodes.Stloc, targetLocal)
                let argumentsLocal = emitArguments isAsync il runtimeLocal environment arguments
                il.Emit(OpCodes.Ldloc, runtimeLocal)
                il.Emit(OpCodes.Ldloc, targetLocal)
                il.Emit(OpCodes.Ldstr, memberName)
                il.Emit(OpCodes.Ldloc, argumentsLocal)
                il.Emit(OpCodes.Call, instanceCallMethod)
            | InstanceProperty(target, memberName) ->
                emitIn isAsync il runtimeLocal environment target
                il.Emit(OpCodes.Ldstr, memberName)
                il.Emit(OpCodes.Call, instancePropertyMethod)
            | Construct(clrType, arguments) ->
                let typeId = runtime.RegisterConstant clrType
                let argumentsLocal = emitArguments isAsync il runtimeLocal environment arguments
                il.Emit(OpCodes.Ldloc, runtimeLocal)
                il.Emit(OpCodes.Ldc_I4, typeId)
                il.Emit(OpCodes.Ldloc, argumentsLocal)
                il.Emit(OpCodes.Call, constructMethod)
            | ConstructHint(hintForm, arguments) ->
                let hintId = runtime.RegisterConstant hintForm
                let argumentsLocal = emitArguments isAsync il runtimeLocal environment arguments
                il.Emit(OpCodes.Ldloc, runtimeLocal)
                il.Emit(OpCodes.Ldc_I4, hintId)
                il.Emit(OpCodes.Ldloc, argumentsLocal)
                il.Emit(OpCodes.Call, constructHintMethod)
            | TypeTestHint(hintForm, value) ->
                let hintId = runtime.RegisterConstant hintForm
                il.Emit(OpCodes.Ldloc, runtimeLocal)
                il.Emit(OpCodes.Ldc_I4, hintId)
                emitIn isAsync il runtimeLocal environment value
                il.Emit(OpCodes.Call, typeTestHintMethod)
            | DefineType(name, fields, definition, isStruct) ->
                let fieldsId = runtime.RegisterConstant (fields |> Array.ofList)
                let definitionId =
                    match definition with
                    | Some value -> runtime.RegisterConstant value
                    | None -> -1
                il.Emit(OpCodes.Ldloc, runtimeLocal)
                il.Emit(OpCodes.Ldstr, name)
                il.Emit(OpCodes.Ldc_I4, fieldsId)
                il.Emit(OpCodes.Ldc_I4, definitionId)
                il.Emit(OpCodes.Ldc_I4, if isStruct then 1 else 0)
                il.Emit(OpCodes.Call, defineTypeMethod)
            | DefineInterface(definition) ->
                let definitionId = runtime.RegisterConstant definition
                il.Emit(OpCodes.Ldloc, runtimeLocal)
                il.Emit(OpCodes.Ldc_I4, definitionId)
                il.Emit(OpCodes.Call, defineInterfaceMethod)
            | DefineDelegate(definition) ->
                let definitionId = runtime.RegisterConstant definition
                il.Emit(OpCodes.Ldloc, runtimeLocal)
                il.Emit(OpCodes.Ldc_I4, definitionId)
                il.Emit(OpCodes.Call, defineDelegateMethod)
            | CreateDelegate(delegateType, callback) ->
                let typeId = runtime.RegisterConstant delegateType
                il.Emit(OpCodes.Ldloc, runtimeLocal)
                il.Emit(OpCodes.Ldc_I4, typeId)
                emitIn isAsync il runtimeLocal environment callback
                il.Emit(OpCodes.Call, createDelegateMethod)
            | Declare symbols ->
                for symbol in symbols do
                    let symbolId = runtime.RegisterConstant symbol
                    il.Emit(OpCodes.Ldloc, runtimeLocal)
                    il.Emit(OpCodes.Ldc_I4, symbolId)
                    il.Emit(OpCodes.Call, declareMethod)
                    il.Emit OpCodes.Pop
                il.Emit OpCodes.Ldnull
            | SetVar(symbol, value) ->
                let symbolId = runtime.RegisterConstant symbol
                let valueLocal = il.DeclareLocal(typeof<obj>)
                emitIn isAsync il runtimeLocal environment value
                il.Emit(OpCodes.Stloc, valueLocal)
                il.Emit(OpCodes.Ldloc, runtimeLocal)
                il.Emit(OpCodes.Ldc_I4, symbolId)
                il.Emit(OpCodes.Ldloc, valueLocal)
                il.Emit(OpCodes.Call, setVarMethod)
            | DefineMethod(typeName, methodName, value, isPrivate, namespaceName) ->
                let valueLocal = il.DeclareLocal(typeof<obj>)
                emitIn isAsync il runtimeLocal environment value
                il.Emit(OpCodes.Stloc, valueLocal)
                il.Emit(OpCodes.Ldloc, runtimeLocal)
                il.Emit(OpCodes.Ldstr, typeName)
                il.Emit(OpCodes.Ldstr, methodName)
                il.Emit(OpCodes.Ldloc, valueLocal)
                il.Emit(OpCodes.Ldc_I4, if isPrivate then 1 else 0)
                il.Emit(OpCodes.Ldstr, namespaceName)
                il.Emit(OpCodes.Call, defineMethodMethod)
            | DefineConstructor(typeName, value, parameterHints, isPrivate, namespaceName) ->
                let valueLocal = il.DeclareLocal(typeof<obj>)
                emitIn isAsync il runtimeLocal environment value
                il.Emit(OpCodes.Stloc, valueLocal)
                let hintsId = runtime.RegisterConstant parameterHints
                il.Emit(OpCodes.Ldloc, runtimeLocal)
                il.Emit(OpCodes.Ldstr, typeName)
                il.Emit(OpCodes.Ldloc, valueLocal)
                il.Emit(OpCodes.Ldc_I4, hintsId)
                il.Emit(OpCodes.Ldc_I4, if isPrivate then 1 else 0)
                il.Emit(OpCodes.Ldstr, namespaceName)
                il.Emit(OpCodes.Call, defineConstructorMethod)
            | Loop(bindings, body) ->
                let outerEnvironment = il.DeclareLocal(typeof<obj>)
                let valuesLocal = il.DeclareLocal(typeof<obj[]>)
                let recurred = il.DeclareLocal(typeof<bool>)
                let resultLocal = il.DeclareLocal(typeof<obj>)
                let loopStart = il.DefineLabel()
                il.Emit(OpCodes.Ldloc, environment)
                il.Emit(OpCodes.Stloc, outerEnvironment)
                let initialValues = emitArguments isAsync il runtimeLocal environment (bindings |> List.map snd)
                il.Emit(OpCodes.Ldloc, initialValues)
                il.Emit(OpCodes.Stloc, valuesLocal)
                il.MarkLabel loopStart
                il.Emit(OpCodes.Ldloc, outerEnvironment)
                il.Emit(OpCodes.Stloc, environment)
                for index in 0 .. bindings.Length - 1 do
                    let name = fst bindings.[index]
                    il.Emit(OpCodes.Ldloc, environment)
                    il.Emit(OpCodes.Ldstr, name)
                    il.Emit(OpCodes.Ldloc, valuesLocal)
                    il.Emit(OpCodes.Ldc_I4, index)
                    il.Emit(OpCodes.Ldelem_Ref)
                    il.Emit(OpCodes.Call, bindEnvMethod)
                    il.Emit(OpCodes.Stloc, environment)
                il.Emit(OpCodes.Ldc_I4_0)
                il.Emit(OpCodes.Stloc, recurred)
                let endLabel = il.BeginExceptionBlock()
                emitIn isAsync il runtimeLocal environment body
                il.Emit(OpCodes.Stloc, resultLocal)
                il.Emit(OpCodes.Leave, endLabel)
                il.BeginCatchBlock(typeof<RecurSignal>)
                il.Emit(OpCodes.Callvirt, recurSignalValues)
                il.Emit(OpCodes.Stloc, valuesLocal)
                il.Emit(OpCodes.Ldc_I4_1)
                il.Emit(OpCodes.Stloc, recurred)
                il.Emit(OpCodes.Leave, endLabel)
                il.EndExceptionBlock()
                il.Emit(OpCodes.Ldloc, recurred)
                il.Emit(OpCodes.Brtrue, loopStart)
                il.Emit(OpCodes.Ldloc, resultLocal)
            | Recur(arguments) ->
                let argumentsLocal = emitArguments isAsync il runtimeLocal environment arguments
                il.Emit(OpCodes.Ldloc, argumentsLocal)
                il.Emit(OpCodes.Call, recurMethod)
            | Try(tryBody, catches, finallyBody) ->
                if catches.IsEmpty && finallyBody.IsNone then
                    emitIn isAsync il runtimeLocal environment tryBody
                else
                    let resultLocal = il.DeclareLocal(typeof<obj>)
                    let exceptionLocal = il.DeclareLocal(typeof<Exception>)
                    let endLabel = il.BeginExceptionBlock()
                    emitIn isAsync il runtimeLocal environment tryBody
                    il.Emit(OpCodes.Stloc, resultLocal)
                    il.Emit(OpCodes.Leave, endLabel)

                    // Handlers are emitted in source order so the CLR's typed
                    // match picks the first assignable clause.
                    for clause in catches do
                        match clause.ExceptionType with
                        | Some clrType -> il.BeginCatchBlock clrType
                        | None -> il.BeginCatchBlock typeof<Exception>
                        il.Emit(OpCodes.Stloc, exceptionLocal)
                        let handleLabel = il.DefineLabel()
                        il.Emit(OpCodes.Ldloc, exceptionLocal)
                        il.Emit(OpCodes.Isinst, typeof<RecurSignal>)
                        il.Emit(OpCodes.Brfalse, handleLabel)
                        il.Emit(OpCodes.Ldloc, exceptionLocal)
                        il.Emit(OpCodes.Throw)
                        il.MarkLabel handleLabel
                        il.Emit(OpCodes.Ldloc, environment)
                        il.Emit(OpCodes.Ldstr, clause.Binding)
                        il.Emit(OpCodes.Ldloc, exceptionLocal)
                        il.Emit(OpCodes.Call, bindEnvMethod)
                        il.Emit(OpCodes.Stloc, environment)
                        emitIn isAsync il runtimeLocal environment clause.Body
                        il.Emit(OpCodes.Stloc, resultLocal)
                        il.Emit(OpCodes.Leave, endLabel)

                    match finallyBody with
                    | Some body ->
                        il.BeginFinallyBlock()
                        emitIn isAsync il runtimeLocal environment body
                        il.Emit(OpCodes.Pop)
                    | None -> ()

                    il.EndExceptionBlock()
                    il.Emit(OpCodes.Ldloc, resultLocal)
            | Async(returnKind, body) when useRuntimeAsync ->
                let start = defineAsyncMethod returnKind body
                il.Emit(OpCodes.Ldloc, runtimeLocal)
                il.Emit(OpCodes.Ldloc, environment)
                il.Emit(OpCodes.Call, start)
                match returnKind with
                | AsyncTask -> ()
                | AsyncValueTask ->
                    il.Emit(OpCodes.Box, typeof<System.Threading.Tasks.ValueTask>)
                    il.Emit(OpCodes.Call, toValueTaskMethod)
                | AsyncValueTaskOf resultType ->
                    il.Emit(OpCodes.Box, typedefof<System.Threading.Tasks.ValueTask<_>>.MakeGenericType resultType)
                    emitConstant il runtimeLocal (box resultType)
                    il.Emit(OpCodes.Castclass, typeof<Type>)
                    il.Emit(OpCodes.Call, toValueTaskOfMethod)
            | Async(returnKind, body) ->
                let started = AsyncTransform.start body
                emitIn isAsync il runtimeLocal environment started
                match returnKind with
                | AsyncTask -> ()
                | AsyncValueTask -> il.Emit(OpCodes.Call, toValueTaskMethod)
                | AsyncValueTaskOf resultType ->
                    emitConstant il runtimeLocal (box resultType)
                    il.Emit(OpCodes.Castclass, typeof<Type>)
                    il.Emit(OpCodes.Call, toValueTaskOfMethod)
            | Await value ->
                emitIn isAsync il runtimeLocal environment value
                if isAsync && useRuntimeAsync then
                    il.Emit(OpCodes.Call, asTaskMethod)
                    match AsyncSupport.awaitTaskOfObject with
                    | Some awaitMethod -> il.Emit(OpCodes.Call, awaitMethod)
                    | None -> il.Emit(OpCodes.Call, awaitSyncMethod)
                else
                    il.Emit(OpCodes.Call, awaitSyncMethod)
            | Throw(value) ->
                emitIn isAsync il runtimeLocal environment value
                il.Emit(OpCodes.Call, throwMethod)

        and emitArguments
            (isAsync: bool)
            (il: ILGenerator)
            (runtimeLocal: LocalBuilder)
            (environment: LocalBuilder)
            (arguments: CoreExpr list)
            : LocalBuilder =
            let argumentsLocal = il.DeclareLocal(typeof<obj[]>)
            il.Emit(OpCodes.Ldc_I4, arguments.Length)
            il.Emit(OpCodes.Newarr, typeof<obj>)
            il.Emit(OpCodes.Stloc, argumentsLocal)
            arguments
            |> List.iteri (fun index argument ->
                let valueLocal = il.DeclareLocal(typeof<obj>)
                emitIn isAsync il runtimeLocal environment argument
                il.Emit(OpCodes.Stloc, valueLocal)
                il.Emit(OpCodes.Ldloc, argumentsLocal)
                il.Emit(OpCodes.Ldc_I4, index)
                il.Emit(OpCodes.Ldloc, valueLocal)
                il.Emit(OpCodes.Stelem_Ref))
            argumentsLocal

        and defineClosure (expression: CoreExpr) : Type =
            let name, parameters, rest, body =
                match expression.Node with
                | Lambda(name, parameters, rest, body) -> name, parameters, rest, body
                | _ -> failwith "defineClosure called with a non-lambda expression."

            let closureName = "Wisp.Generated.Closure_" + Guid.NewGuid().ToString("N")
            let closureBuilder =
                moduleBuilder.DefineType(
                    closureName,
                    TypeAttributes.Public ||| TypeAttributes.Sealed,
                    typeof<AFn>
                )
            let runtimeField =
                closureBuilder.DefineField("_runtime", typeof<RuntimeState>, FieldAttributes.Private)
            let environmentField =
                closureBuilder.DefineField("_environment", typeof<obj>, FieldAttributes.Private)

            let constructorBuilder =
                closureBuilder.DefineConstructor(
                    MethodAttributes.Public,
                    CallingConventions.Standard,
                    [| typeof<RuntimeState>; typeof<obj> |]
                )
            let constructorIl = constructorBuilder.GetILGenerator()
            constructorIl.Emit(OpCodes.Ldarg_0)
            constructorIl.Emit(OpCodes.Call, afnConstructor)
            constructorIl.Emit(OpCodes.Ldarg_0)
            constructorIl.Emit(OpCodes.Ldarg_1)
            constructorIl.Emit(OpCodes.Stfld, runtimeField)
            constructorIl.Emit(OpCodes.Ldarg_0)
            constructorIl.Emit(OpCodes.Ldarg_2)
            constructorIl.Emit(OpCodes.Stfld, environmentField)
            constructorIl.Emit(OpCodes.Ret)

            let invokeBuilder =
                closureBuilder.DefineMethod(
                    "Invoke",
                    MethodAttributes.Public ||| MethodAttributes.Virtual ||| MethodAttributes.HideBySig,
                    typeof<obj>,
                    [| typeof<obj[]> |]
                )
            closureBuilder.DefineMethodOverride(invokeBuilder, afnInvoke)
            let invokeIl = invokeBuilder.GetILGenerator()
            let runtimeLocal = invokeIl.DeclareLocal(typeof<RuntimeState>)
            invokeIl.Emit(OpCodes.Ldarg_0)
            invokeIl.Emit(OpCodes.Ldfld, runtimeField)
            invokeIl.Emit(OpCodes.Stloc, runtimeLocal)
            let capturedLocal = invokeIl.DeclareLocal(typeof<obj>)
            invokeIl.Emit(OpCodes.Ldarg_0)
            invokeIl.Emit(OpCodes.Ldfld, environmentField)
            invokeIl.Emit(OpCodes.Stloc, capturedLocal)
            let environment = invokeIl.DeclareLocal(typeof<obj>)
            let valuesLocal = invokeIl.DeclareLocal(typeof<obj[]>)
            invokeIl.Emit(OpCodes.Ldarg_1)
            invokeIl.Emit(OpCodes.Stloc, valuesLocal)
            let initialLocal = invokeIl.DeclareLocal(typeof<bool>)
            invokeIl.Emit(OpCodes.Ldc_I4_1)
            invokeIl.Emit(OpCodes.Stloc, initialLocal)
            let recurredLocal = invokeIl.DeclareLocal(typeof<bool>)
            let resultLocal = invokeIl.DeclareLocal(typeof<obj>)
            let parameterNamesId = runtime.RegisterConstant (parameters |> Array.ofList)
            let loopStart = invokeIl.DefineLabel()

            invokeIl.MarkLabel loopStart
            invokeIl.Emit(OpCodes.Ldloc, capturedLocal)
            invokeIl.Emit(OpCodes.Ldloc, runtimeLocal)
            invokeIl.Emit(OpCodes.Ldc_I4, parameterNamesId)
            invokeIl.Emit(OpCodes.Call, constantMethod)
            invokeIl.Emit(OpCodes.Castclass, typeof<string[]>)
            match rest with
            | Some restName -> invokeIl.Emit(OpCodes.Ldstr, restName)
            | None -> invokeIl.Emit(OpCodes.Ldnull)
            invokeIl.Emit(OpCodes.Ldloc, valuesLocal)
            match name with
            | Some functionName -> invokeIl.Emit(OpCodes.Ldstr, functionName)
            | None -> invokeIl.Emit(OpCodes.Ldnull)
            invokeIl.Emit(OpCodes.Ldarg_0)
            invokeIl.Emit(OpCodes.Ldloc, initialLocal)
            invokeIl.Emit(OpCodes.Call, bindClosureMethod)
            invokeIl.Emit(OpCodes.Stloc, environment)

            invokeIl.Emit(OpCodes.Ldc_I4_0)
            invokeIl.Emit(OpCodes.Stloc, recurredLocal)
            let endLabel = invokeIl.BeginExceptionBlock()
            emitIn false invokeIl runtimeLocal environment body
            invokeIl.Emit(OpCodes.Stloc, resultLocal)
            invokeIl.Emit(OpCodes.Leave, endLabel)
            invokeIl.BeginCatchBlock(typeof<RecurSignal>)
            invokeIl.Emit(OpCodes.Callvirt, recurSignalValues)
            invokeIl.Emit(OpCodes.Stloc, valuesLocal)
            invokeIl.Emit(OpCodes.Ldc_I4_1)
            invokeIl.Emit(OpCodes.Stloc, recurredLocal)
            invokeIl.Emit(OpCodes.Ldc_I4_0)
            invokeIl.Emit(OpCodes.Stloc, initialLocal)
            invokeIl.Emit(OpCodes.Leave, endLabel)
            invokeIl.EndExceptionBlock()
            invokeIl.Emit(OpCodes.Ldloc, recurredLocal)
            invokeIl.Emit(OpCodes.Brtrue, loopStart)
            invokeIl.Emit(OpCodes.Ldloc, resultLocal)
            invokeIl.Emit(OpCodes.Ret)

            closureBuilder.CreateTypeInfo().AsType()

        and defineAsyncMethod (returnKind: AsyncResultKind) (body: CoreExpr) : MethodInfo =
            let asyncName = "Wisp.Generated.Async_" + Guid.NewGuid().ToString("N")
            let asyncBuilder =
                moduleBuilder.DefineType(
                    asyncName,
                    TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Abstract
                )
            let returnType =
                match returnKind with
                | AsyncTask -> typeof<System.Threading.Tasks.Task<obj>>
                | AsyncValueTask -> typeof<System.Threading.Tasks.ValueTask>
                | AsyncValueTaskOf resultType ->
                    typedefof<System.Threading.Tasks.ValueTask<_>>.MakeGenericType resultType
            let methodBuilder =
                asyncBuilder.DefineMethod(
                    "Start",
                    MethodAttributes.Public ||| MethodAttributes.Static,
                    returnType,
                    [| typeof<RuntimeState>; typeof<obj> |]
                )
            methodBuilder.SetImplementationFlags MethodImplAttributes.Async
            let asyncIl = methodBuilder.GetILGenerator()
            let asyncRuntime = asyncIl.DeclareLocal(typeof<RuntimeState>)
            asyncIl.Emit(OpCodes.Ldarg_0)
            asyncIl.Emit(OpCodes.Stloc, asyncRuntime)
            let asyncEnvironment = asyncIl.DeclareLocal(typeof<obj>)
            asyncIl.Emit(OpCodes.Ldarg_1)
            asyncIl.Emit(OpCodes.Stloc, asyncEnvironment)
            emitIn true asyncIl asyncRuntime asyncEnvironment body
            match returnKind with
            | AsyncTask -> ()
            | AsyncValueTask -> asyncIl.Emit OpCodes.Pop
            | AsyncValueTaskOf resultType ->
                asyncIl.Emit(OpCodes.Ldtoken, resultType)
                asyncIl.Emit(OpCodes.Call, getTypeFromHandle)
                asyncIl.Emit(OpCodes.Call, convertResultMethod)
                if resultType.IsValueType || resultType.IsGenericParameter then
                    asyncIl.Emit(OpCodes.Unbox_Any, resultType)
                else
                    asyncIl.Emit(OpCodes.Castclass, resultType)
            asyncIl.Emit OpCodes.Ret
            let created = asyncBuilder.CreateTypeInfo().AsType()
            created.GetMethod("Start")

        let emit (il: ILGenerator) (runtimeLocal: LocalBuilder) (environment: LocalBuilder) (expression: CoreExpr) =
            emitIn false il runtimeLocal environment expression

        let runtimeLocal = il.DeclareLocal(typeof<RuntimeState>)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Stloc, runtimeLocal)
        let environment = il.DeclareLocal(typeof<obj>)
        il.Emit(OpCodes.Call, emptyEnvMethod)
        il.Emit(OpCodes.Stloc, environment)
        emit il runtimeLocal environment expression
        il.Emit OpCodes.Ret

        let evaluatorType = typeBuilder.CreateTypeInfo().AsType()
        let evaluateMethod = evaluatorType.GetMethod("Evaluate")
        let compiled = evaluateMethod.CreateDelegate(typeof<Func<RuntimeState, obj>>)
        compiled :?> Func<RuntimeState, obj>

    /// One persistent collectible assembly per session; each form adds evaluator
    /// and closure types. Redefining vars gives REPL hot reload.
    [<Sealed>]
    type CodegenContext(runtime: RuntimeState) =
        let assemblyName = AssemblyName("Wisp.Generated." + Guid.NewGuid().ToString("N"))
        let assembly = AssemblyBuilder.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.RunAndCollect)
        let moduleBuilder = assembly.DefineDynamicModule(assemblyName.Name)
        let mutable counter = 0
        do runtime.RegisterAssembly assembly

        member _.Assembly = assembly :> Assembly

        member _.Compile(expression: CoreExpr, currentNamespace: string) =
            let typeName = sprintf "Wisp.Generated.Evaluator%d" counter
            counter <- counter + 1
            compileInModule moduleBuilder typeName runtime expression currentNamespace

    let compile (runtime: RuntimeState) (expression: CoreExpr) (currentNamespace: string) =
        CodegenContext(runtime).Compile(expression, currentNamespace)
