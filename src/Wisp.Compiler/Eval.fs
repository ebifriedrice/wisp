namespace Wisp.Compiler

open System
open Wisp.Core

type Env = Map<string, obj>

[<Sealed>]
type Closure
    (
        name: string option,
        parameters: string list,
        rest: string option,
        body: CoreExpr,
        captured: Env,
        definingNamespace: string,
        evaluate: CoreExpr -> Env -> string -> obj
    ) =
    inherit AFn()

    let mutable self: obj = null

    member _.Self
        with set value = self <- value

    override _.Name =
        match name with
        | Some functionName when not (String.IsNullOrWhiteSpace functionName) ->
            if String.IsNullOrWhiteSpace definingNamespace then functionName
            else definingNamespace + "/" + functionName
        | _ -> "fn"

    override _.InvokeCore(args: obj[]) =
        let required = parameters.Length
        if args.Length < required || (rest.IsNone && args.Length <> required) then
            let expected = if rest.IsSome then sprintf "at least %d" required else string required
            raise (ArityException(expected, args.Length))

        let mutable values = args
        let mutable initial = true
        let mutable running = true
        let mutable result: obj = null

        while running do
            let mutable environment = captured
            match name with
            | Some functionName -> environment <- environment.Add(functionName, self)
            | None -> ()
            for index in 0 .. required - 1 do
                environment <- environment.Add(parameters.[index], values.[index])
            match rest with
            | Some restName ->
                let restValue =
                    if initial then
                        if values.Length > required then
                            values.[required..] |> Array.toList |> PersistentList.OfSeq :> obj
                        else
                            null
                    else
                        values.[required]
                environment <- environment.Add(restName, restValue)
            | None -> ()

            try
                result <- evaluate body environment definingNamespace
                running <- false
            with :? RecurSignal as signal ->
                let expected = required + (if rest.IsSome then 1 else 0)
                if signal.Values.Length <> expected then
                    raise (
                        EvaluationException(
                            sprintf "recur expected %d arguments, got %d." expected signal.Values.Length,
                            body.Span
                        )
                    )
                values <- signal.Values
                initial <- false

        result

module Eval =
    let private raiseEvaluation message span = raise (EvaluationException(message, span))

    let rec resolveVar (runtime: RuntimeState) (symbol: Symbol) (currentNamespace: string) =
        let lookupVar namespaceName =
            runtime.Registry.FindVar(namespaceName, symbol.Name)
        match symbol.Namespace with
        | None ->
            match lookupVar currentNamespace with
            | Some variable -> variable.Root
            | None ->
                let referral =
                    runtime.Registry.TryFind currentNamespace
                    |> Option.bind (fun namespaceValue -> namespaceValue.TryResolveReferral symbol.Name)
                match referral with
                | Some(namespaceName, varName) ->
                    match runtime.Registry.FindVar(namespaceName, varName) with
                    | Some variable -> variable.Root
                    | None ->
                        raiseEvaluation
                            (sprintf "Unable to resolve referred var %s/%s." namespaceName varName)
                            (SourceMetadata.trySpan (symbol :> obj))
                | None ->
                    match runtime.Builtins.TryFind symbol.Name with
                    | Some value -> value
                    | None ->
                        raiseEvaluation
                            (sprintf "Unable to resolve symbol %s." symbol.FullName)
                            (SourceMetadata.trySpan (symbol :> obj))
        | Some namespaceName ->
            let namespaceName =
                runtime.Registry.ResolveAlias(currentNamespace, namespaceName)
                |> Option.defaultValue namespaceName
            match lookupVar namespaceName with
            | Some variable -> variable.Root
            | None ->
                match TypeHints.tryResolveName namespaceName with
                | Some clrType ->
                    try Clr.invokeStatic clrType symbol.Name [||]
                    with InteropException message ->
                        raiseEvaluation
                            (sprintf "Unable to resolve %s: %s" symbol.FullName message)
                            (SourceMetadata.trySpan (symbol :> obj))
                | None ->
                    raiseEvaluation
                        (sprintf "Unable to resolve symbol %s." symbol.FullName)
                        (SourceMetadata.trySpan (symbol :> obj))

    let rec eval (runtime: RuntimeState) (expression: CoreExpr) (environment: Env) (currentNamespace: string) : obj =
        let evaluate = eval runtime
        let invoke (value: obj) (arguments: obj[]) =
            match value with
            | :? Delegate as delegateValue -> DelegateOps.Invoke(delegateValue, arguments)
            | :? IFn as functionValue -> functionValue.Invoke arguments
            | _ -> raiseEvaluation (sprintf "%s is not callable." (Print.printString value)) expression.Span

        // Runtime exception types are part of try/catch semantics, so calls must
        // not flatten them into EvaluationException before a typed handler runs.
        // The call site is still published so an escaping exception gets a frame.
        let guarded action = CallStack.withSite expression.Span action

        match expression.Node with
        | Const value -> value
        | Local name ->
            match environment.TryFind name with
            | Some value -> value
            | None -> raiseEvaluation (sprintf "Local %s is not bound." name) expression.Span
        | VarRef symbol -> resolveVar runtime symbol currentNamespace
        | If(condition, whenTrue, whenFalse) ->
            if Truth.isTruthy (evaluate condition environment currentNamespace) then
                evaluate whenTrue environment currentNamespace
            else
                evaluate whenFalse environment currentNamespace
        | Do expressions ->
            let mutable result: obj = null
            for item in expressions do
                result <- evaluate item environment currentNamespace
            result
        | Let(name, value, body) ->
            let resolved = evaluate value environment currentNamespace
            evaluate body (environment.Add(name, resolved)) currentNamespace
        | Lambda(name, parameters, rest, body) ->
            let closure =
                Closure(
                    name,
                    parameters,
                    rest,
                    body,
                    environment,
                    currentNamespace,
                    fun body environment namespaceName -> evaluate body environment namespaceName
                )
            closure.Self <- closure
            closure :> obj
        | Call(callee, arguments) ->
            let calleeValue = evaluate callee environment currentNamespace
            let argumentValues = arguments |> List.map (fun argument -> evaluate argument environment currentNamespace) |> List.toArray
            guarded (fun () -> invoke calleeValue argumentValues)
        | Def(symbol, value) ->
            let resolved = evaluate value environment currentNamespace
            let namespaceName =
                match symbol.Namespace with
                | Some namespaceName ->
                    runtime.Registry.ResolveAlias(currentNamespace, namespaceName)
                    |> Option.defaultValue namespaceName
                | None -> currentNamespace
            let variable = runtime.Registry.Intern(namespaceName, symbol.Name, resolved, symbol.Meta)
            variable :> obj
        | Quote value -> value
        | SetNamespace(namespaceName, doc) ->
            if String.IsNullOrWhiteSpace namespaceName then
                raiseEvaluation "Namespace name cannot be empty." expression.Span
            runtime.CurrentNamespace <- namespaceName
            let namespaceValue = runtime.Registry.GetOrCreate namespaceName
            namespaceValue.Meta <-
                doc
                |> Option.map (fun text ->
                    PersistentMap.Empty.Assoc(Keyword.Intern "doc", box text) :> IPersistentMap)
            null
        | ImportType(alias, fullName) ->
            let namespaceValue = runtime.Registry.GetOrCreate runtime.CurrentNamespace
            namespaceValue.AddImport(alias, fullName)
            null
        | RequireNamespace(alias, namespaceName) ->
            let namespaceValue = runtime.Registry.GetOrCreate runtime.CurrentNamespace
            namespaceValue.AddAlias(alias, namespaceName)
            null
        | ReferVar(localName, namespaceName, varName) ->
            let namespaceValue = runtime.Registry.GetOrCreate runtime.CurrentNamespace
            namespaceValue.AddReferral(localName, namespaceName, varName)
            null
        | UseNamespace namespaceName ->
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
        | UseNamespaceOnly(namespaceName, names) ->
            let current = runtime.Registry.GetOrCreate runtime.CurrentNamespace
            for name in names do
                current.AddReferral(name, namespaceName, name)
            null
        | StaticCall(clrType, memberName, arguments) ->
            let argumentValues = arguments |> List.map (fun argument -> evaluate argument environment currentNamespace) |> List.toArray
            guarded (fun () -> Clr.invokeStatic clrType memberName argumentValues)
        | InstanceCall(target, memberName, arguments) ->
            let targetValue = evaluate target environment currentNamespace
            let argumentValues = arguments |> List.map (fun argument -> evaluate argument environment currentNamespace) |> List.toArray
            guarded (fun () -> RecordOps.invokeInstance runtime targetValue memberName argumentValues)
        | InstanceProperty(target, memberName) ->
            let targetValue = evaluate target environment currentNamespace
            guarded (fun () -> Clr.getInstanceProperty targetValue memberName)
        | Construct(clrType, arguments) ->
            let argumentValues = arguments |> List.map (fun argument -> evaluate argument environment currentNamespace) |> List.toArray
            guarded (fun () -> Clr.construct clrType argumentValues)
        | ConstructHint(hintForm, arguments) ->
            let clrType =
                match NamespaceResolution.resolveHint runtime hintForm with
                | Ok(Some resolved) -> resolved
                | Ok None ->
                    raiseEvaluation
                        (sprintf "Cannot resolve CLR type %s." (Print.printString hintForm))
                        expression.Span
                | Error errors ->
                    let notes =
                        errors |> List.map (fun error -> sprintf "%s: %s" error.Path error.Message) |> String.concat "; "
                    raiseEvaluation (sprintf "Cannot resolve CLR type %s: %s" (Print.printString hintForm) notes) expression.Span
            let argumentValues = arguments |> List.map (fun argument -> evaluate argument environment currentNamespace) |> List.toArray
            guarded (fun () -> Clr.construct clrType argumentValues)
        | TypeTestHint(hintForm, value) ->
            let clrType =
                match NamespaceResolution.resolveHint runtime hintForm with
                | Ok(Some resolved) -> resolved
                | Ok None ->
                    raiseEvaluation
                        (sprintf "Cannot resolve type %s." (Print.printString hintForm))
                        expression.Span
                | Error errors ->
                    let notes =
                        errors |> List.map (fun error -> sprintf "%s: %s" error.Path error.Message) |> String.concat "; "
                    raiseEvaluation
                        (sprintf "Cannot resolve type %s: %s" (Print.printString hintForm) notes)
                        expression.Span
            let targetValue = evaluate value environment currentNamespace
            box (clrType.IsInstanceOfType targetValue)
        | DefineType(name, fields, definition, isStruct) ->
            guarded (fun () -> RecordOps.defineType runtime name (fields |> Array.ofList) definition isStruct)
        | DefineInterface(definition) ->
            guarded (fun () -> RecordOps.defineInterface runtime definition)
        | DefineDelegate(definition) ->
            guarded (fun () -> RecordOps.defineDelegate runtime definition)
        | CreateDelegate(delegateType, callback) ->
            let callbackValue = evaluate callback environment currentNamespace
            guarded (fun () -> DelegateOps.Create(delegateType, callbackValue) :> obj)
        | Async(returnKind, body) ->
            let started = evaluate (AsyncTransform.start body) environment currentNamespace
            match returnKind with
            | AsyncTask -> started
            | AsyncValueTask -> AsyncOps.ToValueTask started
            | AsyncValueTaskOf resultType -> AsyncOps.ToValueTaskOf(started, resultType)
        | Await value ->
            let awaited = evaluate value environment currentNamespace
            guarded (fun () -> AsyncOps.AwaitSync awaited)
        | Declare symbols ->
            for symbol in symbols do
                VarOps.declare runtime symbol |> ignore
            null
        | SetVar(symbol, value) ->
            let resolved = evaluate value environment currentNamespace
            let namespaceName =
                match symbol.Namespace with
                | Some namespaceName ->
                    runtime.Registry.ResolveAlias(currentNamespace, namespaceName)
                    |> Option.defaultValue namespaceName
                | None -> currentNamespace
            match runtime.Registry.FindVar(namespaceName, symbol.Name) with
            | Some variable ->
                variable.Root <- resolved
                resolved
            | None ->
                raiseEvaluation
                    (sprintf "Cannot set unbound var %s." symbol.FullName)
                    expression.Span
        | DefineMethod(typeName, methodName, value, isPrivate, namespaceName) ->
            let implementation = evaluate value environment currentNamespace
            guarded (fun () ->
                RecordOps.defineMethod runtime typeName methodName implementation isPrivate namespaceName)
        | DefineConstructor(typeName, value, parameterHints, isPrivate, namespaceName) ->
            let implementation = evaluate value environment currentNamespace
            guarded (fun () ->
                RecordOps.defineConstructor runtime typeName implementation parameterHints isPrivate namespaceName)
        | Loop(bindings, body) ->
            let names = bindings |> List.map fst |> Array.ofList
            let mutable values =
                bindings
                |> List.map (fun (_, value) -> evaluate value environment currentNamespace)
                |> Array.ofList
            let mutable running = true
            let mutable result: obj = null
            while running do
                try
                    let mutable loopEnvironment = environment
                    for index in 0 .. names.Length - 1 do
                        loopEnvironment <- loopEnvironment.Add(names.[index], values.[index])
                    result <- evaluate body loopEnvironment currentNamespace
                    running <- false
                with :? RecurSignal as signal ->
                    if signal.Values.Length <> names.Length then
                        raiseEvaluation
                            (sprintf "recur expected %d arguments, got %d." names.Length signal.Values.Length)
                            expression.Span
                    values <- signal.Values
            result
        | Recur(arguments) ->
            let values =
                arguments
                |> List.map (fun argument -> evaluate argument environment currentNamespace)
                |> Array.ofList
            raise (RecurSignal values)
        | Try(tryBody, catches, finallyBody) ->
            let run () =
                try
                    evaluate tryBody environment currentNamespace
                with
                | :? RecurSignal -> reraise ()
                | error ->
                    let rec choose remaining =
                        match remaining with
                        | [] -> raise error
                        | clause :: rest ->
                            let matches =
                                match clause.ExceptionType with
                                | None -> true
                                | Some clrType -> clrType.IsInstanceOfType error
                            if matches then
                                evaluate clause.Body (environment.Add(clause.Binding, error)) currentNamespace
                            else
                                choose rest
                    choose catches
            match finallyBody with
            | Some finalBody ->
                try run ()
                finally evaluate finalBody environment currentNamespace |> ignore
            | None -> run ()
        | Throw(value) ->
            let thrown = evaluate value environment currentNamespace
            match thrown with
            | :? Exception as error ->
                CallStack.capture error expression.Span
                raise error
            | _ ->
                let message =
                    match thrown with
                    | :? string as text -> text
                    | _ -> Print.printString thrown
                raiseEvaluation message expression.Span

    let makeClosure
        (runtime: RuntimeState)
        (name: string option)
        (parameters: string list)
        (rest: string option)
        (body: CoreExpr)
        (environment: Env)
        (currentNamespace: string)
        =
        let closure =
            Closure(
                name,
                parameters,
                rest,
                body,
                environment,
                currentNamespace,
                fun body environment namespaceName -> eval runtime body environment namespaceName
            )
        closure.Self <- closure
        closure :> obj
