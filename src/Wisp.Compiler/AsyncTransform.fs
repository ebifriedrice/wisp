namespace Wisp.Compiler

open System
open System.Threading
open Wisp.Core

/// Continuation-passing transform used for .NET 10 codegen and for the
/// interpreter. Each async body becomes a chain of Task continuations so await
/// works without runtime-async support.
module AsyncTransform =
    let private counter = ref 0

    let private gensym () =
        let value = Interlocked.Increment counter
        sprintf "__wisp_async_%d" value

    let private call (name: string) (arguments: CoreExpr list) (span: TextSpan option) =
        Core.make span (StaticCall(typeof<AsyncOps>, name, arguments))

    let private pureTask (value: CoreExpr) = call "Pure" [ value ] value.Span

    let private bind (task: CoreExpr) (continuation: CoreExpr) =
        call "Bind" [ task; continuation ] task.Span

    let private defer (expression: CoreExpr) =
        let thunk = Core.make expression.Span (Lambda(None, [], None, expression))
        call "Start" [ thunk ] expression.Span

    let rec containsAwait (expression: CoreExpr) =
        match expression.Node with
        | Await _ -> true
        | Const _
        | Local _
        | VarRef _
        | Quote _
        | SetNamespace _
        | ImportType _
        | RequireNamespace _
        | ReferVar _
        | UseNamespace _
        | UseNamespaceOnly _
        | DefineType _
        | DefineInterface _
        | DefineDelegate _
        | Async(_, _)
        | Lambda _ -> false
        | If(condition, whenTrue, whenFalse) ->
            containsAwait condition || containsAwait whenTrue || containsAwait whenFalse
        | Do expressions -> expressions |> List.exists containsAwait
        | Let(_, value, body) -> containsAwait value || containsAwait body
        | Call(callee, arguments) ->
            containsAwait callee || arguments |> List.exists containsAwait
        | Def(_, value)
        | CreateDelegate(_, value)
        | SetVar(_, value)
        | TypeTestHint(_, value)
        | Throw value -> containsAwait value
        | StaticCall(_, _, arguments)
        | Construct(_, arguments)
        | ConstructHint(_, arguments) -> arguments |> List.exists containsAwait
        | InstanceCall(target, _, arguments) ->
            containsAwait target || arguments |> List.exists containsAwait
        | InstanceProperty(target, _) -> containsAwait target
        | DefineMethod(_, _, value, _, _)
        | DefineConstructor(_, value, _, _, _) -> containsAwait value
        | Loop(bindings, body) ->
            bindings |> List.exists (fun (_, value) -> containsAwait value) || containsAwait body
        | Recur(arguments) -> arguments |> List.exists containsAwait
        | Try(tryBody, catches, finallyBody) ->
            containsAwait tryBody
            || catches |> List.exists (fun clause -> containsAwait clause.Body)
            || finallyBody |> Option.exists containsAwait

    let lower (body: CoreExpr) : CoreExpr =
        let rec lowerExpression (expression: CoreExpr) (resume: CoreExpr -> CoreExpr) : CoreExpr =
            let span = expression.Span
            let make node = Core.make span node
            let resultName () = gensym ()
            let local name = Core.make span (Local name)

            let bindValue (value: CoreExpr) (resume: CoreExpr -> CoreExpr) =
                let name = resultName ()
                make (Let(name, value, resume (local name)))

            let lowerMany (expressions: CoreExpr list) (build: CoreExpr list -> CoreExpr) =
                let rec go accumulated remaining =
                    match remaining with
                    | [] -> build (List.rev accumulated)
                    | current :: rest ->
                        if containsAwait current then
                            lowerExpression current (fun value ->
                                let name = resultName ()
                                make (Let(name, value, go (local name :: accumulated) rest)))
                        elif rest |> List.exists containsAwait then
                            let name = resultName ()
                            make (Let(name, current, go (local name :: accumulated) rest))
                        else
                            go (current :: accumulated) rest
                go [] expressions

            let rec lowerDo expressions =
                match expressions with
                | [] -> resume (Core.nil span)
                | [ last ] -> lowerExpression last resume
                | current :: rest ->
                    if containsAwait current then
                        lowerExpression current (fun _ -> lowerDo rest)
                    else
                        make (Do [ current; lowerDo rest ])

            let catchHandler (catches: CatchClause list) =
                let exceptionName = gensym ()
                let rec dispatch clauses =
                    match clauses with
                    | [] -> make (Throw(local exceptionName))
                    | clause :: rest ->
                        let catchResult = lowerExpression clause.Body pureTask
                        let body =
                            Core.make
                                clause.Body.Span
                                (Let(clause.Binding, local exceptionName, catchResult))
                        match clause.ExceptionType with
                        | None -> body
                        | Some clrType ->
                            let test =
                                call
                                    "IsInstance"
                                    [ local exceptionName; Core.make span (Const(box clrType)) ]
                                    span
                            make (If(test, body, dispatch rest))
                make (Lambda(None, [ exceptionName ], None, dispatch catches))

            if not (containsAwait expression) then
                let name = resultName ()
                make (Let(name, expression, resume (local name)))
            else
                match expression.Node with
                | Await taskExpression ->
                    lowerExpression taskExpression (fun task ->
                        let name = resultName ()
                        let continuation =
                            make (Lambda(None, [ name ], None, resume (local name)))
                        bind task continuation)
                | Do expressions -> lowerDo expressions
                | Let(name, value, body) ->
                    if containsAwait value then
                        lowerExpression value (fun resolved ->
                            make (Let(name, resolved, lowerExpression body resume)))
                    else
                        make (Let(name, value, lowerExpression body resume))
                | If(condition, whenTrue, whenFalse) ->
                    if containsAwait condition then
                        lowerExpression condition (fun resolved ->
                            make (
                                If(
                                    resolved,
                                    lowerExpression whenTrue resume,
                                    lowerExpression whenFalse resume
                                )
                            ))
                    else
                        make (
                            If(
                                condition,
                                lowerExpression whenTrue resume,
                                lowerExpression whenFalse resume
                            )
                        )
                | Call(callee, arguments) ->
                    lowerMany (callee :: arguments) (fun values ->
                        match values with
                        | resolvedCallee :: resolvedArguments ->
                            bindValue (make (Call(resolvedCallee, resolvedArguments))) resume
                        | [] -> failwith "unreachable")
                | StaticCall(clrType, memberName, arguments) ->
                    lowerMany arguments (fun resolved ->
                        bindValue (make (StaticCall(clrType, memberName, resolved))) resume)
                | InstanceCall(target, memberName, arguments) ->
                    lowerMany (target :: arguments) (fun values ->
                        match values with
                        | resolvedTarget :: resolvedArguments ->
                            bindValue (
                                make (InstanceCall(resolvedTarget, memberName, resolvedArguments))
                            ) resume
                        | [] -> failwith "unreachable")
                | InstanceProperty(target, memberName) ->
                    lowerExpression target (fun resolved ->
                        bindValue (make (InstanceProperty(resolved, memberName))) resume)
                | Construct(clrType, arguments) ->
                    lowerMany arguments (fun resolved ->
                        bindValue (make (Construct(clrType, resolved))) resume)
                | ConstructHint(hintForm, arguments) ->
                    lowerMany arguments (fun resolved ->
                        bindValue (make (ConstructHint(hintForm, resolved))) resume)
                | TypeTestHint(hintForm, value) ->
                    lowerExpression value (fun resolved ->
                        bindValue (make (TypeTestHint(hintForm, resolved))) resume)
                | CreateDelegate(delegateType, callback) ->
                    lowerExpression callback (fun resolved ->
                        bindValue (make (CreateDelegate(delegateType, resolved))) resume)
                | SetVar(symbol, value) ->
                    lowerExpression value (fun resolved ->
                        bindValue (make (SetVar(symbol, resolved))) resume)
                | Def(symbol, value) ->
                    lowerExpression value (fun resolved ->
                        bindValue (make (Def(symbol, resolved))) resume)
                | DefineMethod(typeName, methodName, value, isPrivate, namespaceName) ->
                    lowerExpression value (fun resolved ->
                        bindValue (
                            make (DefineMethod(typeName, methodName, resolved, isPrivate, namespaceName))
                        ) resume)
                | DefineConstructor(typeName, value, parameterHints, isPrivate, namespaceName) ->
                    lowerExpression value (fun resolved ->
                        bindValue (
                            make (
                                DefineConstructor(
                                    typeName,
                                    resolved,
                                    parameterHints,
                                    isPrivate,
                                    namespaceName
                                )
                            )
                        ) resume)
                | Throw value ->
                    lowerExpression value (fun resolved -> make (Throw resolved))
                | Try(tryBody, catches, finallyBody) ->
                    let tryTask = defer (lowerExpression tryBody pureTask)
                    let caught =
                        match catches with
                        | [] -> tryTask
                        | _ -> call "Catch" [ tryTask; catchHandler catches ] span
                    let completed =
                        match finallyBody with
                        | Some finalBody ->
                            if containsAwait finalBody then
                                raise (
                                    NotSupportedException(
                                        "await in a finally block is not supported by the .NET 10 async transform."
                                    )
                                )
                            call "Finally" [ caught; make (Lambda(None, [], None, finalBody)) ] span
                        | None -> caught
                    let name = resultName ()
                    bind completed (make (Lambda(None, [ name ], None, resume (local name))))
                | Loop _ ->
                    raise (
                        NotSupportedException(
                            "await inside loop/recur is not supported by the .NET 10 async transform."
                        )
                    )
                | Recur _ ->
                    raise (
                        NotSupportedException(
                            "await inside recur is not supported by the .NET 10 async transform."
                        )
                    )
                | Const _
                | Local _
                | VarRef _
                | Quote _
                | SetNamespace _
                | ImportType _
                | RequireNamespace _
                | ReferVar _
                | UseNamespace _
                | UseNamespaceOnly _
                | DefineType _
                | DefineInterface _
                | DefineDelegate _
                | Async(_, _)
                | Lambda _ ->
                    let name = resultName ()
                    make (Let(name, expression, resume (local name)))

        lowerExpression body (fun value -> pureTask value)

    let start (body: CoreExpr) : CoreExpr =
        let lowered = lower body
        let thunk = Core.make body.Span (Lambda(None, [], None, lowered))
        call "Start" [ thunk ] body.Span
