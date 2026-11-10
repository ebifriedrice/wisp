namespace Wisp.Compiler

open Wisp.Core

module Optimize =
    let private truthy (value: obj) =
        match value with
        | null -> false
        | :? bool as flag -> flag
        | _ -> true

    let private isConstant (expression: CoreExpr) =
        match expression.Node with
        | Const _ -> true
        | _ -> false

    let rec optimize (expression: CoreExpr) =
        let rebuild node = Core.make expression.Span node
        match expression.Node with
        | Const _
        | Local _
        | VarRef _
        | Quote _
        | SetNamespace _
        | ImportType _
        | RequireNamespace _
        | ReferVar _
        | UseNamespace _
        | UseNamespaceOnly _ -> expression
        | If(condition, whenTrue, whenFalse) ->
            let condition = optimize condition
            let whenTrue = optimize whenTrue
            let whenFalse = optimize whenFalse
            match condition.Node with
            | Const value -> if truthy value then whenTrue else whenFalse
            | _ -> rebuild (If(condition, whenTrue, whenFalse))
        | Do expressions ->
            let optimized = expressions |> List.map optimize
            let flattened =
                optimized
                |> List.collect (fun item ->
                    match item.Node with
                    | Do nested -> nested
                    | _ -> [ item ])
            match flattened with
            | [] -> Core.nil expression.Span
            | [ single ] -> single
            | items -> rebuild (Do items)
        | Let(name, value, body) ->
            let value = optimize value
            let body = optimize body
            match body.Node with
            | Local localName when localName = name -> value
            | _ -> rebuild (Let(name, value, body))
        | Lambda(name, parameters, rest, body) -> rebuild (Lambda(name, parameters, rest, optimize body))
        | Call(callee, arguments) -> rebuild (Call(optimize callee, arguments |> List.map optimize))
        | Def(symbol, value) -> rebuild (Def(symbol, optimize value))
        | StaticCall(clrType, memberName, arguments) ->
            rebuild (StaticCall(clrType, memberName, arguments |> List.map optimize))
        | InstanceCall(target, memberName, arguments) ->
            rebuild (InstanceCall(optimize target, memberName, arguments |> List.map optimize))
        | InstanceProperty(target, memberName) -> rebuild (InstanceProperty(optimize target, memberName))
        | Construct(clrType, arguments) -> rebuild (Construct(clrType, arguments |> List.map optimize))
        | ConstructHint(hintForm, arguments) ->
            rebuild (ConstructHint(hintForm, arguments |> List.map optimize))
        | TypeTestHint(hintForm, value) -> rebuild (TypeTestHint(hintForm, optimize value))
        | DefineType _ -> expression
        | DefineInterface _ -> expression
        | DefineDelegate _ -> expression
        | CreateDelegate(delegateType, callback) ->
            rebuild (CreateDelegate(delegateType, optimize callback))
        | SetVar(symbol, value) -> rebuild (SetVar(symbol, optimize value))
        | Async(returnKind, body) -> rebuild (Async(returnKind, optimize body))
        | Await value -> rebuild (Await(optimize value))
        | DefineMethod(typeName, methodName, value, isPrivate, namespaceName) ->
            rebuild (DefineMethod(typeName, methodName, optimize value, isPrivate, namespaceName))
        | DefineConstructor(typeName, value, parameterHints, isPrivate, namespaceName) ->
            rebuild (DefineConstructor(typeName, optimize value, parameterHints, isPrivate, namespaceName))
        | Loop(bindings, body) ->
            rebuild (
                Loop(
                    bindings |> List.map (fun (name, value) -> name, optimize value),
                    optimize body
                )
            )
        | Recur(arguments) -> rebuild (Recur(arguments |> List.map optimize))
        | Try(tryBody, catches, finallyBody) ->
            rebuild (
                Try(
                    optimize tryBody,
                    catches
                    |> List.map (fun clause -> { clause with Body = optimize clause.Body }),
                    finallyBody |> Option.map optimize
                )
            )
        | Throw(value) -> rebuild (Throw(optimize value))
