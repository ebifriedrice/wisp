namespace Wisp.Compiler

open Wisp.Core

/// Tail-position validation for recur after desugaring. The emitter uses
/// exceptions for recursion, so without this check a non-tail recur would
/// silently unwind past surrounding work.
module Validate =
    let validate (expression: CoreExpr) : Result<CoreExpr, CompilerError list> =
        let invalid (expression: CoreExpr) (message: string) : Result<unit, CompilerError list> =
            Error [ CompilerError.create "WISP3026" message expression.Span ]

        let traverse (items: (unit -> Result<unit, CompilerError list>) list) : Result<unit, CompilerError list> =
            let rec loop remaining =
                match remaining with
                | [] -> Ok ()
                | item :: rest ->
                    match item () with
                    | Ok () -> loop rest
                    | Error errors -> Error errors
            loop items

        let rec walk (target: RecurTarget option) (tail: bool) (expression: CoreExpr) : Result<unit, CompilerError list> =
            match expression.Node with
            | Recur(arguments) ->
                match target with
                | None -> invalid expression "recur requires an enclosing loop or function."
                | Some recurTarget ->
                    let expected = recurTarget.Arity + (if recurTarget.HasRest then 1 else 0)
                    if not tail then
                        invalid expression "recur is only valid in tail position."
                    elif arguments.Length <> expected then
                        invalid expression (sprintf "recur expects %d arguments for this target, got %d." expected arguments.Length)
                    else
                        arguments
                        |> List.map (fun argument -> fun () -> walk target false argument)
                        |> traverse
            | Loop(bindings, body) ->
                let bindingValues =
                    bindings
                    |> List.map (fun (_, value) -> fun () -> walk target false value)
                match traverse bindingValues with
                | Error errors -> Error errors
                | Ok () ->
                    walk
                        (Some { Arity = bindings.Length; HasRest = false })
                        true
                        body
            | If(condition, whenTrue, whenFalse) ->
                match walk target false condition with
                | Error errors -> Error errors
                | Ok () ->
                    match walk target tail whenTrue with
                    | Error errors -> Error errors
                    | Ok () -> walk target tail whenFalse
            | Do(expressions) ->
                let count = expressions.Length
                expressions
                |> List.mapi (fun index item ->
                    fun () ->
                        let isLast = index = count - 1
                        walk target (if isLast then tail else false) item)
                |> traverse
            | Let(_, value, body) ->
                match walk target false value with
                | Error errors -> Error errors
                | Ok () -> walk target tail body
            | Lambda(_, parameters, rest, body) ->
                walk
                    (Some { Arity = parameters.Length; HasRest = rest.IsSome })
                    true
                    body
            | Try(tryBody, catches, finallyBody) ->
                match walk None false tryBody with
                | Error errors -> Error errors
                | Ok () ->
                    let catchCheck =
                        catches
                        |> List.map (fun clause -> fun () -> walk None false clause.Body)
                        |> traverse
                    match catchCheck with
                    | Error errors -> Error errors
                    | Ok () ->
                        match finallyBody with
                        | Some body -> walk None false body
                        | None -> Ok ()
            | Call(callee, arguments) ->
                (fun () -> walk target false callee)
                :: (arguments |> List.map (fun argument -> fun () -> walk target false argument))
                |> traverse
            | StaticCall(_, _, arguments) ->
                arguments
                |> List.map (fun argument -> fun () -> walk target false argument)
                |> traverse
            | InstanceCall(targetExpression, _, arguments) ->
                (fun () -> walk target false targetExpression)
                :: (arguments |> List.map (fun argument -> fun () -> walk target false argument))
                |> traverse
            | InstanceProperty(targetExpression, _) -> walk target false targetExpression
            | Construct(_, arguments)
            | ConstructHint(_, arguments) ->
                arguments
                |> List.map (fun argument -> fun () -> walk target false argument)
                |> traverse
            | TypeTestHint(_, value) -> walk target false value
            | CreateDelegate(_, callback) -> walk None false callback
            | SetVar(_, value) -> walk None false value
            | Declare _ -> Ok ()
            | Async(_, body) -> walk None false body
            | Await value -> walk target false value
            | DefineMethod(_, _, value, _, _) -> walk None false value
            | DefineConstructor(_, value, _, _, _) -> walk None false value
            | Def(_, value) -> walk target false value
            | Throw(value) -> walk target false value
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
            | DefineDelegate _ -> Ok ()

        match walk None true expression with
        | Ok () -> Ok expression
        | Error errors -> Error errors
