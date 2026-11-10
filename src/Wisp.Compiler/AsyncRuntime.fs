namespace Wisp.Compiler

open System
open System.Collections.Concurrent
open System.Reflection
open System.Threading.Tasks
open Wisp.Core

/// Runtime helpers shared by the async compiler paths. The CPS transform used
/// on .NET 10 and by the interpreter builds on Pure/Bind; .NET 11 also uses
/// AsTask with AsyncHelpers.Await from emitted runtime-async methods.
[<AbstractClass; Sealed>]
type AsyncOps private () =
    static member AsTask(value: obj) : Task<obj> =
        match value with
        | :? Task<obj> as task -> task
        | :? Task as task ->
            task.ContinueWith(fun (completed: Task) ->
                if completed.IsCanceled then
                    raise (TaskCanceledException completed)
                elif completed.IsFaulted then
                    raise (completed.Exception.GetBaseException())
                else
                    let resultProperty = completed.GetType().GetProperty("Result")
                    if isNull resultProperty then
                        null
                    else
                        let result = resultProperty.GetValue completed
                        if not (isNull result)
                           && result.GetType().FullName = "System.Threading.Tasks.VoidTaskResult" then
                            null
                        else
                            result)
        | _ when not (isNull value) ->
            let valueType = value.GetType()
            let asTask = valueType.GetMethod("AsTask", Type.EmptyTypes)
            if isNull asTask then
                raise (ArgumentException(sprintf "Cannot await %s." (Print.printString value)))
            let task = AsyncOps.AsTask(asTask.Invoke(value, [||]))
            if valueType = typeof<ValueTask> then
                task.ContinueWith(fun (completed: Task<obj>) ->
                    if completed.IsCanceled then
                        raise (TaskCanceledException completed)
                    elif completed.IsFaulted then
                        raise (completed.Exception.GetBaseException())
                    else
                        null)
            else
                task
        | _ -> raise (ArgumentNullException("value", "Cannot await nil."))

    static member Start(thunk: obj) : obj =
        try
            (thunk :?> IFn).Invoke [||] |> AsyncOps.AsTask :> obj
        with error ->
            Task.FromException<obj>(error) :> obj

    static member Pure(value: obj) : obj = Task.FromResult<obj>(value) :> obj

    static member ToValueTask(task: obj) : obj =
        ValueTask(AsyncOps.AsTask task) :> obj

    static member private ValueTaskOf<'T>(task: Task<obj>) : ValueTask<'T> =
        let converted =
            task.ContinueWith(fun (completed: Task<obj>) ->
                if completed.IsCanceled then
                    raise (TaskCanceledException completed)
                elif completed.IsFaulted then
                    raise (completed.Exception.GetBaseException())
                else
                    match Clr.tryConvertValue completed.Result typeof<'T> with
                    | Some value -> unbox<'T> value
                    | None ->
                        raise (
                            InvalidCastException(
                                sprintf "Cannot convert async result %s to %O." (Print.printString completed.Result) typeof<'T>
                            )
                        ))
        ValueTask<'T>(converted)

    static member ToValueTaskOf(task: obj, resultType: Type) : obj =
        let generic =
            typeof<AsyncOps>
                .GetMethod("ValueTaskOf", BindingFlags.NonPublic ||| BindingFlags.Static)
                .MakeGenericMethod(resultType)
        generic.Invoke(null, [| AsyncOps.AsTask task :> obj |])

    static member Bind(task: obj, continuation: obj) : obj =
        let continuation = continuation :?> IFn
        AsyncOps
            .AsTask(task)
            .ContinueWith(fun (completed: Task<obj>) ->
                if completed.IsCanceled then
                    raise (TaskCanceledException completed)
                elif completed.IsFaulted then
                    raise (completed.Exception.GetBaseException())
                else
                    AsyncOps.AsTask(continuation.Invoke [| completed.Result |]))
            .Unwrap()
        :> obj

    static member AwaitSync(value: obj) : obj =
        AsyncOps.AsTask(value).GetAwaiter().GetResult()

    static member Catch(task: obj, handler: obj) : obj =
        let handler = handler :?> IFn
        AsyncOps
            .AsTask(task)
            .ContinueWith(fun (completed: Task<obj>) ->
                if completed.IsCanceled then
                    raise (TaskCanceledException completed)
                elif completed.IsFaulted then
                    AsyncOps.AsTask(handler.Invoke [| completed.Exception.GetBaseException() |])
                else
                    completed)
            .Unwrap()
        :> obj

    static member Finally(task: obj, action: obj) : obj =
        let action = action :?> IFn
        AsyncOps
            .AsTask(task)
            .ContinueWith(fun (completed: Task<obj>) ->
                action.Invoke [||] |> ignore
                if completed.IsCanceled then
                    raise (TaskCanceledException completed)
                elif completed.IsFaulted then
                    raise (completed.Exception.GetBaseException())
                else
                    completed.Result)
        :> obj

    static member IsInstance(value: obj, clrType: Type) : obj =
        box (not (isNull value) && clrType.IsInstanceOfType value)

module AsyncSupport =
    let private asyncHelpersType =
        typeof<Task>.Assembly.GetType("System.Runtime.CompilerServices.AsyncHelpers")

    let private awaitTaskOf =
        if isNull asyncHelpersType then
            None
        else
            asyncHelpersType.GetMethods(BindingFlags.Public ||| BindingFlags.Static)
            |> Array.tryFind (fun methodInfo ->
                let parameters = methodInfo.GetParameters()
                methodInfo.Name = "Await"
                && methodInfo.IsGenericMethodDefinition
                && parameters.Length = 1
                && parameters.[0].ParameterType.IsGenericType
                && parameters.[0].ParameterType.GetGenericTypeDefinition() = typedefof<Task<_>>)

    let runtimeAsyncAvailable = Environment.Version.Major >= 11 && awaitTaskOf.IsSome

    let awaitTaskOfObject =
        awaitTaskOf |> Option.map (fun methodInfo -> methodInfo.MakeGenericMethod typeof<obj>)
