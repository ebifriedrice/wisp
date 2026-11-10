namespace Wisp.Core

open System

/// Base class for callable values. The array entry point is the one the
/// interpreter and CLR call sites use; the arity overloads are conveniences.
/// Named functions record a stack frame around every invocation so errors can
/// report where they originated.
[<AbstractClass>]
type AFn() =
    abstract InvokeCore: args: obj[] -> obj

    /// Name used by Wisp stack traces. Null for unnamed built-ins, which stay
    /// transparent so callbacks they invoke inherit the built-in's call site.
    abstract Name: string
    default _.Name = null

    member this.Invoke(args: obj[]) =
        let name = this.Name
        if String.IsNullOrEmpty name then
            let site = CallStack.peekSite ()
            try
                this.InvokeCore args
            with error ->
                CallStack.capture error site
                reraise ()
        else
            let span = CallStack.takeSite ()
            let previous = CallStack.push name span
            try
                try
                    this.InvokeCore args
                with error ->
                    CallStack.capture error span
                    reraise ()
            finally
                CallStack.restore previous

    member this.Invoke() = this.Invoke(Array.empty<obj>)
    member this.Invoke(a: obj) = this.Invoke([| a |])
    member this.Invoke(a: obj, b: obj) = this.Invoke([| a; b |])
    member this.Invoke(a: obj, b: obj, c: obj) = this.Invoke([| a; b; c |])
    member this.Invoke(a: obj, b: obj, c: obj, d: obj) = this.Invoke([| a; b; c; d |])

    interface IFn with
        member this.Invoke(args) = this.Invoke(args)

/// Adapts an F# function to the IFn protocol without declaring a type per call site.
[<Sealed>]
type Fn(implementation: obj[] -> obj) =
    inherit AFn()
    override _.InvokeCore(args) = implementation args
