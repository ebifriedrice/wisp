namespace Wisp.Core

open System

/// Base class for callable values. The array entry point is the one the
/// interpreter and CLR call sites use; the arity overloads are conveniences.
[<AbstractClass>]
type AFn() =
    abstract Invoke: args: obj[] -> obj

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
    override _.Invoke(args) = implementation args
