namespace Wisp.Tests

open System
open Xunit
open Wisp.Core

type FunctionTests() =
    [<Fact>]
    member _.``AFn packs convenience arities into the array entry point``() =
        let functionValue = Fn(fun args -> box args.Length)
        Assert.Equal(box 0, functionValue.Invoke())
        Assert.Equal(box 1, functionValue.Invoke(box "a"))
        Assert.Equal(box 2, functionValue.Invoke(box 1, box 2))
        let callable = functionValue :> IFn
        Assert.Equal(box 3, callable.Invoke([| box 1; box 2; box 3 |]))

    [<Fact>]
    member _.``keywords are callable map lookups``() =
        let map =
            PersistentMap.Empty
                .Assoc(Keyword.Intern "name", box "Wisp")
                .Assoc(Keyword.Intern "version", box 0)
        let keyword = Keyword.Intern "name" :> IFn
        Assert.Equal(box "Wisp", keyword.Invoke([| map |]))
        Assert.Null(keyword.Invoke([| PersistentMap.Empty |]))
        Assert.Equal(box "fallback", keyword.Invoke([| PersistentMap.Empty; box "fallback" |]))

    [<Fact>]
    member _.``keyword interning and namespaces are stable``() =
        let first = Keyword.Intern("my.app", "thing")
        let second = Keyword.Intern(":my.app/thing")
        Assert.Same(first, second)
        Assert.Equal(Some "my.app", first.Namespace)
        Assert.Equal("thing", first.Name)
        Assert.Equal(":my.app/thing", first.ToString())

    [<Fact>]
    member _.``symbols compare by namespace and name and ignore metadata``() =
        let metadata = PersistentMap.Empty.Assoc(Keyword.Intern "tag", box "String")
        let left = (Symbol.Create("ns", "name") :> IObj).WithMeta(Some metadata) :?> Symbol
        let right = Symbol.Create("ns", "name")
        Assert.Equal(right, left)
        Assert.Equal(right.GetHashCode(), left.GetHashCode())

    [<Fact>]
    member _.``invalid call arity raises ArityException``() =
        let map = PersistentMap.Empty :> IFn
        Assert.Throws<ArityException>(fun () -> map.Invoke([||]) |> ignore) |> ignore
