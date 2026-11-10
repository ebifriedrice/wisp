namespace Wisp.Tests

open System
open System.Numerics
open Xunit
open Wisp.Core

type CollectionTests() =
    [<Fact>]
    member _.``list cons pop and equality ignore metadata``() =
        let list = PersistentList.Empty.Cons(box 1).Cons(box 2)
        Assert.Equal(2, list.Count)
        Assert.Equal(box 2, list.First)
        Assert.Equal(box 1, list.Pop().First)
        Assert.Equal<obj list>([ box 2; box 1 ], list.Items |> Seq.toList)
        let metadata = PersistentMap.Empty.Assoc(Keyword.Intern "source", box "test")
        let withMetadata = (list :> IObj).WithMeta(Some metadata) :?> PersistentList
        match withMetadata.Meta with
        | Some stored -> Assert.Same(metadata, stored)
        | None -> failwith "Expected metadata."
        Assert.True(list.Equals withMetadata)
        Assert.Equal(box 3, withMetadata.Cons(box 3).First)

    [<Fact>]
    member _.``vector supports trie growth indexing updates and popping``() =
        let mutable vector = PersistentVector.Empty
        for index in 0 .. 2999 do
            vector <- vector.Add(box index)
        Assert.Equal(3000, vector.Count)
        for index in 0 .. 2999 do
            Assert.Equal(box index, vector.Nth index)
        Assert.Equal<obj list>([ box 0; box 1; box 2 ], vector.Elements |> Seq.truncate 3 |> Seq.toList)
        let updated = vector.AssocN(1500, box -1)
        Assert.Equal(box -1, updated.Nth 1500)
        Assert.Equal(box 1500, vector.Nth 1500)
        let appended = vector.AssocN(3000, box 3000)
        Assert.Equal(3001, appended.Count)
        Assert.Equal(box 3000, appended.Nth 3000)
        let mutable popped = vector
        for index in 2999 .. -1 .. 0 do
            Assert.Equal(index + 1, popped.Count)
            Assert.Equal(box index, popped.Nth index)
            popped <- popped.Pop()
        Assert.True(popped.IsEmpty)
        Assert.Equal(0, popped.Count)

    [<Fact>]
    member _.``vector equality hashing and ifn lookup are structural``() =
        let left = PersistentVector.OfSeq [ box 1; box 2; box 3 ]
        let right = PersistentVector.OfSeq [ box 1; box 2; box 3 ]
        Assert.True(left.Equals right)
        Assert.Equal(left.GetHashCode(), right.GetHashCode())
        let callable = left :> IFn
        Assert.Equal(box 2, callable.Invoke([| box 1 |]))
        Assert.Equal(box 99, callable.Invoke([| box 9; box 99 |]))

    [<Fact>]
    member _.``map handles nils lookup removal equality and ifn``() =
        let map =
            PersistentMap.Empty
                .Assoc(Keyword.Intern "a", box 1)
                .Assoc(Keyword.Intern "b", box 2)
                .Assoc(null, box 3)
        Assert.Equal(3, map.Count)
        Assert.Equal(box 1, map.ValAt(Keyword.Intern "a"))
        Assert.Equal(box 3, map.ValAt null)
        Assert.Equal(Some(box 2), map.TryGet(Keyword.Intern "b"))
        Assert.Equal(None, map.TryGet(Keyword.Intern "missing"))
        let removed = map.Without(Keyword.Intern "a")
        Assert.Equal(2, removed.Count)
        Assert.False(removed.ContainsKey(Keyword.Intern "a"))
        let reordered =
            PersistentMap.Empty
                .Assoc(null, box 3)
                .Assoc(Keyword.Intern "b", box 2)
                .Assoc(Keyword.Intern "a", box 1)
        Assert.True(map.Equals reordered)
        Assert.Equal(map.GetHashCode(), reordered.GetHashCode())
        let callable = map :> IFn
        Assert.Equal(box 2, callable.Invoke([| Keyword.Intern "b" |]))
        Assert.Equal(box 42, callable.Invoke([| Keyword.Intern "missing"; box 42 |]))

    [<Fact>]
    member _.``map metadata survives association``() =
        let metadata = PersistentMap.Empty.Assoc(Keyword.Intern "line", box 7)
        let map = (PersistentMap.Empty :> IObj).WithMeta(Some metadata) :?> PersistentMap
        let associated = map.Assoc(Keyword.Intern "a", box 1)
        match associated.Meta with
        | Some stored -> Assert.Same(metadata, stored)
        | None -> failwith "Expected metadata."

    [<Fact>]
    member _.``set handles nils duplicates ifn and equality``() =
        let set = PersistentSet.OfSeq [ box 1; box 2; box 2; null ]
        Assert.Equal(3, set.Count)
        Assert.True(set.Contains null)
        Assert.True(set.Contains(box 1))
        Assert.False(set.Contains(box 3))
        let callable = set :> IFn
        Assert.Equal(box 1, callable.Invoke([| box 1 |]))
        Assert.Null(callable.Invoke([| box 3 |]))
        Assert.Equal(box 9, callable.Invoke([| box 3; box 9 |]))
        let reordered = PersistentSet.OfSeq [ null; box 2; box 1 ]
        Assert.True(set.Equals reordered)
        Assert.Equal(set.GetHashCode(), reordered.GetHashCode())

    [<Fact>]
    member _.``lazy seq realizes once and supports infinite tails``() =
        let mutable realizations = 0
        let delayed =
            LazySeq(fun () ->
                realizations <- realizations + 1
                Some(PersistentList.OfSeq [ box 42 ] :> ISeq))
        let sequence = delayed :> ISeq
        Assert.Equal(0, realizations)
        Assert.False(sequence.IsEmpty)
        Assert.Equal(1, realizations)
        Assert.Equal(box 42, sequence.First)
        Assert.Equal(1, realizations)

        let rec naturals value : ISeq =
            Cons(box value, LazySeq(fun () -> Some(naturals (value + 1))))
        let firstFive =
            Seqs.toSeq (Some(naturals 0))
            |> Seq.take 5
            |> Seq.toList
        Assert.Equal<obj list>([ box 0; box 1; box 2; box 3; box 4 ], firstFive)

    [<Fact>]
    member _.``seq helpers traverse vectors maps and sets``() =
        let vector = PersistentVector.OfSeq [ box 1; box 2; box 3 ]
        Assert.Equal<obj list>([ box 1; box 2; box 3 ], Seqs.toList vector)
        Assert.Null(Seqs.ofSeqable null)
        Assert.True((Seqs.ofSeqable PersistentVector.Empty).IsNone)

    [<Fact>]
    member _.``namespace registry resolves aliases``() =
        let registry = NamespaceRegistry()
        let namespaceValue = registry.GetOrCreate "My.App"
        namespaceValue.AddAlias("core", "My.App.Core")
        Assert.Equal(Some "My.App.Core", registry.ResolveAlias("My.App", "core"))
        Assert.Equal(None, registry.ResolveAlias("My.App", "missing"))

    [<Fact>]
    member _.``printer renders language values and functions``() =
        Assert.Equal("nil", Print.printString null)
        Assert.Equal("\"a\\n\"", Print.printString "a\n")
        Assert.Equal(":a", Print.printString(Keyword.Intern "a"))
        Assert.Equal("[1 2]", Print.printString(PersistentVector.OfSeq [ box 1L; box 2L ]))
        Assert.Equal("(1 2)", Print.printString(PersistentList.OfSeq [ box 1L; box 2L ]))
        Assert.Equal("#<fn>", Print.printString(Fn(fun _ -> null)))
        Assert.Equal("a1", Print.displayString "a" + Print.displayString(box 1L))

    [<Fact>]
    member _.``ratio normalizes sign and common factors``() =
        let ratio = Ratio.Create(BigInteger(2), BigInteger(-4))
        Assert.Equal(BigInteger(-1), ratio.Numerator)
        Assert.Equal(BigInteger(2), ratio.Denominator)
        Assert.Equal(Ratio.Create(BigInteger(-1), BigInteger 2), ratio)
