namespace Wisp.Core

open System
open System.Collections
open System.Collections.Generic
open System.Collections.Immutable

/// Persistent singly-linked list. Empty is a singleton; every other cell owns
/// its first element and tail.
[<Sealed>]
type PersistentList private (count: int, first: obj, rest: PersistentList, metadata: IPersistentMap option) =
    static let empty = PersistentList(0, null, Unchecked.defaultof<PersistentList>, None)

    static member Empty = empty

    member _.Count = count
    member _.IsEmpty = count = 0
    member _.Meta = metadata

    member _.First =
        if count = 0 then raise (InvalidOperationException("Cannot take the first element of an empty list."))
        first

    member _.Rest =
        if count = 0 then raise (InvalidOperationException("Cannot take the rest of an empty list."))
        rest

    member this.Cons(value: obj) = PersistentList(count + 1, value, this, metadata)

    member this.Pop() =
        if count = 0 then raise (InvalidOperationException("Cannot pop an empty list."))
        rest

    member this.WithMeta(metadata: IPersistentMap option) =
        PersistentList(count, first, rest, metadata)

    member this.Items =
        seq {
            let mutable current = this
            while not current.IsEmpty do
                yield current.First
                current <- current.Rest
        }

    static member OfSeq(items: seq<obj>) =
        Seq.foldBack (fun (item: obj) (acc: PersistentList) -> acc.Cons item) items empty

    override this.Equals(other: obj) =
        match other with
        | :? PersistentList as that ->
            count = that.Count && Seq.forall2 (fun a b -> Object.Equals(a, b)) this.Items that.Items
        | _ -> false

    override this.GetHashCode() =
        let mutable hash = HashCode()
        hash.Add count
        for item in this.Items do
            hash.Add item
        hash.ToHashCode()

    interface ISeq with
        member _.IsEmpty = count = 0
        member this.First = this.First
        member this.Next = if count <= 1 then None else Some(rest :> ISeq)
        member this.More = if count = 0 then this :> ISeq else rest :> ISeq
        member _.Count = count

    interface ISeqable with
        member this.Seq = this :> ISeq

    interface IMeta with
        member _.Meta = metadata

    interface IObj with
        member this.WithMeta(metadata) = this.WithMeta(metadata) :> IObj

    interface IEnumerable<obj> with
        member this.GetEnumerator() = this.Items.GetEnumerator()

    interface IEnumerable with
        member this.GetEnumerator() = (this.Items :> IEnumerable).GetEnumerator()

/// A cons cell whose tail is any sequence, which is what makes lazy/infinite
/// sequences expressible without materializing a list.
[<Sealed>]
type Cons(first: obj, rest: ISeq, metadata: IPersistentMap option) =
    new(first: obj, rest: ISeq) = Cons(first, rest, None)

    member _.First = first
    member _.Rest = rest
    member _.Meta = metadata
    member this.WithMeta(metadata: IPersistentMap option) = Cons(first, rest, metadata)

    interface ISeq with
        member _.IsEmpty = false
        member _.First = first
        member _.Next = if rest.IsEmpty then None else Some rest
        member _.More = rest
        member _.Count = 1 + rest.Count

    interface IMeta with
        member _.Meta = metadata

    interface IObj with
        member this.WithMeta(metadata) = this.WithMeta(metadata) :> IObj

/// Sequence backed by a thunk. Realization is one-shot and guarded so concurrent
/// readers see a single value.
[<Sealed>]
type LazySeq(generator: unit -> ISeq option, metadata: IPersistentMap option) =
    let gate = obj()
    let mutable generator = Some generator
    let mutable value: ISeq option = None

    new(generator: unit -> ISeq option) = LazySeq(generator, None)

    member private _.Realize() =
        match generator with
        | None -> ()
        | Some _ ->
            lock gate (fun () ->
                match generator with
                | Some produce ->
                    value <- produce ()
                    generator <- None
                | None -> ())

    member _.Meta = metadata

    member this.WithMeta(metadata: IPersistentMap option) =
        LazySeq(
            (fun () ->
                this.Realize()
                value),
            metadata
        )

    interface ISeq with
        member this.IsEmpty =
            this.Realize()
            match value with
            | Some sequence -> sequence.IsEmpty
            | None -> true

        member this.First =
            this.Realize()
            match value with
            | Some sequence -> sequence.First
            | None -> raise (InvalidOperationException("Cannot take the first element of an empty lazy sequence."))

        member this.Next =
            this.Realize()
            match value with
            | Some sequence -> sequence.Next
            | None -> None

        member this.More =
            this.Realize()
            match value with
            | Some sequence -> sequence.More
            | None -> PersistentList.Empty :> ISeq

        member this.Count =
            this.Realize()
            match value with
            | Some sequence -> sequence.Count
            | None -> 0

    interface ISeqable with
        member this.Seq = this :> ISeq

    interface IMeta with
        member _.Meta = metadata

    interface IObj with
        member this.WithMeta(metadata) = this.WithMeta(metadata) :> IObj

module private VectorInternals =
    let rec newPath (level: int) (node: obj[]) : obj[] =
        if level = 0 then node
        else
            let path = Array.zeroCreate<obj> 32
            path.[0] <- box (newPath (level - 5) node)
            path

    let rec pushTail (level: int) (parent: obj[]) (tailNode: obj[]) (count: int) : obj[] =
        let subindex = ((count - 1) >>> level) &&& 31
        let updated = Array.copy parent
        let child =
            if level = 5 then tailNode
            else
                match parent.[subindex] with
                | :? (obj[]) as existing -> pushTail (level - 5) existing tailNode count
                | _ -> newPath (level - 5) tailNode
        updated.[subindex] <- box child
        updated

    let rec assoc (level: int) (node: obj[]) (index: int) (value: obj) : obj[] =
        let updated = Array.copy node
        if level = 0 then
            updated.[index &&& 31] <- value
        else
            let subindex = (index >>> level) &&& 31
            updated.[subindex] <- box (assoc (level - 5) (node.[subindex] :?> obj[]) index value)
        updated

    let rec arrayFor (level: int) (node: obj[]) (index: int) : obj[] =
        if level = 0 then Array.copy node
        else arrayFor (level - 5) (node.[(index >>> level) &&& 31] :?> obj[]) index

    let rec popTail (level: int) (node: obj[]) (index: int) : obj[] option =
        let subindex = (index >>> level) &&& 31
        if isNull node.[subindex] then
            None
        elif level = 5 then
            let updated = Array.copy node
            updated.[subindex] <- null
            Some updated
        else
            match popTail (level - 5) (node.[subindex] :?> obj[]) index with
            | Some child ->
                let updated = Array.copy node
                updated.[subindex] <- box child
                Some updated
            | None when subindex = 0 -> None
            | None ->
                let updated = Array.copy node
                updated.[subindex] <- null
                Some updated

/// Bit-partitioned persistent vector, 32-way branching, with the tail leaf kept
/// outside the trie. Indexing is O(log32 n).
[<Sealed>]
type PersistentVector private (count: int, shift: int, root: obj[], tail: obj[], metadata: IPersistentMap option) =
    static let emptyRoot () = Array.zeroCreate<obj> 32
    static let empty = PersistentVector(0, 5, emptyRoot (), Array.empty<obj>, None)

    static member Empty = empty

    member _.Count = count
    member _.IsEmpty = count = 0
    member _.Meta = metadata

    member private _.TailOffset =
        if count < 32 then 0
        else ((count - 1) >>> 5) <<< 5

    member this.Nth(index: int) =
        if index < 0 || index >= count then
            raise (ArgumentOutOfRangeException(nameof index, index, sprintf "Index %d is outside a vector of length %d." index count))
        let tailOffset = this.TailOffset
        if index >= tailOffset then
            tail.[index - tailOffset]
        else
            let mutable node = root
            let mutable level = shift
            while level > 0 do
                node <- node.[(index >>> level) &&& 31] :?> obj[]
                level <- level - 5
            node.[index &&& 31]

    member this.Item
        with get (index: int) = this.Nth(index)

    member this.Add(value: obj) =
        if tail.Length < 32 then
            let updatedTail = Array.zeroCreate<obj> (tail.Length + 1)
            Array.blit tail 0 updatedTail 0 tail.Length
            updatedTail.[tail.Length] <- value
            PersistentVector(count + 1, shift, root, updatedTail, metadata)
        else
            let mutable nextShift = shift
            let nextRoot =
                if (count >>> 5) > (1 <<< shift) then
                    let grownRoot = Array.zeroCreate<obj> 32
                    grownRoot.[0] <- box root
                    grownRoot.[1] <- box (VectorInternals.newPath shift tail)
                    nextShift <- shift + 5
                    grownRoot
                else
                    VectorInternals.pushTail shift root tail count
            PersistentVector(count + 1, nextShift, nextRoot, [| value |], metadata)

    member this.AssocN(index: int, value: obj) =
        if index = count then
            this.Add(value)
        elif index < 0 || index > count then
            raise (ArgumentOutOfRangeException(nameof index, index, sprintf "Index %d is outside a vector of length %d." index count))
        elif index >= this.TailOffset then
            let updatedTail = Array.copy tail
            updatedTail.[index - this.TailOffset] <- value
            PersistentVector(count, shift, root, updatedTail, metadata)
        else
            PersistentVector(count, shift, VectorInternals.assoc shift root index value, tail, metadata)

    member this.Pop() =
        if count = 0 then
            raise (InvalidOperationException("Cannot pop an empty vector."))
        elif count = 1 then
            empty
        elif tail.Length > 1 then
            let updatedTail = Array.zeroCreate<obj> (tail.Length - 1)
            Array.blit tail 0 updatedTail 0 updatedTail.Length
            PersistentVector(count - 1, shift, root, updatedTail, metadata)
        else
            let updatedTail = VectorInternals.arrayFor shift root (count - 2)
            let mutable nextShift = shift
            let nextRoot =
                match VectorInternals.popTail shift root (count - 2) with
                | None -> emptyRoot ()
                | Some candidate when nextShift > 5 && isNull candidate.[1] ->
                    nextShift <- nextShift - 5
                    candidate.[0] :?> obj[]
                | Some candidate -> candidate
            PersistentVector(count - 1, nextShift, nextRoot, updatedTail, metadata)

    member this.WithMeta(metadata: IPersistentMap option) =
        PersistentVector(count, shift, root, tail, metadata)

    member this.Elements =
        seq {
            let rec walk level (node: obj[]) =
                seq {
                    if level = 0 then
                        yield! node
                    else
                        for i in 0 .. node.Length - 1 do
                            let child = node.[i]
                            if not (isNull child) then
                                yield! walk (level - 5) (child :?> obj[])
                }
            yield! walk shift root
            yield! tail
        }

    static member OfSeq(items: seq<obj>) =
        Seq.fold (fun (vector: PersistentVector) item -> vector.Add item) empty items

    static member OfArray(items: obj[]) = PersistentVector.OfSeq items

    override this.Equals(other: obj) =
        match other with
        | :? PersistentVector as that ->
            count = that.Count && Seq.forall2 (fun a b -> Object.Equals(a, b)) this.Elements that.Elements
        | _ -> false

    override this.GetHashCode() =
        let mutable hash = HashCode()
        hash.Add count
        for item in this.Elements do
            hash.Add item
        hash.ToHashCode()

    interface IReadOnlyList<obj> with
        member this.Count = count
        member this.Item with get index = this.Nth(index)

    interface IEnumerable<obj> with
        member this.GetEnumerator() = this.Elements.GetEnumerator()

    interface IEnumerable with
        member this.GetEnumerator() = (this.Elements :> IEnumerable).GetEnumerator()

    interface ISeqable with
        member this.Seq =
            if count = 0 then EmptySeq.Instance
            else IndexedSeq(this, 0) :> ISeq

    interface IMeta with
        member _.Meta = metadata

    interface IObj with
        member this.WithMeta(metadata) = this.WithMeta(metadata) :> IObj

    interface IFn with
        member this.Invoke(args: obj[]) =
            match args.Length with
            | 1 ->
                let index = Convert.ToInt32 args.[0]
                this.Nth(index)
            | 2 ->
                let index = Convert.ToInt32 args.[0]
                if index >= 0 && index < count then this.Nth(index) else args.[1]
            | _ -> raise (ArityException("1 or 2", args.Length))

/// Persistent hash map. The backing store is System.Collections.Immutable's
/// HAMT; this type supplies language equality, nil keys, metadata, and IFn lookup.
[<Sealed>]
type PersistentMap private (inner: ImmutableDictionary<obj, obj>, metadata: IPersistentMap option) =
    static let nilKey = obj ()
    static let keyOf (key: obj) = if isNull key then nilKey else key
    static let valueOf (key: obj) = if Object.ReferenceEquals(key, nilKey) then null else key
    static let entries (inner: ImmutableDictionary<obj, obj>) =
        seq { for entry in inner -> KeyValuePair(valueOf entry.Key, entry.Value) }

    static member Empty = PersistentMap(ImmutableDictionary<obj, obj>.Empty, None)

    member _.Count = inner.Count
    member _.Meta = metadata
    member _.Entries = entries inner

    member _.ContainsKey(key: obj) = inner.ContainsKey(keyOf key)

    member _.TryGet(key: obj) =
        match inner.TryGetValue(keyOf key) with
        | true, value -> Some value
        | false, _ -> None

    member _.ValAt(key: obj) =
        match inner.TryGetValue(keyOf key) with
        | true, value -> value
        | false, _ -> null

    member this.Assoc(key: obj, value: obj) =
        PersistentMap(inner.SetItem(keyOf key, value), metadata)

    member this.Without(key: obj) =
        let key = keyOf key
        if inner.ContainsKey key then PersistentMap(inner.Remove key, metadata)
        else this

    member this.WithMeta(metadata: IPersistentMap option) =
        PersistentMap(inner, metadata)

    member this.Seq =
        PersistentList.OfSeq(
            entries inner
            |> Seq.map (fun entry -> PersistentVector.OfSeq [| entry.Key; entry.Value |] :> obj)
        )

    override this.Equals(other: obj) =
        match other with
        | :? PersistentMap as that ->
            this.Count = that.Count
            && this.Entries
               |> Seq.forall (fun entry ->
                   match that.TryGet entry.Key with
                   | Some value -> Object.Equals(value, entry.Value)
                   | None -> false)
        | _ -> false

    override this.GetHashCode() =
        let mutable hash = 0
        for entry in this.Entries do
            let keyHash = if isNull entry.Key then 0 else entry.Key.GetHashCode()
            let valueHash = if isNull entry.Value then 0 else entry.Value.GetHashCode()
            hash <- hash ^^^ (keyHash * 31 + valueHash)
        hash

    interface IPersistentMap with
        member this.Count = this.Count
        member this.ValAt(key) = this.ValAt(key)
        member this.TryGet(key) = this.TryGet(key)
        member this.Assoc(key, value) = this.Assoc(key, value) :> IPersistentMap
        member this.Without(key) = this.Without(key) :> IPersistentMap
        member this.Entries = this.Entries
        member this.Seq = this.Seq

    interface ISeqable with
        member this.Seq = this.Seq :> ISeq

    interface IMeta with
        member _.Meta = metadata

    interface IObj with
        member this.WithMeta(metadata) = this.WithMeta(metadata) :> IObj

    interface IFn with
        member this.Invoke(args: obj[]) =
            match args.Length with
            | 1 -> this.ValAt(args.[0])
            | 2 ->
                match this.TryGet(args.[0]) with
                | Some value -> value
                | None -> args.[1]
            | _ -> raise (ArityException("1 or 2", args.Length))

    interface IEnumerable<KeyValuePair<obj, obj>> with
        member this.GetEnumerator() = (this.Entries :> seq<_>).GetEnumerator()

    interface IEnumerable with
        member this.GetEnumerator() = (this.Entries :> IEnumerable).GetEnumerator()

/// Persistent hash set over the same HAMT machinery as the map.
[<Sealed>]
type PersistentSet private (inner: ImmutableHashSet<obj>, metadata: IPersistentMap option) =
    static member Empty = PersistentSet(ImmutableHashSet<obj>.Empty, None)

    member _.Count = inner.Count
    member _.Meta = metadata
    member _.Elements = inner :> seq<obj>

    member _.Contains(value: obj) = inner.Contains value

    member this.Add(value: obj) = PersistentSet(inner.Add value, metadata)

    member this.Remove(value: obj) =
        if inner.Contains value then PersistentSet(inner.Remove value, metadata)
        else this

    member this.WithMeta(metadata: IPersistentMap option) = PersistentSet(inner, metadata)

    member this.Seq = PersistentList.OfSeq inner

    static member OfSeq(items: seq<obj>) =
        Seq.fold (fun (set: PersistentSet) item -> set.Add item) PersistentSet.Empty items

    override this.Equals(other: obj) =
        match other with
        | :? PersistentSet as that ->
            this.Count = that.Count && this.Elements |> Seq.forall that.Contains
        | _ -> false

    override this.GetHashCode() =
        let mutable hash = 0
        for element in this.Elements do
            hash <- hash ^^^ (if isNull element then 0 else element.GetHashCode())
        hash

    interface ISeqable with
        member this.Seq = this.Seq :> ISeq

    interface IMeta with
        member _.Meta = metadata

    interface IObj with
        member this.WithMeta(metadata) = this.WithMeta(metadata) :> IObj

    interface IEnumerable<obj> with
        member this.GetEnumerator() = this.Elements.GetEnumerator()

    interface IEnumerable with
        member this.GetEnumerator() = (this.Elements :> IEnumerable).GetEnumerator()

    interface IFn with
        member this.Invoke(args: obj[]) =
            match args.Length with
            | 1 ->
                if this.Contains args.[0] then args.[0] else null
            | 2 ->
                if this.Contains args.[0] then args.[0] else args.[1]
            | _ -> raise (ArityException("1 or 2", args.Length))

module Seqs =
    let rec toSeq (source: ISeq option) : seq<obj> =
        seq {
            match source with
            | Some sequence when not sequence.IsEmpty ->
                yield sequence.First
                yield! toSeq sequence.Next
            | _ -> ()
        }

    let ofSeqable (value: obj) : ISeq option =
        match value with
        | null -> None
        | :? ISeq as sequence -> if sequence.IsEmpty then None else Some sequence
        | :? ISeqable as seqable ->
            let sequence = seqable.Seq
            if sequence.IsEmpty then None else Some sequence
        | :? IReadOnlyList<obj> as list ->
            if list.Count = 0 then None else Some(IndexedSeq(list, 0) :> ISeq)
        | _ -> None

    let toList (value: obj) = ofSeqable value |> toSeq |> Seq.toList
    let toVector (value: obj) = PersistentVector.OfSeq(toList value)

module Meta =
    let merge (left: IPersistentMap option) (right: IPersistentMap option) =
        match left, right with
        | None, None -> None
        | Some map, None -> Some map
        | None, Some map -> Some map
        | Some left, Some right ->
            let mutable result = left
            for entry in right.Entries do
                result <- result.Assoc(entry.Key, entry.Value)
            Some result

    let tryGet (key: obj) (metadata: IPersistentMap option) =
        metadata |> Option.bind (fun map -> map.TryGet key)
