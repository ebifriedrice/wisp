namespace Wisp.Core

open System
open System.Collections.Generic

/// A sequence view. IsEmpty allows lazy tails to be tested without forcing their length.
type ISeq =
    abstract IsEmpty: bool
    abstract First: obj
    abstract Next: ISeq option
    abstract More: ISeq
    abstract Count: int

type ISeqable =
    abstract Seq: ISeq

type IPersistentMap =
    abstract Count: int
    abstract ValAt: key: obj -> obj
    abstract TryGet: key: obj -> obj option
    abstract Assoc: key: obj * value: obj -> IPersistentMap
    abstract Without: key: obj -> IPersistentMap
    abstract Entries: seq<KeyValuePair<obj, obj>>
    abstract Seq: ISeq

type IMeta =
    abstract Meta: IPersistentMap option

type IObj =
    inherit IMeta
    abstract WithMeta: metadata: IPersistentMap option -> IObj

/// User-defined data type. Methods live in a separate dispatch table, mirroring
/// the language's data/impl split.
type IWispRecord =
    abstract TypeName: string
    abstract FieldNames: string list
    abstract Fields: IPersistentMap

type IFn =
    abstract Invoke: args: obj[] -> obj

type ArityException(expected: string, actual: int) =
    inherit ArgumentException(sprintf "Wrong number of arguments: expected %s, got %d" expected actual)

/// Empty sequence sentinel. Collections that are not themselves lists return this
/// from More at the end rather than inventing a new empty value per call.
[<Sealed>]
type EmptySeq private () =
    static let instance = EmptySeq()

    static member Instance = instance :> ISeq

    interface ISeq with
        member _.IsEmpty = true
        member _.First = raise (InvalidOperationException("Cannot take the first element of an empty sequence."))
        member _.Next = None
        member _.More = instance :> ISeq
        member _.Count = 0

/// ISeq view over an indexed collection, used by vectors and other random-access values.
[<Sealed>]
type internal IndexedSeq(items: IReadOnlyList<obj>, index: int) =
    interface ISeq with
        member _.IsEmpty = index >= items.Count
        member _.First = items.[index]
        member _.Next =
            if index + 1 < items.Count then Some(IndexedSeq(items, index + 1) :> ISeq)
            else None
        member _.More =
            if index + 1 < items.Count then IndexedSeq(items, index + 1) :> ISeq
            else EmptySeq.Instance
        member _.Count = items.Count - index
