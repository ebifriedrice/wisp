namespace Wisp.Core

open System
open System.Threading

/// Ambient runtime captured by record instances. Interface stubs emitted by the
/// compiler use it to reach the session's defmethod dispatch table.
module RecordRuntimeContext =
    let private current = AsyncLocal<obj>()
    let getCurrent () = current.Value
    let setCurrent (value: obj) = current.Value <- value

/// Helpers called by emitted value-type structs, which cannot inherit
/// WispRecordBase.
module RecordSupport =
    let private buildFields (fieldNames: string[]) (values: obj[]) =
        let mutable map = PersistentMap.Empty
        for index in 0 .. fieldNames.Length - 1 do
            map <- map.Assoc(Keyword.Intern fieldNames.[index], values.[index])
        map :> IPersistentMap

    let FieldNames(fieldNames: string[]) = fieldNames |> Array.toList

    let Fields(typeName: string, fieldNames: string[], values: obj[]) : IPersistentMap =
        ignore typeName
        buildFields fieldNames values

    let Equals(typeName: string, fieldNames: string[], values: obj[], other: obj) =
        match other with
        | :? IWispRecord as record ->
            String.Equals(typeName, record.TypeName, StringComparison.Ordinal)
            && Object.Equals(buildFields fieldNames values, record.Fields)
        | _ -> false

    let Hash(typeName: string, fieldNames: string[], values: obj[]) =
        HashCode.Combine(typeName, (buildFields fieldNames values).GetHashCode())

    let ToString(typeName: string, fieldNames: string[], values: obj[]) =
        let fields =
            fieldNames
            |> Array.mapi (fun index name -> ":" + name + " " + Print.printString values.[index])
            |> String.concat " "
        "#" + typeName + "{" + fields + "}"

/// Base class for generated reference-type records. Value-type structs use
/// RecordSupport instead. Data lives here; methods are registered separately in
/// the runtime dispatch table.
[<AbstractClass>]
type WispRecordBase(typeName: string, fieldNames: string[], values: obj[]) =
    do
        if isNull fieldNames then nullArg (nameof fieldNames)
        if isNull values then nullArg (nameof values)
        if fieldNames.Length <> values.Length then
            invalidArg (nameof values) "Record field names and values must have the same length."

    let fields =
        let mutable map = PersistentMap.Empty
        for index in 0 .. fieldNames.Length - 1 do
            map <- map.Assoc(Keyword.Intern fieldNames.[index], values.[index])
        map

    member _.TypeName = typeName
    member _.FieldNames = fieldNames
    member _.Values = values
    member _.Get(index: int) = values.[index]
    member _.Fields : IPersistentMap = fields :> IPersistentMap
    member val Runtime: obj = RecordRuntimeContext.getCurrent () with get, set

    override _.Equals(other: obj) =
        match other with
        | :? IWispRecord as record ->
            String.Equals(typeName, record.TypeName, StringComparison.Ordinal)
            && Object.Equals(fields, record.Fields)
        | _ -> false

    override _.GetHashCode() = HashCode.Combine(typeName, fields.GetHashCode())

    override this.ToString() = Print.printString (box this)

    interface IWispRecord with
        member _.TypeName = typeName
        member _.FieldNames = fieldNames |> Array.toList
        member _.Fields = fields :> IPersistentMap
