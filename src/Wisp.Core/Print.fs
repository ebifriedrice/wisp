namespace Wisp.Core

open System
open System.Globalization
open System.Numerics
open System.Text
open System.Text.RegularExpressions

/// Reader-oriented and human-oriented rendering for runtime values.
module Print =
    let private escapeString (value: string) =
        let builder = StringBuilder(value.Length + 2)
        builder.Append '"' |> ignore
        for ch in value do
            match ch with
            | '\\' -> builder.Append "\\\\" |> ignore
            | '"' -> builder.Append "\\\"" |> ignore
            | '\n' -> builder.Append "\\n" |> ignore
            | '\r' -> builder.Append "\\r" |> ignore
            | '\t' -> builder.Append "\\t" |> ignore
            | '\b' -> builder.Append "\\b" |> ignore
            | '\f' -> builder.Append "\\f" |> ignore
            | _ when Char.IsControl ch -> builder.AppendFormat("\\u{0:X4}", int ch) |> ignore
            | _ -> builder.Append ch |> ignore
        builder.Append '"' |> ignore
        builder.ToString()

    let private charName (value: char) =
        match value with
        | '\n' -> "newline"
        | ' ' -> "space"
        | '\t' -> "tab"
        | '\b' -> "backspace"
        | '\f' -> "formfeed"
        | '\r' -> "return"
        | _ -> string value

    let private formatDouble (value: double) =
        if Double.IsNaN value then "##NaN"
        elif Double.IsPositiveInfinity value then "##Inf"
        elif Double.IsNegativeInfinity value then "##-Inf"
        else value.ToString("R", CultureInfo.InvariantCulture)

    let private join (items: seq<string>) = String.Join(" ", items)

    let rec printString (value: obj) : string =
        match value with
        | null -> "nil"
        | :? string as text -> escapeString text
        | :? char as ch -> "\\" + charName ch
        | :? bool as flag -> if flag then "true" else "false"
        | :? int as number -> number.ToString(CultureInfo.InvariantCulture)
        | :? int64 as number -> number.ToString(CultureInfo.InvariantCulture)
        | :? int16 as number -> number.ToString(CultureInfo.InvariantCulture)
        | :? uint as number -> number.ToString(CultureInfo.InvariantCulture)
        | :? uint64 as number -> number.ToString(CultureInfo.InvariantCulture)
        | :? uint16 as number -> number.ToString(CultureInfo.InvariantCulture)
        | :? byte as number -> number.ToString(CultureInfo.InvariantCulture)
        | :? sbyte as number -> number.ToString(CultureInfo.InvariantCulture)
        | :? single as number ->
            if Single.IsNaN number then "##NaN"
            elif Single.IsPositiveInfinity number then "##Inf"
            elif Single.IsNegativeInfinity number then "##-Inf"
            else number.ToString("R", CultureInfo.InvariantCulture)
        | :? double as number -> formatDouble number
        | :? decimal as number -> number.ToString(CultureInfo.InvariantCulture) + "M"
        | :? BigInteger as number -> number.ToString(CultureInfo.InvariantCulture) + "N"
        | :? Ratio as ratio -> ratio.ToString()
        | :? Keyword as keyword -> keyword.ToString()
        | :? Symbol as symbol -> symbol.FullName
        | :? Var as variable -> variable.ToString()
        | :? PersistentList as list -> "(" + join (list.Items |> Seq.map printString) + ")"
        | :? PersistentVector as vector -> "[" + join (vector.Elements |> Seq.map printString) + "]"
        | :? PersistentMap as map ->
            let entries =
                map.Entries
                |> Seq.map (fun entry -> printString entry.Key + " " + printString entry.Value)
            "{" + join entries + "}"
        | :? PersistentSet as set -> "#{" + join (set.Elements |> Seq.map printString) + "}"
        | :? TaggedLiteral as literal -> "#" + literal.Tag.FullName + " " + printString literal.Form
        | :? Regex as regex -> "#\"" + regex.ToString() + "\""
        | :? DateTimeOffset as instant -> "#inst \"" + instant.ToString("O", CultureInfo.InvariantCulture) + "\""
        | :? Guid as identifier -> "#uuid \"" + identifier.ToString() + "\""
        | :? Type as clrType -> if isNull clrType.FullName then clrType.Name else clrType.FullName
        | :? IWispRecord as record -> printRecord record.TypeName record.FieldNames record.Fields
        | :? IFn -> "#<fn>"
        | _ -> value.ToString()

    and private printRecord typeName fieldNames (fields: IPersistentMap) =
        let fieldsText =
            fieldNames
            |> List.map (fun fieldName ->
                ":" + fieldName + " " + printString (fields.ValAt(Keyword.Intern fieldName)))
            |> join
        "#" + typeName + "{" + fieldsText + "}"

    /// Human display used by str, print, and println: strings lose their quotes
    /// and nil renders as the empty string.
    let displayString (value: obj) =
        match value with
        | null -> ""
        | :? string as text -> text
        | _ -> printString value

    let printMany (values: seq<obj>) = values |> Seq.map displayString |> join
    let prStrMany (values: seq<obj>) = values |> Seq.map printString |> join
