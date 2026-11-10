namespace Wisp.Compiler

open System
open System.Collections.Generic
open System.IO
open System.Text

/// A small terminal Markdown renderer for docstrings. It intentionally covers
/// the constructs documentation uses (headings, lists, code, emphasis, links)
/// and reports the doc targets referenced by the text.
module Markdown =
    let private style (styled: bool) (code: string) (text: string) =
        if styled then "\u001b[" + code + "m" + text + "\u001b[0m" else text

    let private isFence (line: string) =
        line.TrimStart().StartsWith("```", StringComparison.Ordinal)

    let private heading (line: string) =
        let trimmed = line.TrimStart()
        let mutable level = 0
        while level < trimmed.Length && trimmed.[level] = '#' do
            level <- level + 1
        if level > 0 && level <= 6 && level < trimmed.Length && trimmed.[level] = ' ' then
            Some(level, trimmed.Substring(level + 1).Trim())
        else
            None

    let private bullet (line: string) =
        let trimmed = line.TrimStart()
        if trimmed.Length >= 2
           && (trimmed.[0] = '-' || trimmed.[0] = '*' || trimmed.[0] = '+')
           && trimmed.[1] = ' ' then
            Some(trimmed.Substring(2))
        else
            None

    let private ordered (line: string) =
        let trimmed = line.TrimStart()
        let mutable digits = 0
        while digits < trimmed.Length && Char.IsDigit trimmed.[digits] do
            digits <- digits + 1
        if digits > 0 && digits < trimmed.Length && trimmed.[digits] = '.' then
            let number = Int32.Parse(trimmed.Substring(0, digits), Globalization.CultureInfo.InvariantCulture)
            Some(number, trimmed.Substring(digits + 1).TrimStart())
        else
            None

    let private isRule (line: string) =
        let trimmed = line.Trim()
        trimmed.Length >= 3
        && (trimmed |> Seq.forall ((=) '-')
            || trimmed |> Seq.forall ((=) '*')
            || trimmed |> Seq.forall ((=) '_'))

    let render (writer: TextWriter) (text: string) (resolve: string -> string option) (styled: bool) : string list =
        let references = ResizeArray<string>()

        let addReference canonical =
            if not (String.IsNullOrWhiteSpace canonical) && not (references.Contains canonical) then
                references.Add canonical

        let rec renderInline (value: string) =
            let builder = StringBuilder()
            let mutable index = 0
            while index < value.Length do
                let current = value.[index]
                if current = '`' then
                    let closing = value.IndexOf('`', index + 1)
                    if closing > index then
                        let content = value.Substring(index + 1, closing - index - 1)
                        match resolve content with
                        | Some canonical ->
                            addReference canonical
                            builder.Append(style styled "4;36" content) |> ignore
                        | None -> builder.Append(style styled "36" content) |> ignore
                        index <- closing + 1
                    else
                        builder.Append current |> ignore
                        index <- index + 1
                elif current = '*' && index + 1 < value.Length && value.[index + 1] = '*' then
                    let closing = value.IndexOf("**", index + 2, StringComparison.Ordinal)
                    if closing > index + 1 then
                        let content = value.Substring(index + 2, closing - index - 2)
                        builder.Append(style styled "1" (renderInline content)) |> ignore
                        index <- closing + 2
                    else
                        builder.Append current |> ignore
                        index <- index + 1
                elif current = '*' then
                    let closing = value.IndexOf('*', index + 1)
                    if closing > index + 1 then
                        let content = value.Substring(index + 1, closing - index - 1)
                        builder.Append(style styled "3" (renderInline content)) |> ignore
                        index <- closing + 1
                    else
                        builder.Append current |> ignore
                        index <- index + 1
                elif current = '[' then
                    let closeBracket = value.IndexOf(']', index + 1)
                    if closeBracket > index
                       && closeBracket + 1 < value.Length
                       && value.[closeBracket + 1] = '(' then
                        let closeParen = value.IndexOf(')', closeBracket + 2)
                        if closeParen > closeBracket then
                            let label = value.Substring(index + 1, closeBracket - index - 1)
                            let target =
                                value.Substring(closeBracket + 2, closeParen - closeBracket - 2).Trim()
                            match resolve target with
                            | Some canonical ->
                                addReference canonical
                                builder.Append(style styled "4" (renderInline label)) |> ignore
                            | None -> builder.Append(renderInline label) |> ignore
                            index <- closeParen + 1
                        else
                            builder.Append current |> ignore
                            index <- index + 1
                    else
                        builder.Append current |> ignore
                        index <- index + 1
                else
                    builder.Append current |> ignore
                    index <- index + 1
            builder.ToString()

        let lines =
            text.Replace("\r\n", "\n").Replace("\r", "\n").Split('\n') |> Array.toList |> List.toArray

        let output = ResizeArray<string>()
        let ensureGap () =
            if output.Count > 0 && output.[output.Count - 1] <> "" then
                output.Add ""

        let startsBlock (line: string) =
            let trimmed = line.Trim()
            isFence line
            || (heading line |> Option.isSome)
            || (bullet line |> Option.isSome)
            || (ordered line |> Option.isSome)
            || trimmed.StartsWith(">", StringComparison.Ordinal)
            || isRule line

        let mutable index = 0
        while index < lines.Length do
            let line = lines.[index]
            let trimmed = line.Trim()
            if trimmed = "" then
                index <- index + 1
            elif isFence line then
                ensureGap ()
                index <- index + 1
                while index < lines.Length && not (isFence lines.[index]) do
                    output.Add(if lines.[index].Trim() = "" then "" else "  " + lines.[index])
                    index <- index + 1
                if index < lines.Length then
                    index <- index + 1
                ensureGap ()
            elif heading line |> Option.isSome then
                let level, content = heading line |> Option.get
                ensureGap ()
                let code = if level = 1 then "1;4;36" elif level = 2 then "1;36" else "1"
                output.Add(style styled code (renderInline content))
                index <- index + 1
            elif bullet line |> Option.isSome then
                let content = bullet line |> Option.get
                let indent = line.Length - line.TrimStart().Length
                output.Add(String(' ', indent) + style styled "36" "• " + renderInline content)
                index <- index + 1
            elif ordered line |> Option.isSome then
                let number, content = ordered line |> Option.get
                let indent = line.Length - line.TrimStart().Length
                output.Add(
                    String(' ', indent)
                    + style styled "36" (string number + ". ")
                    + renderInline content
                )
                index <- index + 1
            elif trimmed.StartsWith(">", StringComparison.Ordinal) then
                ensureGap ()
                let content = trimmed.Substring(1).TrimStart()
                output.Add(style styled "2" ("│ " + renderInline content))
                index <- index + 1
            elif isRule line then
                ensureGap ()
                output.Add(style styled "2" (String('─', 40)))
                ensureGap ()
                index <- index + 1
            else
                ensureGap ()
                let paragraph = ResizeArray<string>()
                while index < lines.Length
                      && lines.[index].Trim() <> ""
                      && not (startsBlock lines.[index]) do
                    paragraph.Add(lines.[index].Trim())
                    index <- index + 1
                output.Add(renderInline (String.Join(" ", paragraph)))

        while output.Count > 0 && output.[output.Count - 1] = "" do
            output.RemoveAt(output.Count - 1)
        for line in output do
            writer.WriteLine line
        references |> Seq.toList
