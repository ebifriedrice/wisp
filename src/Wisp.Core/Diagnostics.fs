namespace Wisp.Core

open System
open System.Text

/// A half-open range in a source text, measured in UTF-16 code units.
[<Struct>]
type TextSpan =
    { Start: int
      Length: int }

    static member Zero = { Start = 0; Length = 0 }

    static member Create(start: int, length: int) =
        if start < 0 then invalidArg (nameof start) "A span cannot start before zero."
        if length < 0 then invalidArg (nameof length) "A span cannot have a negative length."
        { Start = start; Length = length }

    member this.End = this.Start + this.Length

[<RequireQualifiedAccess>]
type DiagnosticSeverity =
    | Error
    | Warning
    | Information
    | Hint

type DiagnosticLabel =
    { Span: TextSpan
      Message: string option }

type Diagnostic =
    { Code: string
      Severity: DiagnosticSeverity
      Message: string
      Span: TextSpan
      Labels: DiagnosticLabel list
      Notes: string list }

module Diagnostic =
    let create code severity message span =
        { Code = code
          Severity = severity
          Message = message
          Span = span
          Labels = []
          Notes = [] }

    let withLabel span message diagnostic =
        { diagnostic with
            Labels = diagnostic.Labels @ [ { Span = span; Message = Some message } ] }

    let withNote note diagnostic =
        { diagnostic with Notes = diagnostic.Notes @ [ note ] }

/// Mutable collection used by reader and compiler passes while walking a tree.
/// Consumers see an immutable snapshot through Items.
[<Sealed>]
type DiagnosticBag() =
    let items = ResizeArray<Diagnostic>()

    member _.Add(diagnostic: Diagnostic) = items.Add diagnostic
    member _.AddRange(diagnostics: seq<Diagnostic>) = items.AddRange diagnostics
    member _.Count = items.Count
    member _.Items = items |> Seq.toList
    member _.HasErrors = items |> Seq.exists (fun d -> d.Severity = DiagnosticSeverity.Error)

/// Source text plus the line index needed to turn offsets into human locations.
[<Sealed>]
type SourceText private (text: string, path: string option, lineStarts: int[]) =
    static member Create(text: string, ?path: string) =
        if isNull text then nullArg (nameof text)
        let starts = ResizeArray<int>()
        starts.Add 0
        for i in 0 .. text.Length - 1 do
            if text.[i] = '\n' then starts.Add(i + 1)
        SourceText(text, path, starts.ToArray())

    member _.Text = text
    member _.Path = path
    member _.Length = text.Length
    member _.LineCount = lineStarts.Length

    member _.LineStart(line: int) =
        if line < 0 || line >= lineStarts.Length then
            raise (ArgumentOutOfRangeException(nameof line, line, "Line index is outside the source text."))
        lineStarts.[line]

    member _.GetLine(line: int) =
        let start = lineStarts.[line]
        let finish =
            if line + 1 < lineStarts.Length then lineStarts.[line + 1] - 1
            else text.Length
        let finish = if finish > start && text.[finish - 1] = '\r' then finish - 1 else finish
        text.Substring(start, finish - start)

    /// One-based line and column for a zero-based offset.
    member _.LineColumn(offset: int) =
        let offset = max 0 (min text.Length offset)
        let index = Array.BinarySearch(lineStarts, offset)
        let line = if index >= 0 then index else ~~~index - 1
        line + 1, offset - lineStarts.[line] + 1

    member this.Slice(span: TextSpan) =
        let start = max 0 (min text.Length span.Start)
        let finish = max start (min text.Length span.End)
        text.Substring(start, finish - start)

    /// Renders a compiler-style diagnostic with the offending source line and a caret.
    member this.Render(diagnostic: Diagnostic) =
        let line, column = this.LineColumn diagnostic.Span.Start
        let severity =
            match diagnostic.Severity with
            | DiagnosticSeverity.Error -> "error"
            | DiagnosticSeverity.Warning -> "warning"
            | DiagnosticSeverity.Information -> "info"
            | DiagnosticSeverity.Hint -> "hint"
        let location =
            match path with
            | Some path -> sprintf "%s:%d:%d" path line column
            | None -> sprintf "%d:%d" line column
        let sourceLine = this.GetLine(line - 1)
        let lineNumber = string line
        let gutter = String(' ', lineNumber.Length)
        let caretStart = max 0 (min sourceLine.Length (column - 1))
        let caretLength =
            if diagnostic.Span.Length = 0 then 1
            else max 1 (min diagnostic.Span.Length (sourceLine.Length - caretStart))
        let builder = StringBuilder()
        builder.AppendFormat("{0}: {1} [{2}]: {3}", location, severity, diagnostic.Code, diagnostic.Message) |> ignore
        builder.AppendLine() |> ignore
        builder.AppendFormat("{0} |", lineNumber) |> ignore
        builder.AppendLine() |> ignore
        builder.AppendFormat("{0} | {1}", gutter, sourceLine) |> ignore
        builder.AppendLine() |> ignore
        builder.AppendFormat("{0} | {1}{2}", gutter, String(' ', caretStart), String('^', caretLength)) |> ignore
        for label in diagnostic.Labels do
            let labelLine, labelColumn = this.LineColumn label.Span.Start
            let message = defaultArg label.Message "related location"
            builder.AppendLine() |> ignore
            builder.AppendFormat("{0} = {1}:{2}: {3}", gutter, labelLine, labelColumn, message) |> ignore
        for note in diagnostic.Notes do
            builder.AppendLine() |> ignore
            builder.AppendFormat("{0} = note: {1}", gutter, note) |> ignore
        builder.ToString()
