namespace Wisp.Core

open System
open System.Text

/// A half-open range in a source text, measured in UTF-16 code units. Spans
/// read from source also carry the file and one-based position so diagnostics
/// and stack frames can point outside the text currently being rendered.
[<Struct; CustomEquality; NoComparison>]
type TextSpan =
    { Start: int
      Length: int
      Path: string option
      Line: int option
      Column: int option }

    static member Zero =
        { Start = 0
          Length = 0
          Path = None
          Line = None
          Column = None }

    static member Create(start: int, length: int) =
        if start < 0 then invalidArg (nameof start) "A span cannot start before zero."
        if length < 0 then invalidArg (nameof length) "A span cannot have a negative length."

        { Start = start
          Length = length
          Path = None
          Line = None
          Column = None }

    static member Create(start: int, length: int, path: string option, line: int, column: int) =
        if start < 0 then invalidArg (nameof start) "A span cannot start before zero."
        if length < 0 then invalidArg (nameof length) "A span cannot have a negative length."

        { Start = start
          Length = length
          Path = path
          Line = Some line
          Column = Some column }

    member this.End = this.Start + this.Length

    // Source identity is diagnostic metadata, not part of the range identity.
    override this.Equals(other: obj) =
        match other with
        | :? TextSpan as other -> this.Start = other.Start && this.Length = other.Length
        | _ -> false

    override this.GetHashCode() = HashCode.Combine(this.Start, this.Length)

    interface IEquatable<TextSpan> with
        member this.Equals(other) = this.Start = other.Start && this.Length = other.Length

[<RequireQualifiedAccess>]
type DiagnosticSeverity =
    | Error
    | Warning
    | Information
    | Hint

type DiagnosticLabel =
    { Span: TextSpan
      Message: string option }

/// One active Wisp function, innermost first. Span is the call site that
/// entered the function.
type CallFrame =
    { Function: string
      Span: TextSpan option }

type CallStackTrace =
    { Origin: TextSpan option
      Frames: CallFrame list }

/// Per-thread (and per-async-flow) call stack captured when an exception
/// escapes evaluation.
module CallStack =
    open System.Threading

    let private frames = AsyncLocal<CallFrame list option>()
    let private site = AsyncLocal<TextSpan option>()
    let private dataKey = "Wisp.CallStack"

    let current () = frames.Value |> Option.defaultValue []

    let unboxSpan (value: obj) =
        if isNull value then None
        else unbox<TextSpan option> value

    let push (name: string) (span: TextSpan option) =
        let previous = current ()
        frames.Value <- Some({ Function = name; Span = span } :: previous)
        previous

    let restore (previous: CallFrame list) = frames.Value <- Some previous

    let peekSite () = site.Value

    let takeSite () =
        let value = site.Value
        site.Value <- None
        value

    let capture (error: exn) (fallbackOrigin: TextSpan option) =
        if not (error.Data.Contains dataKey) then
            error.Data.[dataKey] <-
                { Origin = fallbackOrigin
                  Frames = current () }

    let tryGet (error: exn) =
        match error.Data.Contains dataKey with
        | true -> Some(error.Data.[dataKey] :?> CallStackTrace)
        | false -> None

    /// Publishes the call site for the next function invocation. Unnamed
    /// built-ins leave it in place so callbacks invoked by built-ins inherit it.
    let withSite (span: TextSpan option) (action: unit -> 'T) =
        let previous = site.Value
        site.Value <- span
        try
            try
                action ()
            with error ->
                capture error span
                reraise ()
        finally
            site.Value <- previous

type Diagnostic =
    { Code: string
      Severity: DiagnosticSeverity
      Message: string
      Span: TextSpan
      Labels: DiagnosticLabel list
      Notes: string list
      Trace: CallFrame list }

module Diagnostic =
    let create code severity message span =
        { Code = code
          Severity = severity
          Message = message
          Span = span
          Labels = []
          Notes = []
          Trace = [] }

    let withLabel span message diagnostic =
        { diagnostic with
            Labels = diagnostic.Labels @ [ { Span = span; Message = Some message } ] }

    let withNote note diagnostic =
        { diagnostic with Notes = diagnostic.Notes @ [ note ] }

    let withTrace frames diagnostic = { diagnostic with Trace = frames }

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

    member private this.SpanLocation(fallbackPath: string option, span: TextSpan) =
        let line, column =
            match span.Line, span.Column with
            | Some line, Some column -> line, column
            | _ -> this.LineColumn span.Start
        match span.Path |> Option.orElse fallbackPath with
        | Some locationPath -> sprintf "%s:%d:%d" locationPath line column
        | None -> sprintf "%d:%d" line column

    /// Renders a compiler-style diagnostic with the offending source line, a
    /// caret, and the captured Wisp call stack.
    member this.Render(diagnostic: Diagnostic) =
        let line, column =
            match diagnostic.Span.Line, diagnostic.Span.Column with
            | Some line, Some column -> line, column
            | _ -> this.LineColumn diagnostic.Span.Start
        let sameSource =
            match diagnostic.Span.Path with
            | Some spanPath -> path |> Option.exists (fun sourcePath -> String.Equals(spanPath, sourcePath, StringComparison.Ordinal))
            | None -> true
        let severity =
            match diagnostic.Severity with
            | DiagnosticSeverity.Error -> "error"
            | DiagnosticSeverity.Warning -> "warning"
            | DiagnosticSeverity.Information -> "info"
            | DiagnosticSeverity.Hint -> "hint"
        let location = this.SpanLocation(path, diagnostic.Span)
        let builder = StringBuilder()
        builder.AppendFormat("{0}: {1} [{2}]: {3}", location, severity, diagnostic.Code, diagnostic.Message) |> ignore
        let gutter =
            if sameSource && line >= 1 && line <= this.LineCount then
                let sourceLine = this.GetLine(line - 1)
                let lineNumber = string line
                let gutter = String(' ', lineNumber.Length)
                let caretStart = max 0 (min sourceLine.Length (column - 1))
                let caretLength =
                    if diagnostic.Span.Length = 0 then 1
                    else max 1 (min diagnostic.Span.Length (sourceLine.Length - caretStart))
                builder.AppendLine() |> ignore
                builder.AppendFormat("{0} |", lineNumber) |> ignore
                builder.AppendLine() |> ignore
                builder.AppendFormat("{0} | {1}", gutter, sourceLine) |> ignore
                builder.AppendLine() |> ignore
                builder.AppendFormat("{0} | {1}{2}", gutter, String(' ', caretStart), String('^', caretLength)) |> ignore
                gutter
            else
                String(' ', (string line).Length)
        for label in diagnostic.Labels do
            let labelLine, labelColumn =
                match label.Span.Line, label.Span.Column with
                | Some line, Some column -> line, column
                | _ -> this.LineColumn label.Span.Start
            let message = defaultArg label.Message "related location"
            builder.AppendLine() |> ignore
            builder.AppendFormat("{0} = {1}:{2}: {3}", gutter, labelLine, labelColumn, message) |> ignore
        for note in diagnostic.Notes do
            builder.AppendLine() |> ignore
            builder.AppendFormat("{0} = note: {1}", gutter, note) |> ignore
        if not diagnostic.Trace.IsEmpty then
            builder.AppendLine() |> ignore
            builder.AppendFormat("{0} = stack trace:", gutter) |> ignore
            for frame in diagnostic.Trace do
                builder.AppendLine() |> ignore
                match frame.Span with
                | Some span ->
                    builder.AppendFormat(
                        "{0} =   at {1} ({2})",
                        gutter,
                        frame.Function,
                        this.SpanLocation(path, span)
                    )
                    |> ignore
                | None -> builder.AppendFormat("{0} =   at {1}", gutter, frame.Function) |> ignore
        builder.ToString()
