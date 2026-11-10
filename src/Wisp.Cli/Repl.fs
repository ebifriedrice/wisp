module Wisp.Cli.Repl

open System
open System.IO
open System.Text
open Spectre.Console
open Wisp.Core

let private reset = "\u001b[0m"

let private colorize (code: string) (text: string) (enabled: bool) =
    if enabled && not (String.IsNullOrEmpty text) then code + text + reset else text

let highlight (text: string) (enabled: bool) =
    if not enabled then
        text
    else
        let builder = StringBuilder()
        let mutable index = 0
        while index < text.Length do
            let character = text.[index]
            if character = ';' then
                builder.Append(colorize "\u001b[2;37m" (text.Substring index) enabled) |> ignore
                index <- text.Length
            elif character = '"' then
                let start = index
                index <- index + 1
                let mutable escaped = false
                while index < text.Length && (escaped || text.[index] <> '"') do
                    if escaped then escaped <- false
                    elif text.[index] = '\\' then escaped <- true
                    index <- index + 1
                if index < text.Length then index <- index + 1
                builder.Append(colorize "\u001b[33m" (text.Substring(start, index - start)) enabled) |> ignore
            elif character = '(' || character = ')' || character = '[' || character = ']'
                 || character = '{' || character = '}' then
                builder.Append(colorize "\u001b[1;37m" (string character) enabled) |> ignore
                index <- index + 1
            elif character = ':' && (index = 0 || Char.IsWhiteSpace text.[index - 1] || "([{".Contains(text.[index - 1])) then
                let start = index
                index <- index + 1
                while index < text.Length
                      && not (Char.IsWhiteSpace text.[index])
                      && not ("()[]{}".Contains text.[index]) do
                    index <- index + 1
                builder.Append(colorize "\u001b[36m" (text.Substring(start, index - start)) enabled) |> ignore
            elif Char.IsDigit character then
                let start = index
                index <- index + 1
                while index < text.Length && (Char.IsDigit text.[index] || text.[index] = '.') do
                    index <- index + 1
                builder.Append(colorize "\u001b[35m" (text.Substring(start, index - start)) enabled) |> ignore
            elif character = '\'' || character = '`' || character = '~' || character = '@' || character = '^' then
                builder.Append(colorize "\u001b[34m" (string character) enabled) |> ignore
                index <- index + 1
            else
                builder.Append character |> ignore
                index <- index + 1
        builder.ToString()

let nextIndent (line: string) =
    let mutable depth = 0
    let mutable index = 0
    let mutable inString = false
    let mutable escaped = false
    let mutable inComment = false
    while index < line.Length do
        let character = line.[index]
        if inComment then
            index <- line.Length
        elif inString then
            if escaped then escaped <- false
            elif character = '\\' then escaped <- true
            elif character = '"' then inString <- false
            index <- index + 1
        else
            match character with
            | ';' ->
                inComment <- true
                index <- index + 1
            | '"' ->
                inString <- true
                index <- index + 1
            | '('
            | '['
            | '{' ->
                depth <- depth + 1
                index <- index + 1
            | ')'
            | ']'
            | '}' ->
                depth <- depth - 1
                index <- index + 1
            | _ -> index <- index + 1
    let leading = line.Length - line.TrimStart().Length
    max 0 (leading + depth * 2)

let banner (version: string) (title: string option) (namespaceName: string) =
    let heading =
        title
        |> Option.map (fun title -> sprintf "Wisp %s — %s" version title)
        |> Option.defaultValue (sprintf "Wisp %s" version)
    let rule = Rule("[bold]" + Markup.Escape heading + "[/]")
    rule.Justification <- Justify.Left
    AnsiConsole.Write rule
    AnsiConsole.MarkupLine(
        sprintf "[grey]namespace %s · :help commands · Ctrl-D exits[/]" (Markup.Escape namespaceName)
    )

let info (message: string) =
    AnsiConsole.MarkupLine("[grey]" + Markup.Escape message + "[/]")

let error (message: string) =
    AnsiConsole.MarkupLine("[red]" + Markup.Escape message + "[/]")

let help () =
    let table = Table()
    table.AddColumn("Command") |> ignore
    table.AddColumn("Description") |> ignore
    table.AddRow(":help", "Show this help") |> ignore
    table.AddRow(":reload", "Reload project files") |> ignore
    table.AddRow(":quit", "Exit the REPL") |> ignore
    table.Border <- TableBorder.None
    table.BorderStyle <- Style.Parse "grey"
    table.Columns.[1].Padding <- Padding(2, 0, 0, 0)
    AnsiConsole.Write table

let diagnostic (source: SourceText) (diagnostic: Diagnostic) =
    let color, label =
        match diagnostic.Severity with
        | DiagnosticSeverity.Error -> "red", "error"
        | DiagnosticSeverity.Warning -> "yellow", "warning"
        | DiagnosticSeverity.Information -> "blue", "info"
        | DiagnosticSeverity.Hint -> "grey", "hint"
    let panel = Panel(Text(source.Render diagnostic))
    panel.Border <- BoxBorder.Rounded
    panel.BorderStyle <- Style.Parse color
    panel.Padding <- Padding(1, 0, 1, 0)
    panel.Expand <- true
    panel.Header <- PanelHeader(sprintf "[%s]%s %s[/]" color label diagnostic.Code)
    AnsiConsole.Write panel

type ReadResult =
    | Line of string
    | Cancelled
    | EndOfInput

type LineReader(historyPath: string, color: bool) =
    let history = ResizeArray<string>()
    let mutable historyIndex = 0
    let mutable draft = ""

    do
        if File.Exists historyPath then
            try
                history.AddRange(File.ReadAllLines historyPath |> Array.filter (String.IsNullOrWhiteSpace >> not))
            with _ -> ()
        historyIndex <- history.Count

    member private _.SaveHistory() =
        try
            let directory = Path.GetDirectoryName historyPath
            if not (String.IsNullOrEmpty directory) then
                Directory.CreateDirectory directory |> ignore
            File.WriteAllLines(historyPath, history)
        with _ -> ()

    member private _.AddHistory(line: string) =
        if not (String.IsNullOrWhiteSpace line) then
            if history.Count = 0 || history.[history.Count - 1] <> line then
                history.Add line
                if history.Count > 1000 then history.RemoveAt 0
            historyIndex <- history.Count
            draft <- ""

    member this.Save() = this.SaveHistory()

    member private _.Render(prompt: string, buffer: StringBuilder, cursor: int) =
        let visible = buffer.ToString()
        Console.Write "\r\u001b[2K"
        Console.Write prompt
        Console.Write(highlight visible color)
        Console.Write "\r"
        let column = prompt.Length + cursor
        if column > 0 then
            Console.Write(sprintf "\u001b[%dC" column)

    member this.ReadLine(prompt: string, initial: string) : ReadResult =
        if Console.IsInputRedirected || Console.IsOutputRedirected then
            Console.Write prompt
            match Console.ReadLine() with
            | null -> EndOfInput
            | line ->
                this.AddHistory line
                Line line
        else
            let buffer = StringBuilder(initial)
            let mutable cursor = buffer.Length
            let mutable accepted = false
            let mutable cancelled = false
            let mutable eof = false
            let mutable result : string option = None
            let previous = Console.TreatControlCAsInput
            try
                Console.TreatControlCAsInput <- true
                this.Render(prompt, buffer, cursor)
                while not accepted && not cancelled do
                    let key = Console.ReadKey(true)
                    match key.Key with
                    | ConsoleKey.Enter ->
                        Console.WriteLine()
                        accepted <- true
                        result <- Some(buffer.ToString())
                    | ConsoleKey.Backspace ->
                        if cursor > 0 then
                            buffer.Remove(cursor - 1, 1) |> ignore
                            cursor <- cursor - 1
                    | ConsoleKey.Delete ->
                        if cursor < buffer.Length then
                            buffer.Remove(cursor, 1) |> ignore
                    | ConsoleKey.LeftArrow ->
                        if cursor > 0 then cursor <- cursor - 1
                    | ConsoleKey.RightArrow ->
                        if cursor < buffer.Length then cursor <- cursor + 1
                    | ConsoleKey.Home ->
                        cursor <- 0
                    | ConsoleKey.End ->
                        cursor <- buffer.Length
                    | ConsoleKey.UpArrow ->
                        if history.Count > 0 then
                            if historyIndex = history.Count then
                                draft <- buffer.ToString()
                            if historyIndex > 0 then
                                historyIndex <- historyIndex - 1
                                buffer.Clear() |> ignore
                                buffer.Append(history.[historyIndex]) |> ignore
                                cursor <- buffer.Length
                    | ConsoleKey.DownArrow ->
                        if historyIndex < history.Count then
                            historyIndex <- historyIndex + 1
                            buffer.Clear() |> ignore
                            if historyIndex = history.Count then
                                buffer.Append draft |> ignore
                            else
                                buffer.Append(history.[historyIndex]) |> ignore
                            cursor <- buffer.Length
                    | _ ->
                        match key.KeyChar with
                        | '\u0003' ->
                            Console.WriteLine()
                            cancelled <- true
                        | '\u0004' ->
                            if buffer.Length = 0 then
                                Console.WriteLine()
                                eof <- true
                                cancelled <- true
                            elif cursor < buffer.Length then
                                buffer.Remove(cursor, 1) |> ignore
                        | '\u000c' ->
                            Console.Clear()
                        | '\u0015' ->
                            buffer.Remove(0, cursor) |> ignore
                            cursor <- 0
                        | '\u000b' ->
                            if cursor < buffer.Length then
                                buffer.Remove(cursor, buffer.Length - cursor) |> ignore
                        | '\t' ->
                            buffer.Insert(cursor, "  ") |> ignore
                            cursor <- cursor + 2
                        | character when not (Char.IsControl character) ->
                            buffer.Insert(cursor, character) |> ignore
                            cursor <- cursor + 1
                        | _ -> ()
                    if not accepted && not cancelled then
                        this.Render(prompt, buffer, cursor)
            finally
                Console.TreatControlCAsInput <- previous
            if accepted then
                match result with
                | Some line ->
                    this.AddHistory line
                    Line line
                | None -> EndOfInput
            elif eof then
                EndOfInput
            elif cancelled then
                Cancelled
            else
                EndOfInput
