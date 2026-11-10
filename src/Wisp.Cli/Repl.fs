module Wisp.Cli.Repl

open System
open System.IO
open System.Text

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

    member this.ReadLine(prompt: string, initial: string) : string option =
        if Console.IsInputRedirected || Console.IsOutputRedirected then
            Console.Write prompt
            match Console.ReadLine() with
            | null -> None
            | line ->
                this.AddHistory line
                Some line
        else
            let buffer = StringBuilder(initial)
            let mutable cursor = buffer.Length
            let mutable accepted = false
            let mutable cancelled = false
            let mutable eof = false
            let mutable result : string option = None
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
            if accepted then
                match result with
                | Some line ->
                    this.AddHistory line
                    Some line
                | None -> None
            elif eof then
                None
            elif cancelled then
                Some ""
            else
                None
