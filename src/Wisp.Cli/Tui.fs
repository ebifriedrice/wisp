module Wisp.Cli.Tui

#nowarn "44"

open System
open System.IO
open System.Text
open System.Xml
open Terminal.Gui.App
open Terminal.Gui.ViewBase
open Terminal.Gui.Views
open Terminal.Gui.Input
open Terminal.Gui.Editor
open Terminal.Gui.Editor.Document
open Terminal.Gui.Editor.Highlighting
open Terminal.Gui.Editor.Highlighting.Xshd
open Wisp.Core
open Wisp.Reader
open Wisp.Compiler

type BufferWriter() =
    inherit TextWriter()
    let buffer = StringBuilder()

    override _.Encoding = Encoding.UTF8
    override _.Write(value: char) = buffer.Append(value) |> ignore
    override _.Write(value: string) = if not (isNull value) then buffer.Append(value) |> ignore
    override _.WriteLine() = buffer.AppendLine() |> ignore
    override _.WriteLine(value: string) = buffer.AppendLine(value) |> ignore

    member _.Drain() =
        let text = buffer.ToString()
        buffer.Clear() |> ignore
        text

let private wispXshd =
    """<SyntaxDefinition name="Wisp" extensions=".wisp"
    xmlns="http://icsharpcode.net/sharpdevelop/syntaxdefinition/2008">
  <Color name="Comment" foreground="#6A9955" />
  <Color name="String" foreground="#CE9178" />
  <Color name="Number" foreground="#B5CEA8" />
  <Color name="Keyword" foreground="#569CD6" />
  <Color name="Delimiter" foreground="#D4D4D4" />
  <RuleSet>
    <Span color="Comment" begin=";" end="$" />
    <Span color="String" begin="&quot;" end="&quot;" escapeCharacter="\" />
    <Rule color="Number">\b[0-9]+(\.[0-9]+)?\b</Rule>
    <Rule color="Keyword">:[A-Za-z0-9_\-./]+</Rule>
    <Rule color="Delimiter">[\(\)\[\]\{\}]</Rule>
  </RuleSet>
</SyntaxDefinition>"""

let private highlighting () =
    try
        use reader = XmlReader.Create(new StringReader(wispXshd))
        let definition = HighlightingLoader.Load(reader, null)
        HighlightingManager.Instance.RegisterHighlighting("Wisp", [| ".wisp" |], definition)
        Some definition
    with _ ->
        None

let private historyPath () =
    let home = Environment.GetFolderPath Environment.SpecialFolder.UserProfile
    if String.IsNullOrWhiteSpace home then Path.Combine(Path.GetTempPath(), ".wisp_history")
    else Path.Combine(home, ".wisp_history")

let run
    (title: string)
    (getSession: unit -> Session)
    (refresh: (bool -> Result<string list, string list>) option)
    (buffer: BufferWriter)
    =
    use app = Application.Create()
    app.Init() |> ignore
    use window = new Window()
    window.Title <- title

    let output = new TextView()
    output.X <- 0
    output.Y <- 0
    output.Width <- Dim.Fill()
    output.Height <- Dim.Fill(10)
    output.ReadOnly <- true
    output.WordWrap <- true

    let editor = new Terminal.Gui.Editor.Editor()
    editor.X <- 0
    editor.Y <- Pos.Bottom output
    editor.Width <- Dim.Fill()
    editor.Height <- Dim.Fill(1)
    editor.Multiline <- true
    editor.ConvertTabsToSpaces <- true
    editor.IndentationSize <- 2
    match highlighting () with
    | Some definition -> editor.HighlightingDefinition <- definition
    | None -> ()

    let status = new Label()
    status.X <- 0
    status.Y <- Pos.AnchorEnd(1)
    status.Width <- Dim.Fill()
    status.Height <- 1

    window.Add output |> ignore
    window.Add editor |> ignore
    window.Add status |> ignore
    editor.SetFocus() |> ignore

    let history = ResizeArray<string>()
    let mutable historyIndex = 0
    do
        let path = historyPath ()
        if File.Exists path then
            try
                history.AddRange(File.ReadAllLines path |> Array.filter (String.IsNullOrWhiteSpace >> not))
            with _ -> ()
        historyIndex <- history.Count

    let saveHistory () =
        try
            let path = historyPath ()
            let directory = Path.GetDirectoryName path
            if not (String.IsNullOrEmpty directory) then
                Directory.CreateDirectory directory |> ignore
            File.WriteAllLines(path, history)
        with _ -> ()

    let append (text: string) =
        if not (String.IsNullOrEmpty text) then
            output.Text <- output.Text + text
            output.MoveEnd() |> ignore

    let drain () =
        let text = buffer.Drain()
        if not (String.IsNullOrEmpty text) then append text

    let helpText =
        "F5 / Ctrl+Enter evaluate   Alt+Up/Down history   Ctrl+/ help   Ctrl+Q quit\n"

    let updateStatus () =
        status.Text <-
            sprintf
                "%s   F5 evaluate   Alt+Up/Down history   Ctrl+/ help   Ctrl+Q quit"
                ((getSession ()).Runtime.CurrentNamespace)

    let addHistory (text: string) =
        if not (String.IsNullOrWhiteSpace text)
           && (history.Count = 0 || history.[history.Count - 1] <> text) then
            history.Add text
            if history.Count > 1000 then history.RemoveAt 0
        historyIndex <- history.Count

    let refreshNow (force: bool) =
        match refresh with
        | None -> ()
        | Some refresh ->
            match refresh force with
            | Ok changed when not changed.IsEmpty ->
                append (sprintf ";; reloaded %s\n" (changed |> List.map Path.GetFileName |> String.concat ", "))
            | Ok _ -> ()
            | Error errors ->
                for error in errors do append (error + "\n")

    let submit () =
        refreshNow false
        let text = editor.Text
        editor.Text <- ""
        if not (String.IsNullOrWhiteSpace text) then
            let trimmed = text.Trim()
            if trimmed.StartsWith(":", StringComparison.Ordinal) then
                match trimmed with
                | ":quit"
                | ":q"
                | ":exit" -> app.RequestStop()
                | ":help" -> append helpText
                | ":reload" -> refreshNow true
                | command -> append (sprintf "Unknown command %s; try :help\n" command)
            else
                addHistory text
                let session = getSession ()
                append (sprintf "%s> %s\n" session.Runtime.CurrentNamespace text)
                let source = SourceText.Create(text, "<repl>")
                let result = session.EvalText source
                drain ()
                if not result.Diagnostics.IsEmpty then
                    for diagnostic in result.Diagnostics do
                        append (source.Render diagnostic + "\n")
                for value in result.Values do
                    append (Print.printString value + "\n")
        drain ()
        updateStatus ()

    editor.KeyDown.Add(fun key ->
        if key = Key.F5 || key = Key.Enter.WithCtrl then
            submit ()
            key.Handled <- true
        elif key = Key('/').WithCtrl || key = Key('_').WithCtrl then
            append helpText
            key.Handled <- true
        elif key = Key.Q.WithCtrl then
            app.RequestStop()
            key.Handled <- true
        elif key = Key.CursorUp.WithAlt then
            if history.Count > 0 then
                if historyIndex = history.Count then
                    historyIndex <- history.Count - 1
                elif historyIndex > 0 then
                    historyIndex <- historyIndex - 1
                editor.Text <- history.[historyIndex]
            key.Handled <- true
        elif key = Key.CursorDown.WithAlt then
            if historyIndex < history.Count - 1 then
                historyIndex <- historyIndex + 1
                editor.Text <- history.[historyIndex]
            elif historyIndex = history.Count - 1 then
                historyIndex <- history.Count
                editor.Text <- ""
            key.Handled <- true)

    updateStatus ()
    append "Wisp REPL — F5 evaluates the editor, Ctrl+/ shows help, Ctrl+Q quits\n"
    drain ()
    app.Run window |> ignore
    saveHistory ()
