module Wisp.Cli.Program

open System
open System.IO
open System.Text
open Wisp.Core
open Wisp.Reader
open Wisp.Compiler

let private version = "0.1.2-bootstrap"

let private usage () =
    printfn "wisp %s" version
    printfn ""
    printfn "Usage:"
    printfn "  wisp <file.wisp>          Evaluate a source file"
    printfn "  wisp build [project]      Build a Wisp project"
    printfn "  wisp run [project]        Run a Wisp project"
    printfn "  wisp repl [project]       Start the REPL, optionally with a project"
    printfn "  wisp init [dir]           Create a project from a template"
    printfn "  wisp new <name>           Create a directory and project from a template"
    printfn "  wisp templates            List project templates"
    printfn "  wisp --eval <form>        Evaluate a form and print its value"
    printfn "  wisp                      Start the REPL"
    printfn "  wisp --help"

let private renderDiagnostics (source: SourceText) (diagnostics: Diagnostic list) =
    for diagnostic in diagnostics do
        eprintfn "%s" (source.Render diagnostic)

let private hasIncompleteDiagnostic (diagnostics: Diagnostic list) =
    diagnostics |> List.exists (fun diagnostic -> diagnostic.Code = ReaderDiagnostics.UnexpectedEof)

let private evaluate (session: Session) (source: SourceText) (printValues: bool) =
    let result = session.EvalText source
    if not result.Diagnostics.IsEmpty then
        renderDiagnostics source result.Diagnostics
        1
    else
        if printValues then
            match List.tryLast result.Values with
            | Some value -> printfn "%s" (Print.printString value)
            | None -> ()
        0

let private runFile (path: string) =
    if not (File.Exists path) then
        eprintfn "wisp: file not found: %s" path
        1
    else
        let text = File.ReadAllText path
        let source = SourceText.Create(text, path)
        evaluate (Session()) source false

let private findProjectFile (path: string) =
    let path = Path.GetFullPath path
    if File.Exists path then
        Some path
    elif Directory.Exists path then
        let direct = Path.Combine(path, "wisp.proj")
        if File.Exists direct then
            Some direct
        else
            Directory.GetFiles(path, "*.wisp.proj")
            |> Array.sort
            |> Array.tryHead
    else
        None

let private runProject (path: string) (execute: bool) =
    match findProjectFile path with
    | None ->
        eprintfn "wisp: project file not found: %s" path
        1
    | Some projectPath ->
        match Project.run projectPath Console.Out execute with
        | Ok result ->
            if not execute then
                printfn "Build succeeded: %s" result.Project.Name
            else
                match result.LastValue with
                | Some value when not (isNull value) -> printfn "%s" (Print.printString value)
                | _ -> ()
            0
        | Error errors ->
            for error in errors do eprintfn "%s" error
            1

let private historyPath () =
    let home = Environment.GetFolderPath Environment.SpecialFolder.UserProfile
    if String.IsNullOrWhiteSpace home then Path.Combine(Path.GetTempPath(), ".wisp_history")
    else Path.Combine(home, ".wisp_history")

let private watchProject (state: ProjectSession) (dirty: bool ref) =
    let watchers = ResizeArray<FileSystemWatcher>()
    let directories =
        state.Projects
        |> List.map (fun project -> project.Directory)
        |> List.distinct
    for directory in directories do
        if Directory.Exists directory then
            let watcher = new FileSystemWatcher(directory, "*.*")
            watcher.IncludeSubdirectories <- true
            watcher.NotifyFilter <-
                NotifyFilters.LastWrite
                ||| NotifyFilters.FileName
                ||| NotifyFilters.DirectoryName
                ||| NotifyFilters.Size
            let changed (_: obj) (args: FileSystemEventArgs) =
                if args.FullPath.EndsWith(".wisp", StringComparison.Ordinal)
                   || args.FullPath.EndsWith(".wisp.proj", StringComparison.Ordinal) then
                    dirty.Value <- true
            let renamed (_: obj) (_: RenamedEventArgs) = dirty.Value <- true
            watcher.Changed.AddHandler(FileSystemEventHandler changed)
            watcher.Created.AddHandler(FileSystemEventHandler changed)
            watcher.Deleted.AddHandler(FileSystemEventHandler changed)
            watcher.Renamed.AddHandler(RenamedEventHandler renamed)
            watcher.EnableRaisingEvents <- true
            watchers.Add watcher
    watchers

let private runRepl (getSession: unit -> Session) (refresh: (bool -> Result<string list, string list>) option) =
    let color = not Console.IsOutputRedirected
    let session = getSession ()
    printfn "Wisp %s — Ctrl-D to exit, :help for commands" version
    printfn "namespace: %s" session.Runtime.CurrentNamespace
    let reader = Repl.LineReader(historyPath (), color)
    let buffer = StringBuilder()
    let mutable running = true
    let mutable nextIndent = 0

    let refreshNow () =
        match refresh with
        | None -> ()
        | Some refresh ->
            match refresh false with
            | Ok changed when not changed.IsEmpty ->
                printfn ";; reloaded %s" (changed |> List.map Path.GetFileName |> String.concat ", ")
            | Ok _ -> ()
            | Error errors ->
                for error in errors do eprintfn "%s" error

    while running do
        refreshNow ()
        let session = getSession ()
        let prompt = if buffer.Length = 0 then session.Runtime.CurrentNamespace + "> " else "...> "
        match reader.ReadLine(prompt, String(' ', nextIndent)) with
        | None -> running <- false
        | Some line ->
            if buffer.Length = 0 && line.StartsWith(":", StringComparison.Ordinal) then
                match line.Trim() with
                | ":quit"
                | ":q"
                | ":exit" -> running <- false
                | ":help" ->
                    printfn ":help  :reload  :quit"
                | ":reload" ->
                    match refresh with
                    | Some refresh ->
                        match refresh true with
                        | Ok _ -> printfn ";; project reloaded"
                        | Error errors -> for error in errors do eprintfn "%s" error
                    | None -> printfn ";; no project loaded"
                | command ->
                    printfn "Unknown command %s; try :help" command
            elif String.IsNullOrWhiteSpace line && buffer.Length = 0 then
                ()
            else
                if buffer.Length > 0 then buffer.AppendLine() |> ignore
                buffer.Append(line) |> ignore
                nextIndent <- Repl.nextIndent line
                let text = buffer.ToString()
                let source = SourceText.Create(text, "<repl>")
                let session = getSession ()
                let result = session.EvalText source
                if not result.Diagnostics.IsEmpty then
                    if hasIncompleteDiagnostic result.Diagnostics then
                        ()
                    else
                        renderDiagnostics source result.Diagnostics
                        buffer.Clear() |> ignore
                        nextIndent <- 0
                elif String.IsNullOrWhiteSpace text then
                    buffer.Clear() |> ignore
                    nextIndent <- 0
                else
                    for value in result.Values do
                        printfn "%s" (Print.printString value)
                    buffer.Clear() |> ignore
                    nextIndent <- 0

    reader.Save()
    if buffer.Length > 0 && not (String.IsNullOrWhiteSpace(buffer.ToString())) then
        let source = SourceText.Create(buffer.ToString(), "<repl>")
        let read = Reader.readFormsFrom source (getSession()).ReaderOptions 0
        if not read.Diagnostics.IsEmpty then renderDiagnostics source read.Diagnostics
    0

let private runProjectRepl (path: string) =
    let interactive = not Console.IsInputRedirected && not Console.IsOutputRedirected
    match findProjectFile path with
    | None ->
        eprintfn "wisp: project file not found: %s" path
        1
    | Some projectPath ->
        let buffer = if interactive then Some(new Tui.BufferWriter()) else None
        let output = buffer |> Option.map (fun writer -> writer :> TextWriter) |> Option.defaultValue Console.Out
        match Project.openSession projectPath output with
        | Error errors ->
            for error in errors do eprintfn "%s" error
            1
        | Ok state ->
            if not interactive then
                printfn "Loaded project %s" state.Root.Name
            let session = state.Session
            session.Runtime.CurrentNamespace <- state.Root.RootNamespace
            let dirty = ref false
            let watchers = watchProject state dirty
            let refresh (force: bool) =
                if force || dirty.Value then
                    dirty.Value <- false
                    let result = Project.refresh state
                    state.Session.Runtime.CurrentNamespace <- state.Root.RootNamespace
                    result
                else
                    Ok []
            try
                match buffer with
                | Some buffer ->
                    Tui.run
                        (sprintf "Wisp — %s" state.Root.Name)
                        (fun () -> state.Session)
                        (Some refresh)
                        buffer
                | None -> runRepl (fun () -> state.Session) (Some refresh) |> ignore
            finally
                for watcher in watchers do
                    watcher.EnableRaisingEvents <- false
                    watcher.Dispose()

            0

let private parseTemplateOptions (arguments: string list) =
    let mutable template = "app"
    let mutable name = None
    let mutable force = false
    let positional = ResizeArray<string>()
    let rec loop remaining =
        match remaining with
        | [] -> ()
        | "--template" :: value :: rest ->
            template <- value
            loop rest
        | "--name" :: value :: rest ->
            name <- Some value
            loop rest
        | "--force" :: rest ->
            force <- true
            loop rest
        | value :: rest ->
            positional.Add value
            loop rest
    loop arguments
    template, name, force, positional |> Seq.toList

let private initProject (arguments: string list) (newProject: bool) =
    let template, name, force, positional = parseTemplateOptions arguments
    let directory =
        if newProject then
            match positional with
            | [ value ] -> value
            | _ ->
                eprintfn "wisp: new expects a directory name"
                ""
        else
            match positional with
            | [] -> "."
            | [ value ] -> value
            | _ ->
                eprintfn "wisp: init expects at most one directory"
                ""
    if String.IsNullOrWhiteSpace directory then
        2
    else
        match Templates.create directory name template force with
        | Ok files ->
            printfn "Created %s project in %s" template (Path.GetFullPath directory)
            for file in files do printfn "  %s" (Path.GetRelativePath(Path.GetFullPath directory, file))
            0
        | Error errors ->
            for error in errors do eprintfn "wisp: %s" error
            1

let private listTemplates () =
    for template in Templates.list do
        printfn "%-10s %s" template.Name template.Description
    0

[<EntryPoint>]
let main argv =
    match List.ofArray argv with
    | [ "--help" ]
    | [ "-h" ] ->
        usage ()
        0
    | [ "--version" ] ->
        printfn "%s" version
        0
    | [ "--eval"; code ]
    | [ "-e"; code ] ->
        let source = SourceText.Create(code, "<eval>")
        evaluate (Session()) source true
    | [ "build" ] ->
        runProject "." false
    | [ "build"; path ] ->
        runProject path false
    | [ "run" ] ->
        runProject "." true
    | [ "run"; path ] ->
        runProject path true
    | [ "repl" ] ->
        if not Console.IsInputRedirected && not Console.IsOutputRedirected then
            let buffer = new Tui.BufferWriter()
            let session = Session(output = buffer)
            Tui.run "Wisp" (fun () -> session) None buffer
            0
        else
            let session = Session(renderMarkdown = not Console.IsOutputRedirected)
            runRepl (fun () -> session) None
    | [ "repl"; path ] ->
        runProjectRepl path
    | [ "templates" ] ->
        listTemplates ()
    | "init" :: arguments ->
        initProject arguments false
    | "new" :: arguments ->
        initProject arguments true
    | [ path ] when not (path.StartsWith("-", StringComparison.Ordinal)) ->
        runFile path
    | [] ->
        if not Console.IsInputRedirected && not Console.IsOutputRedirected then
            let buffer = new Tui.BufferWriter()
            let session = Session(output = buffer)
            Tui.run "Wisp" (fun () -> session) None buffer
            0
        else
            let session = Session(renderMarkdown = not Console.IsOutputRedirected)
            runRepl (fun () -> session) None
    | _ ->
        usage ()
        2
