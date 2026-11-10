module Wisp.Cli.Program

open System
open System.IO
open System.Text
open Wisp.Core
open Wisp.Reader
open Wisp.Compiler

let private version = "0.1.0"

let private usage () =
    printfn "wisp %s" version
    printfn ""
    printfn "Usage:"
    printfn "  wisp <file.wisp>          Evaluate a source file"
    printfn "  wisp build [project]      Build a Wisp project"
    printfn "  wisp run [project]        Run a Wisp project"
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

let private runRepl () =
    let session = Session(renderMarkdown = not Console.IsOutputRedirected)
    printfn "Wisp %s — Ctrl-D to exit" version
    let buffer = StringBuilder()
    let mutable running = true
    while running do
        if buffer.Length = 0 then Console.Write "wisp> " else Console.Write "...> "
        match Console.ReadLine() with
        | null ->
            running <- false
        | line ->
            if buffer.Length > 0 then buffer.AppendLine() |> ignore
            buffer.Append(line) |> ignore
            let mutable processing = true
            while processing && buffer.Length > 0 do
                let text = buffer.ToString()
                let source = SourceText.Create(text, "<repl>")
                let read = Reader.readFormsFrom source session.ReaderOptions 0
                if not read.Diagnostics.IsEmpty then
                    if hasIncompleteDiagnostic read.Diagnostics then
                        processing <- false
                    else
                        renderDiagnostics source read.Diagnostics
                        buffer.Clear() |> ignore
                        processing <- false
                elif read.Forms.IsEmpty then
                    buffer.Clear() |> ignore
                    processing <- false
                else
                    for form in read.Forms do
                        match session.EvalForm form with
                        | Ok value -> printfn "%s" (Print.printString value)
                        | Error diagnostics -> renderDiagnostics source diagnostics
                    let remainder = text.Substring(read.Position)
                    buffer.Clear() |> ignore
                    buffer.Append(remainder) |> ignore
                    if String.IsNullOrWhiteSpace remainder then processing <- false

    if buffer.Length > 0 && not (String.IsNullOrWhiteSpace(buffer.ToString())) then
        let source = SourceText.Create(buffer.ToString(), "<repl>")
        let read = Reader.readFormsFrom source session.ReaderOptions 0
        if not read.Diagnostics.IsEmpty then renderDiagnostics source read.Diagnostics
    0

[<EntryPoint>]
let main argv =
    match argv with
    | [| "--help" |]
    | [| "-h" |] ->
        usage ()
        0
    | [| "--version" |] ->
        printfn "%s" version
        0
    | [| "--eval"; code |]
    | [| "-e"; code |] ->
        let source = SourceText.Create(code, "<eval>")
        evaluate (Session()) source true
    | [| "build" |] ->
        runProject "." false
    | [| "build"; path |] ->
        runProject path false
    | [| "run" |] ->
        runProject "." true
    | [| "run"; path |] ->
        runProject path true
    | [| path |] when not (path.StartsWith("-", StringComparison.Ordinal)) ->
        runFile path
    | [| |] -> runRepl ()
    | _ ->
        usage ()
        2
