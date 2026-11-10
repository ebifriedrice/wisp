namespace Wisp.Compiler

open System
open System.IO
open Wisp.Core
open Wisp.Reader

type EvalOutput =
    { Values: obj list
      Diagnostics: Diagnostic list }

type Session
    (?registry: NamespaceRegistry, ?output: TextWriter, ?backend: ExecutionBackend, ?renderMarkdown: bool, ?forceCompilerAsync: bool)
    =
    let registry = defaultArg registry (NamespaceRegistry())
    let output = defaultArg output Console.Out
    let backend = defaultArg backend ExecutionBackend.ReflectionEmit
    let runtime = RuntimeState(registry, output)
    let codegen = lazy (Codegen.CodegenContext runtime)
    let mutable lastExpansions: MacroExpansion list = []
    do runtime.Builtins <- Builtins.create runtime
    do runtime.RenderMarkdown <- defaultArg renderMarkdown false
    do runtime.ForceCompilerAsync <- defaultArg forceCompilerAsync false

    let toDiagnostic (error: CompilerError) =
        let diagnostic =
            Diagnostic.create
                error.Code
                DiagnosticSeverity.Error
                error.Message
                (error.Span |> Option.defaultValue TextSpan.Zero)
        if error.Notes.IsEmpty then diagnostic else { diagnostic with Notes = error.Notes }

    let expansionNotes () =
        lastExpansions
        |> List.map (fun expansion ->
            sprintf "macro %s: %s => %s"
                expansion.MacroName
                (Print.printString expansion.Invocation)
                (Print.printString expansion.Result))

    let withExpansionNotes (diagnostics: Diagnostic list) =
        if lastExpansions.IsEmpty then
            diagnostics
        else
            diagnostics
            |> List.map (fun diagnostic ->
                { diagnostic with Notes = diagnostic.Notes @ expansionNotes () })

    let resolveTypeName (name: string) =
        match runtime.Registry.TryFind runtime.CurrentNamespace with
        | Some namespaceValue ->
            match namespaceValue.TryResolveImport name with
            | Some fullName -> TypeHints.tryResolveName fullName
            | None -> TypeHints.tryResolveName name
        | None -> TypeHints.tryResolveName name

    let desugarPass =
        { Name = "desugar"
          Run =
            fun (form: obj) ->
                let context = Desugar.createContext runtime.CurrentNamespace resolveTypeName
                Desugar.desugar context form }

    let validatePass =
        { Name = "validate-recur"
          Run = Validate.validate }

    let optimizePass =
        { Name = "optimize"
          Run = fun (expression: CoreExpr) -> Ok(Optimize.optimize expression) }

    member _.Runtime = runtime
    member _.Backend = backend
    member _.CurrentNamespace = runtime.CurrentNamespace
    member _.LastMacroExpansions = lastExpansions

    member _.ReaderOptions =
        { ReaderOptions.Default with
            CurrentNamespace = runtime.CurrentNamespace
            ResolveAlias = fun alias -> registry.ResolveAlias(runtime.CurrentNamespace, alias) }

    member _.Compile(form: obj) : Result<CoreExpr, Diagnostic list> =
        lastExpansions <- []
        match MacroExpander.expand runtime runtime.CurrentNamespace form with
        | Error errors -> Error(errors |> List.map toDiagnostic)
        | Ok expanded ->
            lastExpansions <- expanded.Expansions
            match
                Pass.run desugarPass expanded.Form
                |> Result.bind (Pass.run validatePass)
                |> Result.bind (Pass.run optimizePass)
            with
            | Ok expression -> Ok expression
            | Error errors -> Error(errors |> List.map toDiagnostic |> withExpansionNotes)

    member this.CompileText(text: string) : Result<CoreExpr, Diagnostic list> =
        let source = SourceText.Create text
        let read = Reader.readOneFrom source this.ReaderOptions 0
        if not read.Diagnostics.IsEmpty then
            Error read.Diagnostics
        else
            match read.Form with
            | Some form -> this.Compile form
            | None ->
                Error
                    [ Diagnostic.create
                          "WISP3000"
                          DiagnosticSeverity.Error
                          "Expected one form to compile."
                          TextSpan.Zero ]

    member this.EvalForm(form: obj) : Result<obj, Diagnostic list> =
        // Record instances capture this ambient runtime so emitted interface
        // stubs can reach the session's defmethod table.
        let previous = RecordRuntimeContext.getCurrent ()
        RecordRuntimeContext.setCurrent (box runtime)
        try
            this.EvalFormCore form
        finally
            RecordRuntimeContext.setCurrent previous

    member private this.EvalFormCore(form: obj) : Result<obj, Diagnostic list> =
        match this.Compile form with
        | Error diagnostics -> Error diagnostics
        | Ok expression ->
            try
                let value =
                    match backend with
                    | ExecutionBackend.Interpreter ->
                        Eval.eval runtime expression Map.empty runtime.CurrentNamespace
                    | ExecutionBackend.ReflectionEmit ->
                        let compiled = codegen.Value.Compile(expression, runtime.CurrentNamespace)
                        compiled.Invoke runtime
                Ok value
            with
            | :? EvaluationException as error ->
                Error(
                    [ Diagnostic.create
                          "WISP3001"
                          DiagnosticSeverity.Error
                          error.Message
                          (error.Span
                           |> Option.orElse expression.Span
                           |> Option.defaultValue TextSpan.Zero) ]
                    |> withExpansionNotes
                )
            | error ->
                Error(
                    [ Diagnostic.create
                          "WISP3001"
                          DiagnosticSeverity.Error
                          error.Message
                          (expression.Span |> Option.defaultValue TextSpan.Zero) ]
                    |> withExpansionNotes
                )

    member this.EvalText(source: SourceText) : EvalOutput =
        let values = ResizeArray<obj>()
        let diagnostics = ResizeArray<Diagnostic>()
        let mutable offset = 0
        let mutable running = true

        while running && offset <= source.Length do
            let read = Reader.readFormsFrom source this.ReaderOptions offset
            if not read.Diagnostics.IsEmpty then
                diagnostics.AddRange read.Diagnostics
                running <- false
            elif read.Forms.IsEmpty then
                running <- false
            else
                let mutable failed = false
                for form in read.Forms do
                    if not failed then
                        match this.EvalForm form with
                        | Ok value -> values.Add value
                        | Error errors ->
                            diagnostics.AddRange errors
                            failed <- true
                if failed then
                    running <- false
                else
                    offset <- read.Position

        { Values = values |> Seq.toList
          Diagnostics = diagnostics |> Seq.toList }

    member this.EvalText(text: string) = this.EvalText(SourceText.Create text)
