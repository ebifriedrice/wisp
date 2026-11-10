namespace Wisp.Compiler

open System
open System.IO
open Wisp.Core
open Wisp.Reader

type EvalOutput =
    { Values: obj list
      Diagnostics: Diagnostic list }

type private DefinitionKind =
    | NamespaceDefinition
    | TypeDefinition
    | MacroDefinition
    | FunctionDefinition
    | ValueDefinition
    | OtherDefinition

module private DefinitionForm =
    let asList (form: obj) =
        match form with
        | :? PersistentList as list -> list.Items |> Seq.toList
        | :? ISeq as sequence -> Seqs.toList (sequence :> obj)
        | _ -> []

    let symbolName (form: obj) =
        match form with
        | :? Symbol as symbol -> Some(Clr.symbolTypeName symbol)
        | :? string as text -> Some text
        | :? Keyword as keyword ->
            Some(
                match keyword.Namespace with
                | Some namespaceName -> namespaceName + "." + keyword.Name
                | None -> keyword.Name
            )
        | _ -> None

    let kind (form: obj) =
        match asList form with
        | head :: _ ->
            match symbolName head with
            | Some "ns" -> NamespaceDefinition
            | Some "defclass"
            | Some "deftype"
            | Some "defstruct"
            | Some "definterface"
            | Some "defdelegate"
            | Some "defunion" -> TypeDefinition
            | Some "defmacro" -> MacroDefinition
            | Some "defn"
            | Some "defactive" -> FunctionDefinition
            | Some "def" -> ValueDefinition
            | _ -> OtherDefinition
        | [] -> OtherDefinition

    let tryNamespaceName (form: obj) =
        match asList form with
        | head :: name :: _ when symbolName head = Some "ns" -> symbolName name
        | _ -> None

    let isNamespaceChange (form: obj) =
        match asList form with
        | head :: _ ->
            match symbolName head with
            | Some "ns"
            | Some "in-ns" -> true
            | _ -> false
        | [] -> false

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

    member private this.EvalOne(form: obj) : Result<obj, Diagnostic list> =
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

    member this.EvalEntriesDetailed(entries: (string * obj) list) : EvalOutput * (int * Diagnostic) list =
        let entryArray = entries |> List.toArray
        let count = entryArray.Length
        let kinds = entryArray |> Array.map (fun (_, form) -> DefinitionForm.kind form)
        let values : obj option[] = Array.zeroCreate count
        let diagnostics = ResizeArray<int * Diagnostic>()

        let evaluate index =
            let namespaceName, form = entryArray.[index]
            runtime.CurrentNamespace <- namespaceName
            runtime.Registry.GetOrCreate namespaceName |> ignore
            this.EvalOne form

        let record index result =
            match result with
            | Ok value -> values.[index] <- Some value
            | Error errors -> errors |> List.iter (fun diagnostic -> diagnostics.Add(index, diagnostic))

        for index in 0 .. count - 1 do
            if kinds.[index] = NamespaceDefinition then
                evaluate index |> record index

        let pendingTypes =
            ResizeArray<int>(
                [ for index in 0 .. count - 1 do
                      if kinds.[index] = TypeDefinition then yield index ]
            )
        let lastTypeErrors = Collections.Generic.Dictionary<int, Diagnostic list>()
        let mutable progress = true
        while progress && pendingTypes.Count > 0 do
            progress <- false
            let remaining = ResizeArray<int>()
            for index in pendingTypes do
                match evaluate index with
                | Ok value ->
                    values.[index] <- Some value
                    progress <- true
                | Error errors ->
                    lastTypeErrors.[index] <- errors
                    remaining.Add index
            pendingTypes.Clear()
            pendingTypes.AddRange remaining
        for index in pendingTypes do
            match lastTypeErrors.TryGetValue index with
            | true, errors -> errors |> List.iter (fun diagnostic -> diagnostics.Add(index, diagnostic))
            | false, _ -> ()

        for kindToRun in [ MacroDefinition; FunctionDefinition ] do
            for index in 0 .. count - 1 do
                if kinds.[index] = kindToRun then
                    evaluate index |> record index

        for index in 0 .. count - 1 do
            match kinds.[index] with
            | ValueDefinition
            | OtherDefinition -> evaluate index |> record index
            | _ -> ()

        let orderedDiagnostics =
            diagnostics
            |> Seq.sortBy fst
            |> Seq.map snd
            |> Seq.toList
        { Values = values |> Array.choose id |> Array.toList
          Diagnostics = orderedDiagnostics },
        List.ofSeq diagnostics

    member this.EvalForm(form: obj) : Result<obj, Diagnostic list> =
        let output, _ = this.EvalEntriesDetailed [ runtime.CurrentNamespace, form ]
        match output.Diagnostics with
        | [] ->
            match List.tryLast output.Values with
            | Some value -> Ok value
            | None -> Ok null
        | errors -> Error errors

    member this.EvalText(source: SourceText) : EvalOutput =
        let entries = ResizeArray<string * obj>()
        let diagnostics = ResizeArray<Diagnostic>()
        let mutable offset = 0
        let mutable running = true
        let mutable currentNamespace = runtime.CurrentNamespace

        while running && offset <= source.Length do
            let read = Reader.readFormsFrom source this.ReaderOptions offset
            if not read.Diagnostics.IsEmpty then
                diagnostics.AddRange read.Diagnostics
                running <- false
            elif read.Forms.IsEmpty then
                running <- false
            else
                for form in read.Forms do
                    entries.Add(currentNamespace, form)
                    if DefinitionForm.isNamespaceChange form then
                        let namespaceName, _ = entries.[entries.Count - 1]
                        runtime.CurrentNamespace <- namespaceName
                        runtime.Registry.GetOrCreate namespaceName |> ignore
                        match this.EvalOne form with
                        | Ok _ -> ()
                        | Error _ -> ()
                        currentNamespace <-
                            DefinitionForm.tryNamespaceName form
                            |> Option.defaultValue runtime.CurrentNamespace
                offset <- read.Position

        if diagnostics.Count > 0 then
            { Values = []
              Diagnostics = List.ofSeq diagnostics }
        else
            let output, _ = this.EvalEntriesDetailed(List.ofSeq entries)
            output

    member this.EvalText(text: string) = this.EvalText(SourceText.Create text)
