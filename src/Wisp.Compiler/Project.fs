namespace Wisp.Compiler

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Xml.Linq
open Wisp.Core
open Wisp.Reader

type WispProject =
    { Name: string
      Path: string
      Directory: string
      OutputType: string
      RootNamespace: string
      StartupObject: string option
      SourcePatterns: string list
      ProjectReferences: string list }

type ProjectSource =
    { Project: WispProject
      Path: string
      RelativePath: string
      Text: string
      NamespaceName: string
      Dependencies: string list }

type ProjectRunResult =
    { Project: WispProject
      Session: Session
      Diagnostics: string list
      LastValue: obj option }

type ProjectSession =
    { RootPath: string
      Output: TextWriter
      mutable Root: WispProject
      mutable Projects: WispProject list
      mutable Sources: ProjectSource list
      mutable Session: Session
      mutable Texts: Map<string, string>
      mutable LastValue: obj option }

module Project =
    type private FormKind =
        | NamespaceForm
        | TypeForm
        | MacroForm
        | FunctionForm
        | ValueForm
        | OtherForm

    let private elementName (name: string) = XName.Get name

    let private elements (document: XDocument) (name: string) =
        document.Descendants(elementName name) |> Seq.toList

    let private property (document: XDocument) (name: string) =
        elements document name
        |> List.tryHead
        |> Option.map (fun element -> element.Value.Trim())
        |> Option.filter (String.IsNullOrWhiteSpace >> not)

    let private includeValues (document: XDocument) (name: string) =
        elements document name
        |> List.choose (fun element ->
            match element.Attribute(XName.Get "Include") with
            | null -> None
            | attribute ->
                Some(
                    attribute.Value.Split([| ';' |], StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map (fun value -> value.Trim())
                    |> Array.toList
                ))
        |> List.collect id

    let private defaultName (path: string) =
        let fileName = Path.GetFileName path
        if fileName.EndsWith(".wisp.proj", StringComparison.OrdinalIgnoreCase) then
            fileName.Substring(0, fileName.Length - ".wisp.proj".Length)
        elif fileName.EndsWith(".proj", StringComparison.OrdinalIgnoreCase) then
            fileName.Substring(0, fileName.Length - ".proj".Length)
        else Path.GetFileNameWithoutExtension path

    let private loadProject (path: string) : Result<WispProject, string list> =
        let path = Path.GetFullPath path
        if not (File.Exists path) then
            Error [ sprintf "%s: project file not found." path ]
        else
            try
                let document = XDocument.Load path
                let directory = Path.GetDirectoryName path
                let name = property document "Name" |> Option.defaultValue (defaultName path)
                let rootNamespace = property document "RootNamespace" |> Option.defaultValue name
                let sources =
                    match includeValues document "Wisp" with
                    | [] -> [ "**/*.wisp" ]
                    | patterns -> patterns
                let references =
                    includeValues document "ProjectReference"
                    |> List.map (fun includePath ->
                        if Path.IsPathRooted includePath then Path.GetFullPath includePath
                        else Path.GetFullPath(Path.Combine(directory, includePath)))
                Ok
                    { Name = name
                      Path = path
                      Directory = directory
                      OutputType = property document "OutputType" |> Option.defaultValue "Library"
                      RootNamespace = rootNamespace
                      StartupObject = property document "StartupObject"
                      SourcePatterns = sources
                      ProjectReferences = references }
            with error ->
                Error [ sprintf "%s: %s" path error.Message ]

    let loadGraph (path: string) : Result<WispProject list, string list> =
        let rec visit (visiting: Set<string>) (loaded: Map<string, WispProject>) (path: string) =
            let path = Path.GetFullPath path
            if loaded.ContainsKey path then
                Ok(loaded, [])
            elif visiting.Contains path then
                Error [ sprintf "Project reference cycle at %s." path ]
            else
                match loadProject path with
                | Error errors -> Error errors
                | Ok project ->
                    let rec loadReferences (state: Map<string, WispProject>) (ordered: WispProject list) remaining =
                        match remaining with
                        | [] -> Ok(state, ordered)
                        | reference :: rest ->
                            match visit (visiting.Add path) state reference with
                            | Error errors -> Error errors
                            | Ok(nextState, newlyLoaded) ->
                                loadReferences nextState (ordered @ newlyLoaded) rest
                    loadReferences loaded [] project.ProjectReferences
                    |> Result.map (fun (state, dependencies) -> state.Add(path, project), dependencies @ [ project ])

        visit Set.empty Map.empty path |> Result.map snd

    let private globToRegex (pattern: string) =
        let pattern = pattern.Replace('\\', '/')
        let builder = StringBuilder("^")
        let mutable index = 0
        while index < pattern.Length do
            match pattern.[index] with
            | '*' when index + 1 < pattern.Length && pattern.[index + 1] = '*' ->
                if index + 2 < pattern.Length && pattern.[index + 2] = '/' then
                    builder.Append("(?:.*/)?") |> ignore
                    index <- index + 3
                else
                    builder.Append(".*") |> ignore
                    index <- index + 2
            | '*' ->
                builder.Append("[^/]*") |> ignore
                index <- index + 1
            | '?' ->
                builder.Append("[^/]") |> ignore
                index <- index + 1
            | character ->
                builder.Append(Regex.Escape(string character)) |> ignore
                index <- index + 1
        builder.Append('$') |> ignore
        Regex(builder.ToString())

    let private excluded (relativePath: string) =
        let segments = relativePath.Replace('\\', '/').Split('/')
        segments
        |> Array.exists (fun segment ->
            segment = "bin"
            || segment = "obj"
            || segment = ".git"
            || segment = "node_modules")

    let private expandSources (project: WispProject) : Result<ProjectSource list, string list> =
        try
            let files =
                Directory.EnumerateFiles(project.Directory, "*", SearchOption.AllDirectories)
                |> Seq.map (fun file -> Path.GetFullPath file)
                |> Seq.filter (fun file ->
                    let relative = Path.GetRelativePath(project.Directory, file) |> fun path -> path.Replace('\\', '/')
                    not (excluded relative))
                |> Seq.toList

            let matched =
                project.SourcePatterns
                |> List.collect (fun pattern ->
                    if pattern.Contains "*" || pattern.Contains "?" then
                        let regex = globToRegex pattern
                        files
                        |> List.filter (fun file ->
                            let relative = Path.GetRelativePath(project.Directory, file).Replace('\\', '/')
                            regex.IsMatch relative)
                    else
                        let file =
                            if Path.IsPathRooted pattern then Path.GetFullPath pattern
                            else Path.GetFullPath(Path.Combine(project.Directory, pattern))
                        if File.Exists file then [ file ] else [])
                |> List.distinct
                |> List.sortWith (fun left right ->
                    StringComparer.Ordinal.Compare(
                        Path.GetRelativePath(project.Directory, left),
                        Path.GetRelativePath(project.Directory, right)
                    ))

            matched
            |> List.map (fun file ->
                { Project = project
                  Path = file
                  RelativePath = Path.GetRelativePath(project.Directory, file).Replace('\\', '/')
                  Text = File.ReadAllText file
                  NamespaceName = project.RootNamespace
                  Dependencies = [] })
            |> Ok
        with error ->
            Error [ sprintf "%s: %s" project.Path error.Message ]

    let private asList (form: obj) =
        match form with
        | :? PersistentList as list -> list.Items |> Seq.toList
        | :? ISeq as sequence -> Seqs.toList (sequence :> obj)
        | _ -> []

    let private symbolName (form: obj) =
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

    let private namespaceNameOfForm (form: obj) =
        match asList form with
        | head :: name :: _ when symbolName head = Some "ns" -> symbolName name
        | _ -> None

    let private dependenciesOfNs (form: obj) =
        let specsFromClause clause =
            match asList clause with
            | head :: specs when
                (match head with
                 | :? Keyword as keyword ->
                     keyword.Namespace.IsNone && (keyword.Name = "require" || keyword.Name = "use")
                 | _ -> false) ->
                specs
                |> List.choose (fun spec ->
                    match spec with
                    | :? PersistentVector as vector ->
                        vector.Elements |> Seq.tryHead |> Option.bind symbolName
                    | _ -> symbolName spec)
            | _ -> []

        match asList form with
        | head :: _name :: clauses when symbolName head = Some "ns" ->
            clauses |> List.collect specsFromClause |> List.distinct
        | _ -> []

    let private readForms (source: ProjectSource) (currentNamespace: string) =
        let text = SourceText.Create(source.Text, source.Path)
        let options =
            { ReaderOptions.Default with
                CurrentNamespace = currentNamespace }
        let rec loop offset (forms: obj list) =
            let read = Reader.readFormsFrom text options offset
            if not read.Diagnostics.IsEmpty then
                Error read.Diagnostics
            elif read.Forms.IsEmpty then
                Ok(List.rev forms)
            else
                loop read.Position (List.rev read.Forms @ forms)
        loop 0 []

    let private scanSources (projects: WispProject list) =
        let mutable errors : string list = []
        let sources =
            projects
            |> List.collect (fun project ->
                match expandSources project with
                | Error projectErrors ->
                    errors <- errors @ projectErrors
                    []
                | Ok projectSources -> projectSources)
            |> List.map (fun source ->
                match readForms { source with NamespaceName = source.Project.RootNamespace } source.Project.RootNamespace with
                | Error diagnostics ->
                    let rendered =
                        diagnostics
                        |> List.map (fun diagnostic -> SourceText.Create(source.Text, source.Path).Render diagnostic)
                    errors <- errors @ rendered
                    source
                | Ok forms ->
                    let namespaceForm =
                        forms
                        |> List.tryFind (fun form -> namespaceNameOfForm form |> Option.isSome)
                    let namespaceName =
                        namespaceForm
                        |> Option.bind namespaceNameOfForm
                        |> Option.defaultValue source.Project.RootNamespace
                    let dependencies =
                        namespaceForm |> Option.map dependenciesOfNs |> Option.defaultValue []
                    { source with NamespaceName = namespaceName; Dependencies = dependencies })
        match errors with
        | [] -> Ok sources
        | errors -> Error errors

    let private orderSources (sources: ProjectSource list) =
        let namespaceOrder =
            let groups =
                sources
                |> List.groupBy (fun source -> source.NamespaceName)
                |> List.map (fun (name, values) -> name, values)
            let dependencies =
                groups
                |> List.map (fun (name, values) ->
                    name,
                    values
                    |> List.collect (fun source -> source.Dependencies)
                    |> List.filter (fun dependency -> groups |> List.exists (fun (other, _) -> other = dependency))
                    |> List.distinct)
                |> Map.ofList
            let mutable visited = Set.empty
            let mutable visiting = Set.empty
            let mutable ordered = []
            let rec visit name =
                if visiting.Contains name then ()
                elif not (visited.Contains name) then
                    visiting <- visiting.Add name
                    dependencies |> Map.tryFind name |> Option.defaultValue [] |> List.iter visit
                    visiting <- visiting.Remove name
                    visited <- visited.Add name
                    ordered <- ordered @ [ name ]
            for name, _ in groups do
                visit name
            ordered
        let orderIndex name =
            namespaceOrder
            |> List.tryFindIndex ((=) name)
            |> Option.defaultValue Int32.MaxValue
        sources |> List.sortBy (fun source -> orderIndex source.NamespaceName)

    let private formKind (form: obj) =
        match asList form with
        | head :: _ ->
            match symbolName head with
            | Some "ns" -> NamespaceForm
            | Some "defclass"
            | Some "deftype"
            | Some "defstruct"
            | Some "definterface"
            | Some "defdelegate"
            | Some "defunion" -> TypeForm
            | Some "defmacro" -> MacroForm
            | Some "defn"
            | Some "defactive" -> FunctionForm
            | Some "def" -> ValueForm
            | _ -> OtherForm
        | [] -> OtherForm

    let private setNamespace (session: Session) (namespaceName: string) =
        session.Runtime.CurrentNamespace <- namespaceName
        session.Runtime.Registry.GetOrCreate namespaceName |> ignore

    let private evaluateSources (session: Session) (sources: ProjectSource list) =
        let diagnostics = ResizeArray<string>()
        let mutable lastValue = None
        for source in sources do
            setNamespace session source.NamespaceName
            let text = SourceText.Create(source.Text, source.Path)
            let result = session.EvalText text
            for diagnostic in result.Diagnostics do
                diagnostics.Add(text.Render diagnostic)
            match List.tryLast result.Values with
            | Some value -> lastValue <- Some value
            | None -> ()
        List.ofSeq diagnostics, lastValue

    let private buildSession (sources: ProjectSource list) (output: TextWriter) =
        let session = Session(output = output)
        let diagnostics, lastValue = evaluateSources session sources
        if diagnostics.IsEmpty then
            Ok(session, lastValue)
        else
            Error diagnostics

    let openSession (path: string) (output: TextWriter) : Result<ProjectSession, string list> =
        match loadGraph path with
        | Error errors -> Error errors
        | Ok projects ->
            match scanSources projects with
            | Error errors -> Error errors
            | Ok sources ->
                let ordered = orderSources sources
                match buildSession ordered output with
                | Error errors -> Error errors
                | Ok(session, lastValue) ->
                    Ok
                        { RootPath = Path.GetFullPath path
                          Output = output
                          Root = List.last projects
                          Projects = projects
                          Sources = ordered
                          Session = session
                          Texts = ordered |> List.map (fun source -> source.Path, source.Text) |> Map.ofList
                          LastValue = lastValue }

    let private projectSignature (project: WispProject) =
        project.Path,
        project.Name,
        project.RootNamespace,
        project.OutputType,
        project.StartupObject,
        project.SourcePatterns,
        project.ProjectReferences

    let refresh (state: ProjectSession) : Result<string list, string list> =
        match loadGraph state.RootPath with
        | Error errors -> Error errors
        | Ok projects ->
            match scanSources projects with
            | Error errors -> Error errors
            | Ok sources ->
                let ordered = orderSources sources
                let oldPaths = state.Sources |> List.map (fun source -> source.Path) |> Set.ofList
                let newPaths = ordered |> List.map (fun source -> source.Path) |> Set.ofList
                let projectsChanged =
                    List.map projectSignature state.Projects <> List.map projectSignature projects
                let changed =
                    ordered
                    |> List.filter (fun source ->
                        match state.Texts.TryFind source.Path with
                        | Some oldText -> oldText <> source.Text
                        | None -> true)
                let oldSources =
                    state.Sources |> List.map (fun source -> source.Path, source) |> Map.ofList
                let mutable needsFullReload = oldPaths <> newPaths || projectsChanged
                for source in changed do
                    if not needsFullReload then
                        match readForms source source.NamespaceName with
                        | Error _ -> needsFullReload <- true
                        | Ok forms ->
                            if forms |> List.exists (fun form -> formKind form = TypeForm) then
                                needsFullReload <- true
                            else
                                match oldSources.TryFind source.Path with
                                | Some oldSource when
                                    oldSource.NamespaceName = source.NamespaceName
                                    && oldSource.Dependencies = source.Dependencies -> ()
                                | _ -> needsFullReload <- true

                if needsFullReload then
                    match buildSession ordered state.Output with
                    | Error errors -> Error errors
                    | Ok(session, lastValue) ->
                        state.Root <- List.last projects
                        state.Projects <- projects
                        state.Sources <- ordered
                        state.Session <- session
                        state.Texts <-
                            ordered
                            |> List.map (fun source -> source.Path, source.Text)
                            |> Map.ofList
                        state.LastValue <- lastValue
                        Ok [ state.Root.Path ]
                elif changed.IsEmpty then
                    Ok []
                else
                    let diagnostics, lastValue = evaluateSources state.Session changed
                    if diagnostics.IsEmpty then
                        state.Root <- List.last projects
                        state.Projects <- projects
                        state.Sources <- ordered
                        state.Texts <-
                            ordered
                            |> List.map (fun source -> source.Path, source.Text)
                            |> Map.ofList
                        state.LastValue <- lastValue
                        Ok(changed |> List.map (fun source -> source.Path))
                    else
                        Error diagnostics

    let run (path: string) (output: TextWriter) (execute: bool) : Result<ProjectRunResult, string list> =
        match openSession path output with
        | Error errors -> Error errors
        | Ok state ->
            let diagnostics = ResizeArray<string>()
            let mutable lastValue = state.LastValue
            if execute then
                let rootProject = state.Root
                let startup =
                    rootProject.StartupObject
                    |> Option.orElse (Some(rootProject.RootNamespace + "/main"))
                let startupValue =
                    startup
                    |> Option.bind (fun name ->
                        let symbol = Symbol.Parse name
                        let namespaceName =
                            symbol.Namespace |> Option.defaultValue rootProject.RootNamespace
                        state.Session.Runtime.Registry.FindVar(namespaceName, symbol.Name))
                    |> Option.map (fun variable -> variable.Root)
                match startupValue with
                | Some value ->
                    let previous = RecordRuntimeContext.getCurrent ()
                    RecordRuntimeContext.setCurrent (box state.Session.Runtime)
                    try
                        match value with
                        | :? IFn as functionValue ->
                            lastValue <- Some(functionValue.Invoke [||])
                        | _ ->
                            diagnostics.Add(
                                sprintf "%s: startup object is not callable."
                                    (startup |> Option.defaultValue "startup")
                            )
                    finally
                        RecordRuntimeContext.setCurrent previous
                | None ->
                    diagnostics.Add(
                        sprintf "%s: startup function not found."
                            (startup |> Option.defaultValue "startup")
                    )

            if diagnostics.Count = 0 then
                Ok
                    { Project = state.Root
                      Session = state.Session
                      Diagnostics = []
                      LastValue = lastValue }
            else
                Error(List.ofSeq diagnostics)
