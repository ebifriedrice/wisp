namespace Wisp.Compiler

open System
open System.IO

module Templates =
    type Template =
        { Name: string
          Description: string }

    let list =
        [ { Name = "app"; Description = "A console application with a startup function." }
          { Name = "lib"; Description = "A library project with one public function." }
          { Name = "workspace"
            Description = "An application project referencing a library project." } ]

    let private sanitize (name: string) =
        let cleaned =
            name
            |> Seq.map (fun character ->
                if Char.IsLetterOrDigit character || character = '_' then character
                else '_')
            |> Seq.toArray
            |> String
        if String.IsNullOrWhiteSpace cleaned then "wisp-app" else cleaned

    let private projectName (directory: string) (name: string option) =
        name
        |> Option.filter (String.IsNullOrWhiteSpace >> not)
        |> Option.defaultValue (Path.GetFileName(Path.TrimEndingDirectorySeparator(Path.GetFullPath directory)))

    let private manifest name rootNamespace outputType startup =
        let startupXml =
            match startup with
            | Some value -> sprintf "    <StartupObject>%s</StartupObject>\n" value
            | None -> ""
        sprintf
            """<Project>
  <PropertyGroup>
    <Name>%s</Name>
    <RootNamespace>%s</RootNamespace>
    <OutputType>%s</OutputType>
%s  </PropertyGroup>
  <ItemGroup>
    <Wisp Include="src/**/*.wisp" />
  </ItemGroup>
</Project>
"""
            name
            rootNamespace
            outputType
            startupXml

    let create
        (directory: string)
        (name: string option)
        (templateName: string)
        (force: bool)
        : Result<string list, string list> =
        let directory = Path.GetFullPath directory
        let name = projectName directory name
        let rootNamespace = sanitize name
        let template =
            list
            |> List.tryFind (fun candidate -> candidate.Name = templateName)
        match template with
        | None ->
            Error
                [ sprintf "Unknown template %s. Available templates: %s."
                      templateName
                      (list |> List.map (fun candidate -> candidate.Name) |> String.concat ", ") ]
        | Some template ->
            let files =
                match template.Name with
                | "app" ->
                    [ "wisp.proj",
                      manifest name rootNamespace "Exe" (Some(rootNamespace + "/main"))
                      "src/main.wisp",
                      sprintf
                          "(ns %s)\n\n(defn main []\n  (println \"Hello from %s!\"))\n"
                          rootNamespace
                          name ]
                | "lib" ->
                    [ "wisp.proj",
                      manifest name rootNamespace "Library" None
                      "src/" + rootNamespace + ".wisp",
                      sprintf
                          "(ns %s)\n\n(defn greet [who]\n  (str \"Hello, \" who \"!\"))\n"
                          rootNamespace ]
                | _ ->
                    let appManifest =
                        (manifest name rootNamespace "Exe" (Some(rootNamespace + "/main")))
                            .Replace(
                                "    <Wisp Include=\"src/**/*.wisp\" />",
                                "    <Wisp Include=\"src/**/*.wisp\" />\n    <ProjectReference Include=\"lib/lib.wisp.proj\" />"
                            )
                    [ "wisp.proj",
                      appManifest
                      "src/main.wisp",
                      sprintf
                          "(ns %s\n  (:require [%s.lib :as lib]))\n\n(defn main []\n  (println (lib/greet \"Wisp\")))\n"
                          rootNamespace
                          rootNamespace
                      "lib/lib.wisp.proj",
                      sprintf
                          """<Project>
  <PropertyGroup>
    <Name>%s.lib</Name>
    <RootNamespace>%s.lib</RootNamespace>
    <OutputType>Library</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <Wisp Include="src/**/*.wisp" />
  </ItemGroup>
</Project>
"""
                          rootNamespace
                          rootNamespace
                      "lib/src/lib.wisp",
                      sprintf
                          "(ns %s.lib)\n\n(defn greet [who]\n  (str \"Hello, \" who \"!\"))\n"
                          rootNamespace ]

            let projectFile = Path.Combine(directory, "wisp.proj")
            if File.Exists projectFile && not force then
                Error [ sprintf "%s already exists. Pass --force to overwrite template files." projectFile ]
            else
                try
                    Directory.CreateDirectory directory |> ignore
                    for relativePath, content in files do
                        let path = Path.Combine(directory, relativePath)
                        let parent = Path.GetDirectoryName path
                        if not (String.IsNullOrEmpty parent) then
                            Directory.CreateDirectory parent |> ignore
                        File.WriteAllText(path, content)
                    Ok(files |> List.map (fun (relativePath, _) -> Path.Combine(directory, relativePath)))
                with error ->
                    Error [ sprintf "%s: %s" directory error.Message ]
