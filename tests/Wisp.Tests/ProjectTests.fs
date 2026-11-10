namespace Wisp.Tests

open System
open System.IO
open Xunit
open Wisp.Compiler

type ProjectTests() =
    let writeProject directory name references =
        Directory.CreateDirectory directory |> ignore
        let projectPath = Path.Combine(directory, name + ".wisp.proj")
        let referenceXml =
            references
            |> List.map (fun reference -> sprintf "<ProjectReference Include=\"%s\" />" reference)
            |> String.concat Environment.NewLine
        File.WriteAllText(
            projectPath,
            $"""<Project>
  <PropertyGroup>
    <Name>{name}</Name>
    <RootNamespace>{name}</RootNamespace>
  </PropertyGroup>
  <ItemGroup>
    <Wisp Include="**/*.wisp" />
    {referenceXml}
  </ItemGroup>
</Project>"""
        )
        projectPath

    [<Fact>]
    member _.``projects load referenced libraries and source-order types across files``() =
        let root = Path.Combine(Path.GetTempPath(), "wisp-project-" + Guid.NewGuid().ToString("N"))
        try
            let lib =
                writeProject (Path.Combine(root, "lib")) "lib" []
                |> fun path -> Path.Combine(Path.GetDirectoryName path, "lib.wisp")
            File.WriteAllText(
                lib,
                "(ns lib)\n(defn add [a b] (+ a b))\n(defn later [] (defined-later))\n(defn defined-later [] 99)\n"
            )
            let app =
                writeProject (Path.Combine(root, "app")) "app" [ "../lib/lib.wisp.proj" ]
                |> fun path -> Path.Combine(Path.GetDirectoryName path, "app.wisp")
            File.WriteAllText(
                app,
                """(ns app (:require [lib :as lib]))
(defclass Later [^int n])
(defclass Holder [^Later value])
(declare helper)
(defn main []
  (println "sum:" (lib/add 20 22))
  (println "forward:" (helper))
  (println "lib-forward:" (lib/later))
  (println "type:" (.-n (.-value (Holder {:value (Later {:n 7})})))))
(defn helper [] "ok")
"""
            )
            let output = new StringWriter()
            match Project.run (Path.Combine(root, "app", "app.wisp.proj")) output true with
            | Error errors -> failwithf "Project failed: %s" (String.Join(Environment.NewLine, errors))
            | Ok _ ->
                let printed = output.ToString()
                Assert.Contains("sum: 42", printed)
                Assert.Contains("forward: ok", printed)
                Assert.Contains("lib-forward: 99", printed)
                Assert.Contains("type: 7", printed)
        finally
            if Directory.Exists root then
                Directory.Delete(root, true)

    [<Fact>]
    member _.``project cycles are reported instead of recursing forever``() =
        let root = Path.Combine(Path.GetTempPath(), "wisp-cycle-" + Guid.NewGuid().ToString("N"))
        try
            writeProject (Path.Combine(root, "a")) "a" [ "../b/b.wisp.proj" ] |> ignore
            writeProject (Path.Combine(root, "b")) "b" [ "../a/a.wisp.proj" ] |> ignore
            match Project.loadGraph (Path.Combine(root, "a", "a.wisp.proj")) with
            | Ok _ -> failwith "Expected a cycle error."
            | Error errors -> Assert.Contains("cycle", String.Join(" ", errors), StringComparison.OrdinalIgnoreCase)
        finally
            if Directory.Exists root then
                Directory.Delete(root, true)
    [<Fact>]
    member _.``templates create runnable projects``() =
        let root = Path.Combine(Path.GetTempPath(), "wisp-template-" + Guid.NewGuid().ToString("N"))
        try
            match Templates.create root None "workspace" false with
            | Error errors -> failwithf "Template failed: %s" (String.Join(Environment.NewLine, errors))
            | Ok _ ->
                let output = new StringWriter()
                match Project.run (Path.Combine(root, "wisp.proj")) output true with
                | Error errors -> failwithf "Template project failed: %s" (String.Join(Environment.NewLine, errors))
                | Ok _ -> Assert.Contains("Hello, Wisp!", output.ToString())
        finally
            if Directory.Exists root then
                Directory.Delete(root, true)

    [<Fact>]
    member _.``project refresh reloads changed files and promotes type changes to a full reload``() =
        let root = Path.Combine(Path.GetTempPath(), "wisp-reload-" + Guid.NewGuid().ToString("N"))
        try
            let lib =
                writeProject (Path.Combine(root, "lib")) "lib" []
                |> fun path -> Path.Combine(Path.GetDirectoryName path, "lib.wisp")
            File.WriteAllText(lib, "(ns lib)\n(defn value [] 1)\n")
            let app =
                writeProject (Path.Combine(root, "app")) "app" [ "../lib/lib.wisp.proj" ]
                |> fun path -> Path.Combine(Path.GetDirectoryName path, "app.wisp")
            File.WriteAllText(app, "(ns app (:require [lib :as lib]))\n(defn main [] (lib/value))\n")
            let output = new StringWriter()
            match Project.openSession (Path.Combine(root, "app", "app.wisp.proj")) output with
            | Error errors -> failwithf "Project failed: %s" (String.Join(Environment.NewLine, errors))
            | Ok state ->
                state.Session.Runtime.CurrentNamespace <- "app"
                let eval (text: string) = List.last (state.Session.EvalText text).Values
                Assert.Equal(1L, Convert.ToInt64(eval "(lib/value)"))
                File.WriteAllText(lib, "(ns lib)\n(defn value [] 2)\n")
                match Project.refresh state with
                | Error errors -> failwithf "Reload failed: %s" (String.Join(Environment.NewLine, errors))
                | Ok changed -> Assert.Contains(lib, changed)
                state.Session.Runtime.CurrentNamespace <- "app"
                Assert.Equal(2L, Convert.ToInt64(eval "(lib/value)"))

                let oldSession = state.Session
                File.WriteAllText(lib, "(ns lib)\n(defclass Box [^int value])\n(defn value [] 3)\n")
                match Project.refresh state with
                | Error errors -> failwithf "Type reload failed: %s" (String.Join(Environment.NewLine, errors))
                | Ok _ -> ()
                Assert.False(Object.ReferenceEquals(oldSession, state.Session))
        finally
            if Directory.Exists root then
                Directory.Delete(root, true)
