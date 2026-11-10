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
    member _.``projects load referenced libraries and forward references across files``() =
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
(defclass Holder [^Later value])
(defclass Later [^int n])
(defn main []
  (println "sum:" (lib/add 20 22))
  (println "forward:" (helper))
  (println "lib-forward:" (lib/later))
  (println "type-forward:" (.-n (.-value (Holder {:value (Later {:n 7})})))))
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
                Assert.Contains("type-forward: 7", printed)
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
