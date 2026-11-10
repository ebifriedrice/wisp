namespace Wisp.Tests

open System
open Xunit
open Wisp.Core
open Wisp.Reader
open Wisp.Syntax

module TestHelpers =
    let options = ReaderOptions.Default

    let readAllWith (readerOptions: ReaderOptions) (text: string) =
        Reader.readAll (SourceText.Create text) readerOptions

    let readAll (text: string) = readAllWith options text

    let readOneWith (readerOptions: ReaderOptions) (text: string) =
        Reader.readOne (SourceText.Create text) readerOptions

    let readOne (text: string) = readOneWith options text

    let formWith (readerOptions: ReaderOptions) (text: string) =
        let result = readOneWith readerOptions text
        if not result.Diagnostics.IsEmpty then
            failwithf "Unexpected reader diagnostics: %A" (result.Diagnostics |> List.map (fun d -> d.Code, d.Message))
        match result.Form with
        | Some value -> value
        | None -> failwith "Expected one form, got EOF."

    let form (text: string) = formWith options text

    let listItems (value: obj) = (value :?> PersistentList).Items |> Seq.toList

    let vectorItems (value: obj) = (value :?> PersistentVector).Elements |> Seq.toList

    let metadataOf (value: obj) =
        match value with
        | :? IMeta as meta -> meta.Meta
        | _ -> None

    let diagnosticCodes (text: string) =
        (readAll text).Diagnostics |> List.map (fun diagnostic -> diagnostic.Code)

    let diagnosticText (text: string) =
        let source = SourceText.Create(text, "test.wisp")
        let result = Reader.readAll source options
        result.Diagnostics |> List.map source.Render

    let key name = Keyword.Intern name

    let asInt64 (value: obj) = Convert.ToInt64(value, Globalization.CultureInfo.InvariantCulture)

    let asVector (value: obj) = value :?> PersistentVector

    let asMap (value: obj) = value :?> PersistentMap

    let asSet (value: obj) = value :?> PersistentSet

    let asSymbol (value: obj) = value :?> Symbol

    let asKeyword (value: obj) = value :?> Keyword

    let syntax text = SyntaxTree.Parse(SourceText.Create text)

    let syntaxText (tree: SyntaxTree) =
        tree.Tokens |> Seq.map (fun token -> token.Text) |> String.concat ""

    let syntaxKinds (tree: SyntaxTree) =
        tree.Root.Children |> Array.map (fun element -> element.Kind)

    let topLevelForms (tree: SyntaxTree) =
        tree.Root.Children
        |> Array.filter (fun element -> not (SyntaxFacts.isTrivia element.Kind))
        |> Array.toList

    let syntaxDiagnosticCodes (tree: SyntaxTree) =
        tree.Diagnostics |> List.map (fun diagnostic -> diagnostic.Code)
