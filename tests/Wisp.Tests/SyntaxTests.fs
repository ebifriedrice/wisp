namespace Wisp.Tests

open System
open Xunit
open Wisp.Core
open Wisp.Syntax
open Wisp.Tests.TestHelpers

type SyntaxTests() =
    [<Fact>]
    member _.``syntax tree preserves every byte including trivia``() =
        let source =
            "#!/usr/bin/env wisp\n; comment\n(defn f [x] ; inline\n  (let [y 1] (+ x y)))\n#{:a 1, :b 2} #_discard #\"a+\" \"str\\n\" \\newline"
        let tree = syntax source
        Assert.Empty(tree.Diagnostics)
        Assert.Equal(source, syntaxText tree)
        Assert.Equal(source.Length, tree.Root.Span.Length)

    [<Fact>]
    member _.``nested forms have structural kinds and text ranges``() =
        let tree = syntax "(def x [1 2])"
        let form = topLevelForms tree |> List.head
        Assert.Equal(SyntaxKind.List, form.Kind)
        Assert.Equal("(def x [1 2])", tree.TextOf form)
        let listNode = form :?> SyntaxNode
        Assert.Equal(SyntaxKind.OpenParen, listNode.Children.[0].Kind)
        let vector = tree.Nodes |> Seq.find (fun node -> node.Kind = SyntaxKind.Vector)
        Assert.Equal("[1 2]", tree.TextOf vector)
        Assert.Equal(TextSpan.Create(7, 5), vector.Span)

    [<Fact>]
    member _.``prefix and dispatch forms are distinct nodes``() =
        let kindOf text =
            let forms = topLevelForms (syntax text)
            Assert.NotEmpty(forms)
            forms.Head.Kind

        Assert.Equal(SyntaxKind.MetadataNode, kindOf "^[:List [:Int]] xs")
        Assert.Equal(SyntaxKind.Set, kindOf "#{1 2}")
        Assert.Equal(SyntaxKind.AnonymousFnNode, kindOf "#(+ % 1)")
        Assert.Equal(SyntaxKind.DiscardNode, kindOf "#_ x")
        Assert.Equal(SyntaxKind.TaggedLiteralNode, kindOf "#uuid \"00000000-0000-0000-0000-000000000000\"")
        Assert.Equal(SyntaxKind.NamespacedMapNode, kindOf "#:foo{:a 1}")
        Assert.Equal(SyntaxKind.VarQuoteNode, kindOf "#'x")
        Assert.Equal(SyntaxKind.SymbolicValueNode, kindOf "##Inf")
        Assert.Equal(SyntaxKind.ReaderConditionalNode, kindOf "#?")
        Assert.Equal(SyntaxKind.ReaderEvalNode, kindOf "#=")
        Assert.Equal(SyntaxKind.Regex, kindOf "#\"a+\"")
        Assert.Equal(SyntaxKind.QuoteNode, kindOf "'x")

    [<Fact>]
    member _.``parser reports structural errors and keeps the token stream``() =
        let codes text = syntaxDiagnosticCodes (syntax text)
        Assert.Contains(SyntaxDiagnostics.MissingClose, codes "(1 2")
        Assert.Contains(SyntaxDiagnostics.MissingClose, codes "[1)")
        Assert.Contains(SyntaxDiagnostics.UnexpectedClose, codes "[1)")
        Assert.Contains(SyntaxDiagnostics.OddMap, codes "{1 2 3}")
        Assert.Contains(SyntaxDiagnostics.UnterminatedString, codes "\"abc")
        Assert.Contains(SyntaxDiagnostics.UnterminatedRegex, codes "#\"abc")
        Assert.Contains(SyntaxDiagnostics.MalformedDispatch, codes "#:foo 1")
        Assert.Contains(SyntaxDiagnostics.MissingForm, codes "#'")
        Assert.Contains(SyntaxDiagnostics.MissingForm, codes "^:a")
        let broken = syntax "(1 2"
        Assert.Equal("(1 2", syntaxText broken)
        let forms = topLevelForms broken
        Assert.Single(forms) |> ignore
        Assert.Equal(SyntaxKind.List, forms.Head.Kind)
        let unterminated = syntax "\"abc"
        Assert.Equal("\"abc", syntaxText unterminated)

    [<Fact>]
    member _.``tokens and nodes expose absolute spans and source text``() =
        let source = SourceText.Create("(a [b])", "test.wisp")
        let tree = SyntaxTree.Parse source
        let b = tree.Tokens |> Seq.find (fun token -> token.Text = "b")
        Assert.Equal(4, b.Offset)
        Assert.Equal(TextSpan.Create(4, 1), b.Span)
        Assert.Equal((1, 5), source.LineColumn b.Offset)
        let vector = tree.Nodes |> Seq.find (fun node -> node.Kind = SyntaxKind.Vector)
        Assert.Equal("[b]", tree.TextOf vector)
        Assert.Equal(TextSpan.Create(3, 3), vector.Span)

    [<Fact>]
    member _.``incremental edit reparses one form and reuses prefix and suffix greens``() =
        let before = syntax "1 2 3 4"
        let after = before.ApplyChange { Start = 4; OldLength = 1; NewText = "33" }
        Assert.Equal("1 2 33 4", after.Text)
        Assert.Equal("1 2 33 4", syntaxText after)
        Assert.Empty(after.Diagnostics)
        Assert.Equal(4, topLevelForms after |> List.length)
        Assert.Equal("33", after.TextOf after.Root.Children.[4])
        Assert.Equal(4, after.Root.Children.[4].Offset)
        Assert.True(Object.ReferenceEquals(before.Root.Children.[0].GreenIdentity, after.Root.Children.[0].GreenIdentity))
        Assert.True(Object.ReferenceEquals(before.Root.Children.[6].GreenIdentity, after.Root.Children.[6].GreenIdentity))

    [<Fact>]
    member _.``deleting trivia can merge forms and the parser sees the merge``() =
        let before = syntax "1 2"
        let after = before.ApplyChange { Start = 1; OldLength = 1; NewText = "" }
        Assert.Equal("12", after.Text)
        let forms = topLevelForms after
        Assert.Single(forms) |> ignore
        Assert.Equal(SyntaxKind.Number, forms.Head.Kind)
        Assert.Empty(after.Diagnostics)

    [<Fact>]
    member _.``incremental boundaries merge character literals and adjacent atoms``() =
        let character = syntax "x "
        let characterAfter = character.ApplyChange { Start = 0; OldLength = 1; NewText = "\\" }
        Assert.Equal("\\ ", characterAfter.Text)
        let characterForms = topLevelForms characterAfter
        Assert.Single(characterForms) |> ignore
        Assert.Equal(SyntaxKind.Character, characterForms.Head.Kind)
        Assert.Empty(characterAfter.Diagnostics)

        let adjacent = syntax "x("
        let adjacentAfter = adjacent.ApplyChange { Start = 1; OldLength = 1; NewText = "1" }
        Assert.Equal("x1", adjacentAfter.Text)
        let adjacentForms = topLevelForms adjacentAfter
        Assert.Single(adjacentForms) |> ignore
        Assert.Equal(SyntaxKind.Symbol, adjacentForms.Head.Kind)
        Assert.Equal("x1", adjacentAfter.TextOf adjacentForms.Head)

        let emptyRegion = syntax "a b c"
        let emptyRegionAfter = emptyRegion.ApplyChange { Start = 2; OldLength = 1; NewText = "" }
        Assert.Equal("a  c", emptyRegionAfter.Text)
        let whitespace = emptyRegionAfter.Tokens |> Seq.filter (fun token -> token.Kind = SyntaxKind.Whitespace) |> Seq.toList
        Assert.Single(whitespace) |> ignore
        Assert.Equal("  ", whitespace.Head.Text)

    [<Fact>]
    member _.``inserting an unbalanced delimiter extends the reparse window``() =
        let before = syntax "1 2 3"
        let after = before.ApplyChange { Start = 0; OldLength = 0; NewText = "(" }
        Assert.Equal("(1 2 3", after.Text)
        Assert.Contains(SyntaxDiagnostics.MissingClose, syntaxDiagnosticCodes after)
        let forms = topLevelForms after
        Assert.Single(forms) |> ignore
        Assert.Equal(SyntaxKind.List, forms.Head.Kind)

    [<Fact>]
    member _.``diagnostic offsets shift when text is inserted before them``() =
        let before = syntax "[1"
        Assert.True(before.Diagnostics |> List.exists (fun diagnostic -> diagnostic.Span.Start = 0))
        let after = before.ApplyChange { Start = 0; OldLength = 0; NewText = "x " }
        Assert.Equal("x [1", after.Text)
        Assert.True(after.Diagnostics |> List.exists (fun diagnostic -> diagnostic.Span.Start = 2))

    [<Fact>]
    member _.``no-op changes return the same tree``() =
        let tree = syntax "1 2 3"
        let same = tree.ApplyChange { Start = 1; OldLength = 0; NewText = "" }
        Assert.Same(tree, same)

    [<Fact>]
    member _.``incremental reparses agree with full parses across random edits``() =
        let random = Random(20261110)
        let replacements = [| ""; " "; "x"; "("; ")"; "\n"; "\""; "#_"; ":k"; "1" |]
        let shape (tree: SyntaxTree) =
            tree.Root.Children
            |> Array.map (fun element -> element.Kind, element.Span.Start, element.Span.Length)
            |> Array.toList
        let diagnostics (tree: SyntaxTree) =
            tree.Diagnostics
            |> List.map (fun diagnostic -> diagnostic.Code, diagnostic.Span.Start, diagnostic.Span.Length)
            |> List.sort
        let mutable tree = syntax "(def x [1 2 3])\n#{:a 1 :b 2}\n"

        for _ in 1 .. 120 do
            let start = random.Next(0, tree.Text.Length + 1)
            let oldLength = random.Next(0, min 4 (tree.Text.Length - start) + 1)
            let replacement = replacements.[random.Next replacements.Length]
            let before = tree.Text
            let expected =
                before.Substring(0, start) + replacement + before.Substring(start + oldLength)
            tree <- tree.ApplyChange { Start = start; OldLength = oldLength; NewText = replacement }
            Assert.Equal(expected, tree.Text)
            Assert.Equal(expected, syntaxText tree)
            let full = SyntaxTree.Parse(SourceText.Create expected)
            let expectedShape = shape full
            let actualShape = shape tree
            if expectedShape <> actualShape then
                failwithf "Incremental shape differs.\nExpected: %A\nActual: %A\nText: %s" expectedShape actualShape expected
            let expectedDiagnostics = diagnostics full
            let actualDiagnostics = diagnostics tree
            if expectedDiagnostics <> actualDiagnostics then
                failwithf "Incremental diagnostics differ.\nExpected: %A\nActual: %A\nText: %s" expectedDiagnostics actualDiagnostics expected

    [<Fact>]
    member _.``a sequence of edits remains lossless``() =
        let mutable tree = syntax "(def x 1)\n(def y 2)\n"
        tree <- tree.ApplyChange { Start = 7; OldLength = 1; NewText = "42" }
        tree <- tree.ApplyChange { Start = 0; OldLength = 0; NewText = "; header\n" }
        tree <- tree.ApplyChange { Start = tree.Text.Length - 2; OldLength = 0; NewText = "\n" }
        Assert.Equal(tree.Source.Text, syntaxText tree)
        Assert.Equal(tree.Source.Length, tree.Root.Span.Length)
