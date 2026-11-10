namespace Wisp.Tests

open System
open System.Numerics
open System.Text.RegularExpressions
open Xunit
open Wisp.Core
open Wisp.Reader
open Wisp.Tests.TestHelpers

type ReaderTests() =
    [<Fact>]
    member _.``reads multiple top level forms and ignores comments and commas``() =
        let result = readAll "1, 2 3 ; ignored\n 4"
        Assert.Empty(result.Diagnostics)
        Assert.Equal<obj list>([ box 1L; box 2L; box 3L; box 4L ], result.Forms)

    [<Fact>]
    member _.``reads nil booleans strings and characters``() =
        let values = vectorItems (form "[nil true false \"a\\n\\u0041\" \\newline \\u0042 \\) \\o101]")
        Assert.Null(values.[0])
        Assert.Equal<obj>(box true, values.[1])
        Assert.Equal<obj>(box false, values.[2])
        Assert.Equal<obj>(box "a\nA", values.[3])
        Assert.Equal<obj>(box '\n', values.[4])
        Assert.Equal<obj>(box 'B', values.[5])
        Assert.Equal<obj>(box ')', values.[6])
        Assert.Equal<obj>(box 'A', values.[7])

    [<Fact>]
    member _.``reads numeric tower literals``() =
        Assert.Equal<obj>(box 42L, form "42")
        Assert.Equal<obj>(box -7L, form "-7")
        Assert.Equal<obj>(box 3L, form "+3")
        Assert.Equal<obj>(box 0.5, form ".5")
        Assert.Equal<obj>(box 1000.0, form "1e3")
        Assert.Equal<obj>(box 1.5, form "1.5")
        Assert.Equal<obj>(box 1.5m, form "1.5M")
        Assert.Equal<obj>(BigInteger(1), form "1N")
        Assert.Equal<obj>(box 8L, form "010")
        Assert.Equal<obj>(box 10L, form "2r1010")
        Assert.Equal<obj>(box 16L, form "0x10")
        let ratio = form "1/2" :?> Ratio
        Assert.Equal(BigInteger.One, ratio.Numerator)
        Assert.Equal(BigInteger(2), ratio.Denominator)
        let big = form "9223372036854775808" :?> BigInteger
        Assert.Equal(BigInteger.Parse("9223372036854775808"), big)

    [<Fact>]
    member _.``reads list vector map and set literals``() =
        let list = listItems (form "(1 2 3)")
        Assert.Equal<obj list>([ box 1L; box 2L; box 3L ], list)
        let vector = vectorItems (form "[1 2 3]")
        Assert.Equal<obj list>([ box 1L; box 2L; box 3L ], vector)
        let map = asMap (form "{:a 1 :b 2}")
        Assert.Equal(2, map.Count)
        Assert.Equal<obj>(box 1L, map.ValAt(key "a"))
        Assert.Equal<obj>(box 2L, map.ValAt(key "b"))
        let set = asSet (form "#{1 2 3}")
        Assert.Equal(3, set.Count)
        Assert.True(set.Contains(box 2L))
        let nested = vectorItems (form "[1 [2 {:a 3}]]")
        Assert.Equal(2, nested.Length)
        Assert.Equal<obj>(box 1L, nested.[0])
        let inner = vectorItems nested.[1]
        Assert.Equal<obj>(box 2L, inner.[0])
        Assert.Equal<obj>(box 3L, (asMap inner.[1]).ValAt(key "a"))

    [<Fact>]
    member _.``keywords resolve namespaces aliases and current namespace``() =
        let plain = asKeyword (form ":foo")
        Assert.Equal(None, plain.Namespace)
        Assert.Equal("foo", plain.Name)
        let qualified = asKeyword (form ":my.app/foo")
        Assert.Equal(Some "my.app", qualified.Namespace)
        let options =
            { ReaderOptions.Default with
                CurrentNamespace = "my.app"
                ResolveAlias = fun alias -> if alias = "core" then Some "my.app.core" else None }
        Assert.Equal("my.app/foo", (asKeyword (formWith options "::foo")).FullName)
        Assert.Equal("my.app.core/bar", (asKeyword (formWith options "::core/bar")).FullName)
        let codes = diagnosticCodes "::missing/foo"
        Assert.True(List.contains ReaderDiagnostics.UnknownAlias codes)

    [<Fact>]
    member _.``reader macros produce canonical lists``() =
        let quoted = listItems (form "'x")
        Assert.Equal("quote", (asSymbol quoted.[0]).FullName)
        Assert.Equal("x", (asSymbol quoted.[1]).FullName)
        let deref = listItems (form "@x")
        Assert.Equal("deref", (asSymbol deref.[0]).FullName)
        let varQuote = listItems (form "#'x")
        Assert.Equal("var", (asSymbol varQuote.[0]).FullName)
        let syntaxQuote = listItems (form "`x")
        Assert.Equal("quote", (asSymbol syntaxQuote.[0]).FullName)
        Assert.Equal("x", (asSymbol syntaxQuote.[1]).FullName)
        Assert.True(List.contains ReaderDiagnostics.InvalidSyntaxQuote (diagnosticCodes "~@x"))
        Assert.True(List.contains ReaderDiagnostics.InvalidSyntaxQuote (diagnosticCodes "~x"))

    [<Fact>]
    member _.``metadata attaches merges and supports hiccup type hints``() =
        let dynamicSymbol = asSymbol (form "^:dynamic x")
        let metadata = metadataOf dynamicSymbol |> Option.get
        Assert.Equal(Some(box true), metadata.TryGet(key "dynamic"))
        let merged = asSymbol (form "^:a ^:b x")
        let mergedMetadata = metadataOf merged |> Option.get
        Assert.Equal(Some(box true), mergedMetadata.TryGet(key "a"))
        Assert.Equal(Some(box true), mergedMetadata.TryGet(key "b"))
        let hinted = asSymbol (form "^[:List [:Int]] xs")
        let hintMetadata = metadataOf hinted |> Option.get
        match TypeHints.tryResolveMetadata hintMetadata with
        | Ok(Some resolved) -> Assert.Equal(typeof<System.Collections.Generic.List<int>>, resolved)
        | other -> failwithf "Expected a resolved type hint, got %A" other
        let codes = diagnosticCodes "^:dynamic 1"
        Assert.True(List.contains ReaderDiagnostics.MetadataNotSupported codes)

    [<Fact>]
    member _.``dispatch handles discard regex symbolic values and tagged literals``() =
        let result = readAll "#_ 1 2"
        Assert.Equal<obj list>([ box 2L ], result.Forms)
        let discardedInside = vectorItems (form "[1 #_ 2 3]")
        Assert.Equal<obj list>([ box 1L; box 3L ], discardedInside)
        let regex = form "#\"a+\"" :?> Regex
        Assert.True(regex.IsMatch "aaa")
        let positiveInfinity = unbox<double> (form "##Inf")
        Assert.True(Double.IsPositiveInfinity positiveInfinity)
        let negativeInfinity = unbox<double> (form "##-Inf")
        Assert.True(Double.IsNegativeInfinity negativeInfinity)
        let nan = unbox<double> (form "##NaN")
        Assert.True(Double.IsNaN nan)
        let identifier = Guid.NewGuid()
        let parsedIdentifier = form (sprintf "#uuid \"%O\"" identifier) :?> Guid
        Assert.Equal(identifier, parsedIdentifier)
        let tagged = form "#foo 1" :?> TaggedLiteral
        Assert.Equal("foo", tagged.Tag.FullName)
        Assert.Equal<obj>(box 1L, tagged.Form)

    [<Fact>]
    member _.``shebang legacy metadata and instance literals are read``() =
        let shebang = readAll "#!/usr/bin/env wisp\n42"
        Assert.Empty(shebang.Diagnostics)
        Assert.Equal<obj list>([ box 42L ], shebang.Forms)
        let legacy = asSymbol (form "#^:legacy x")
        let legacyMetadata = metadataOf legacy |> Option.get
        Assert.Equal(Some(box true), legacyMetadata.TryGet(key "legacy"))
        let instant = form "#inst \"2020-01-02T03:04:05Z\"" :?> DateTimeOffset
        Assert.Equal(DateTimeOffset(2020, 1, 2, 3, 4, 5, TimeSpan.Zero), instant)

    [<Fact>]
    member _.``metadata attaches to collection values and map sequences expose entries``() =
        let vector = asVector (form "^:immutable [1 2]")
        let vectorMetadata = metadataOf vector |> Option.get
        Assert.Equal(Some(box true), vectorMetadata.TryGet(key "immutable"))
        let map = asMap (form "{:a 1}")
        let entries = Seqs.toList map
        Assert.Single(entries) |> ignore
        let entry = vectorItems entries.[0]
        Assert.Equal<obj>(key "a", entry.[0])
        Assert.Equal<obj>(box 1L, entry.[1])
        let codes = diagnosticCodes "{nil 1 nil 2}"
        Assert.True(List.contains ReaderDiagnostics.DuplicateMapKey codes)

    [<Fact>]
    member _.``read one distinguishes an explicit nil form from eof``() =
        let result = readOne "nil"
        Assert.True(result.Form.IsSome)
        Assert.Null(result.Form.Value)
        Assert.Empty(result.Diagnostics)
        let eof = readOne "   "
        Assert.True(eof.Form.IsNone)

    [<Fact>]
    member _.``namespaced maps qualify keyword and symbol keys``() =
        let map = asMap (form "#:foo{:a 1 :bar/b 2 symbol 3}")
        Assert.Equal(3, map.Count)
        Assert.Equal<obj>(box 1L, map.ValAt(key "foo/a"))
        Assert.Equal<obj>(box 2L, map.ValAt(key "bar/b"))
        Assert.Equal<obj>(box 3L, map.ValAt(Symbol.Create("foo", "symbol")))
        Assert.Null(map.ValAt(key "a"))
        let options = { ReaderOptions.Default with CurrentNamespace = "my.app" }
        let auto = asMap (formWith options "#::{:a 1}")
        Assert.True(auto.ContainsKey(key "my.app/a"))
        let aliasOptions =
            { options with
                ResolveAlias = fun alias -> if alias = "core" then Some "my.app.core" else None }
        let aliased = asMap (formWith aliasOptions "#::core{:a 1}")
        Assert.True(aliased.ContainsKey(key "my.app.core/a"))

    [<Fact>]
    member _.``anonymous function literal rewrites percent arguments``() =
        let functionForm = listItems (form "#(+ %1 %2)")
        Assert.Equal("fn*", (asSymbol functionForm.[0]).FullName)
        let parameters = vectorItems functionForm.[1]
        Assert.Equal(2, parameters.Length)
        Assert.StartsWith("p1__", (asSymbol parameters.[0]).Name)
        Assert.StartsWith("p2__", (asSymbol parameters.[1]).Name)
        let body = listItems functionForm.[2]
        Assert.Equal("+", (asSymbol body.[0]).FullName)
        Assert.Equal((asSymbol parameters.[0]).Name, (asSymbol body.[1]).Name)
        Assert.Equal((asSymbol parameters.[1]).Name, (asSymbol body.[2]).Name)
        let restParameters = vectorItems (listItems (form "#(apply %1 %&)")).[1]
        Assert.Equal(3, restParameters.Length)
        Assert.Equal("&", (asSymbol restParameters.[1]).FullName)
        let codes = diagnosticCodes "#(#(%))"
        Assert.True(List.contains ReaderDiagnostics.AnonymousFnError codes)

    [<Fact>]
    member _.``reader diagnostics carry codes positions and rendered carets``() =
        Assert.True(List.contains ReaderDiagnostics.UnexpectedEof (diagnosticCodes "(1 2"))
        Assert.True(List.contains ReaderDiagnostics.UnmatchedDelimiter (diagnosticCodes ")"))
        Assert.True(List.contains ReaderDiagnostics.MismatchedDelimiter (diagnosticCodes "[1)"))
        Assert.True(List.contains ReaderDiagnostics.OddMap (diagnosticCodes "{:a 1 :b}"))
        Assert.True(List.contains ReaderDiagnostics.DuplicateMapKey (diagnosticCodes "{:a 1 :a 2}"))
        Assert.True(List.contains ReaderDiagnostics.DuplicateSetElement (diagnosticCodes "#{1 1}"))
        Assert.True(List.contains ReaderDiagnostics.InvalidEscape (diagnosticCodes "\"\\q\""))
        Assert.True(List.contains ReaderDiagnostics.InvalidCharacter (diagnosticCodes "\\wat"))
        Assert.True(List.contains ReaderDiagnostics.InvalidReaderConditional (diagnosticCodes "#?(:wisp)"))
        Assert.True(List.contains ReaderDiagnostics.ReaderEvalUnsupported (diagnosticCodes "#="))
        Assert.True(List.contains ReaderDiagnostics.UnknownAlias (diagnosticCodes "::missing/foo"))
        let source = SourceText.Create("(\n  1", "test.wisp")
        let result = Reader.readAll source ReaderOptions.Default
        Assert.NotEmpty(result.Diagnostics)
        let rendered = result.Diagnostics |> List.map source.Render |> String.concat "\n"
        Assert.Contains("test.wisp:1:1", rendered)
        Assert.Contains("Unexpected end of input", rendered)
        Assert.Contains("^", rendered)

    [<Fact>]
    member _.``reader conditionals select features and splice collections``() =
        Assert.Equal<obj>(box 1L, form "#?(:wisp 1 :clr 2 :default 3)")
        let clrOptions = { ReaderOptions.Default with Features = Set.ofList [ "clr" ] }
        Assert.Equal<obj>(box 2L, formWith clrOptions "#?(:wisp 1 :clr 2 :default 3)")
        Assert.Equal<obj>(box 3L, form "#?(:clj 1 :default 3)")
        Assert.Null(form "#?(:clj 1)")
        let spliced = vectorItems (form "[0 #?@(:wisp [1 2] :default [3]) 4]")
        Assert.Equal<obj list>([ box 0L; box 1L; box 2L; box 4L ], spliced)
        let listSpliced = listItems (form "(#?@(:wisp (1 2)) 3)")
        Assert.Equal<obj list>([ box 1L; box 2L; box 3L ], listSpliced)
        let all = readAll "#?@(:wisp [1 2]) 3"
        Assert.Empty(all.Diagnostics)
        Assert.Equal<obj list>([ box 1L; box 2L; box 3L ], all.Forms)
        let noMatch = readAll "#?@(:clj [1]) 3"
        Assert.Empty(noMatch.Diagnostics)
        Assert.Equal<obj list>([ box 3L ], noMatch.Forms)

        let odd = readAll "#?(:wisp)"
        Assert.NotEmpty(odd.Diagnostics)
        Assert.Equal(ReaderDiagnostics.InvalidReaderConditional, odd.Diagnostics.Head.Code)
        Assert.Equal(2, odd.Diagnostics.Head.Span.Start)
        let badFeature = readAll "#?([:wisp] 1)"
        Assert.Equal(ReaderDiagnostics.InvalidReaderConditional, badFeature.Diagnostics.Head.Code)
        let badSplice = readAll "#?@(:wisp 1)"
        Assert.Equal(ReaderDiagnostics.InvalidReaderConditional, badSplice.Diagnostics.Head.Code)

    [<Fact>]
    member _.``reader conditionals work after reader prefixes``() =
        let quoted = readAll "'#?@(:wisp [:a :b])"
        Assert.Empty quoted.Diagnostics
        Assert.Equal<string list>(
            [ "(quote :a)"; "(quote :b)" ],
            quoted.Forms |> List.map Print.printString
        )
        for item in quoted.Forms do
            Assert.Equal(Some(TextSpan.Create(0, quoted.Position)), SourceMetadata.trySpan item)

        let selectedText = "'#?(:wisp a :default b)"
        let selected = readAll selectedText
        Assert.Single(selected.Forms) |> ignore
        Assert.Equal("(quote a)", Print.printString selected.Forms.Head)
        Assert.Equal(Some(TextSpan.Create(0, selectedText.Length)), SourceMetadata.trySpan selected.Forms.Head)

        let metadataText = "^:m #?@(:wisp [a b])"
        let metadata = readAll metadataText
        Assert.Empty metadata.Diagnostics
        Assert.Equal(2, metadata.Forms.Length)
        for item in metadata.Forms do
            let itemMetadata = (item :?> IMeta).Meta.Value
            Assert.Equal(Some(box true), itemMetadata.TryGet(Keyword.Intern "m"))
            Assert.Equal(Some(TextSpan.Create(0, metadataText.Length)), SourceMetadata.trySpan item)

        Assert.Equal<string list>(
            [ "(deref a)"; "(deref b)" ],
            (readAll "@#?@(:wisp [a b])").Forms |> List.map Print.printString
        )
        Assert.Equal<string list>(
            [ "(var a)"; "(var b)" ],
            (readAll "#'#?@(:wisp [a b])").Forms |> List.map Print.printString
        )
        Assert.Equal<obj list>([ box 3L ], (readAll "#_#?@(:wisp [a b]) 3").Forms)
        Assert.Equal<string list>(
            [ "(quote a)"; "(quote b)" ],
            (readAll "`#?@(:wisp [a b])").Forms |> List.map Print.printString
        )
        Assert.Equal(
            "[0 (quote a) (quote b) 1]",
            Print.printString (form "[0 '#?@(:wisp [a b]) 1]")
        )

        let badMetadata = readAll "^:m #?@(:wisp [1 2])"
        Assert.NotEmpty(badMetadata.Diagnostics)
        Assert.Equal(ReaderDiagnostics.MetadataNotSupported, badMetadata.Diagnostics.Head.Code)
        Assert.True(badMetadata.Diagnostics.Head.Span.Length > 0)

    [<Fact>]
    member _.``syntax quote expands unquotes splicing and gensyms``() =
        let expanded = listItems (form "`(a ~b ~@c)")
        Assert.Equal("concat", (asSymbol expanded.[0]).FullName)
        let first = listItems expanded.[1]
        Assert.Equal("list", (asSymbol first.[0]).FullName)
        let quotedA = listItems first.[1]
        Assert.Equal("quote", (asSymbol quotedA.[0]).FullName)
        Assert.Equal("a", (asSymbol quotedA.[1]).FullName)
        let second = listItems expanded.[2]
        Assert.Equal("list", (asSymbol second.[0]).FullName)
        Assert.Equal("b", (asSymbol second.[1]).FullName)
        Assert.Equal("c", (asSymbol expanded.[3]).FullName)

        let vectorExpanded = listItems (form "`[~x]")
        Assert.Equal("apply", (asSymbol vectorExpanded.[0]).FullName)
        Assert.Equal("vector", (asSymbol vectorExpanded.[1]).FullName)

        let gensymExpanded = Print.printString (form "`(let [x# 1] x#)")
        Assert.Contains("__auto__", gensymExpanded)
        Assert.DoesNotContain("x#", gensymExpanded)
        Assert.Equal(2, gensymExpanded.Split("__auto__").Length - 1)

        let unquote = readAll "~x"
        Assert.Equal(ReaderDiagnostics.InvalidSyntaxQuote, unquote.Diagnostics.Head.Code)
        Assert.Equal(0, unquote.Diagnostics.Head.Span.Start)
        Assert.Empty((readAll "``x").Diagnostics)
        let topSplice = readAll "`~@x"
        Assert.Equal(ReaderDiagnostics.InvalidSyntaxQuote, topSplice.Diagnostics.Head.Code)
        let quoted = form "`x"
        Assert.True((SourceMetadata.trySpan quoted).IsSome)

    [<Fact>]
    member _.``nested syntax quotes expand inner quotes as data``() =
        let doubleSymbol = Print.printString (form "``x")
        Assert.Contains("concat", doubleSymbol)
        Assert.Contains("(quote quote)", doubleSymbol)
        Assert.Contains("(quote x)", doubleSymbol)
        Assert.Equal("42", Print.printString (form "``42"))
        Assert.Empty((readAll "``(a ~b)").Diagnostics)
        Assert.Empty((readAll "``(a ~~b)").Diagnostics)
        Assert.DoesNotContain("unquote", Print.printString (form "``(a ~~b)"))

        let nested = Print.printString (form "`(a `(b ~c))")
        Assert.Contains("(quote b)", nested)
        Assert.Contains("(quote c)", nested)
        Assert.DoesNotContain("unquote", nested)

        let triple = Print.printString (form "```x")
        Assert.Contains("quote", triple)
        Assert.Contains("concat", triple)

    [<Fact>]
    member _.``map templates support unquote splicing``() =
        let expanded = listItems (form "`{1 ~@[2 3] 4 ~@[5 6]}")
        Assert.Equal("apply", (asSymbol expanded.[0]).FullName)
        Assert.Equal("hash-map", (asSymbol expanded.[1]).FullName)
        let concatenated = listItems expanded.[2]
        Assert.Equal("concat", (asSymbol concatenated.[0]).FullName)
        Assert.Equal(5, concatenated.Length)
        Assert.Equal<obj>([ box 2L; box 3L ], vectorItems concatenated.[2])
        Assert.Equal<obj>([ box 5L; box 6L ], vectorItems concatenated.[4])

        let valueSplice = listItems (form "`{:a ~@[1 2]}")
        Assert.Equal("apply", (asSymbol valueSplice.[0]).FullName)
        let spliced = listItems valueSplice.[2]
        Assert.Equal<obj>([ box 1L; box 2L ], vectorItems spliced.[2])

        let ordered = listItems (form "`{0 ~@[1] 2 ~@[3] 4 ~@[5] 6 ~@[7] 8 ~@[9]}")
        Assert.Equal(
            "(concat (list 0) [1] (list 2) [3] (list 4) [5] (list 6) [7] (list 8) [9])",
            Print.printString ordered.[2]
        )

    [<Fact>]
    member _.``reader attaches source ranges to metadata-capable forms``() =
        let source = SourceText.Create("(a [b])")
        let result = Reader.readOne source ReaderOptions.Default
        let form = result.Form.Value
        Assert.Equal(Some(TextSpan.Create(0, 7)), SourceMetadata.trySpan form)
        let items = listItems form
        Assert.Equal(Some(TextSpan.Create(1, 1)), SourceMetadata.trySpan items.[0])
        Assert.Equal(Some(TextSpan.Create(3, 3)), SourceMetadata.trySpan items.[1])
        let metadata = metadataOf items.[0] |> Option.get
        Assert.Equal(Some(box 1), metadata.TryGet(SourceMetadata.lineKey))
        Assert.Equal(Some(box 2), metadata.TryGet(SourceMetadata.columnKey))

        let without = { ReaderOptions.Default with AttachSourceMetadata = false }
        let plain = (Reader.readOne source without).Form.Value
        Assert.True((SourceMetadata.trySpan plain).IsNone)

    [<Fact>]
    member _.``read one stops after one form and reports its position``() =
        let result = readOne "1 2"
        Assert.Empty(result.Diagnostics)
        Assert.Equal<obj>(box 1L, result.Form.Value)
        Assert.Equal(1, result.Position)
        let empty = readAll " ; only a comment\n"
        Assert.Empty(empty.Forms)
        Assert.Empty(empty.Diagnostics)
