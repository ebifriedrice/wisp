namespace Wisp.Syntax

open System
open System.Collections.Generic
open Wisp.Core

module internal Parser =
    let private nodeElement kind (children: GreenElement[]) =
        GreenElement.Node(Green.makeNode kind children)

    type private ParserState(tokens: Lexer.Token[], diagnostics: ResizeArray<Diagnostic>) =
        let mutable position = 0

        member _.Eof = position >= tokens.Length
        member _.Current = tokens.[position]
        member _.CurrentKind = if position >= tokens.Length then None else Some tokens.[position].Kind
        member _.Position = position
        member _.Advance() = if position < tokens.Length then position <- position + 1

        member _.TokenSpan(token: Lexer.Token) = TextSpan.Create(token.Start, token.Text.Length)

        member _.AddDiagnostic(code: string, message: string, span: TextSpan) =
            diagnostics.Add(Diagnostic.create code DiagnosticSeverity.Error message span)

        member _.TokenElement(token: Lexer.Token) =
            GreenElement.Token { Kind = token.Kind; Text = token.Text }

        member this.AddCurrent(children: ResizeArray<GreenElement>) =
            children.Add(this.TokenElement this.Current)
            this.Advance()

        member this.CollectTrivia(children: ResizeArray<GreenElement>) =
            while not this.Eof && SyntaxFacts.isTrivia this.Current.Kind do
                this.AddCurrent children

        member this.ParseDelimited(nodeKind: SyntaxKind, closeKind: SyntaxKind) : GreenElement =
            let children = ResizeArray<GreenElement>()
            let openToken = this.Current
            children.Add(this.TokenElement openToken)
            this.Advance()
            let mutable closed = false
            let mutable mismatched = None
            let mutable formCount = 0

            while not closed && not this.Eof do
                let kind = this.Current.Kind
                if kind = closeKind then
                    this.AddCurrent children
                    closed <- true
                elif SyntaxFacts.isTrivia kind then
                    this.AddCurrent children
                elif SyntaxFacts.isClose kind then
                    mismatched <- Some kind
                    closed <- true
                else
                    let before = position
                    children.Add(this.ParseElement())
                    formCount <- formCount + 1
                    if this.Position = before then this.Advance()

            let nodeChildren = children.ToArray()
            let nodeEnd = openToken.Start + (nodeChildren |> Array.sumBy Green.elementWidth)

            match mismatched with
            | Some kind ->
                this.AddDiagnostic(
                    SyntaxDiagnostics.MissingClose,
                    sprintf "Expected %O before %O." closeKind kind,
                    TextSpan.Create(openToken.Start, max 1 (nodeEnd - openToken.Start))
                )
            | None when not closed ->
                this.AddDiagnostic(
                    SyntaxDiagnostics.MissingClose,
                    sprintf "Unexpected end of input; expected %O." closeKind,
                    TextSpan.Create(openToken.Start, max 1 (nodeEnd - openToken.Start))
                )
            | None -> ()

            if nodeKind = SyntaxKind.Map && formCount % 2 <> 0 then
                this.AddDiagnostic(
                    SyntaxDiagnostics.OddMap,
                    "A map literal needs an even number of forms.",
                    TextSpan.Create(openToken.Start, max 1 (nodeEnd - openToken.Start))
                )

            nodeElement nodeKind nodeChildren

        member this.ParsePrefix(nodeKind: SyntaxKind) : GreenElement =
            let children = ResizeArray<GreenElement>()
            let marker = this.Current
            children.Add(this.TokenElement marker)
            this.Advance()
            this.CollectTrivia children
            if this.Eof || (this.CurrentKind |> Option.exists SyntaxFacts.isClose) then
                this.AddDiagnostic(
                    SyntaxDiagnostics.MissingForm,
                    "Reader prefix has no following form.",
                    this.TokenSpan marker
                )
            else
                children.Add(this.ParseElement())
            nodeElement nodeKind (children.ToArray())

        member this.ParseMetadata() : GreenElement =
            let children = ResizeArray<GreenElement>()
            let marker = this.Current
            children.Add(this.TokenElement marker)
            this.Advance()
            this.CollectTrivia children

            if this.Eof || (this.CurrentKind |> Option.exists SyntaxFacts.isClose) then
                this.AddDiagnostic(
                    SyntaxDiagnostics.MissingForm,
                    "Metadata marker needs a metadata form and a target form.",
                    this.TokenSpan marker
                )
                nodeElement SyntaxKind.MetadataNode (children.ToArray())
            else
                children.Add(this.ParseElement())
                this.CollectTrivia children
                if this.Eof || (this.CurrentKind |> Option.exists SyntaxFacts.isClose) then
                    this.AddDiagnostic(
                        SyntaxDiagnostics.MissingForm,
                        "Metadata marker has no target form.",
                        this.TokenSpan marker
                    )
                else
                    children.Add(this.ParseElement())
                nodeElement SyntaxKind.MetadataNode (children.ToArray())

        member this.ParseSymbolicValue() : GreenElement =
            let children = ResizeArray<GreenElement>()
            let marker = this.Current
            children.Add(this.TokenElement marker)
            this.Advance()
            this.CollectTrivia children
            if this.Eof || (this.CurrentKind |> Option.exists SyntaxFacts.isClose) then
                this.AddDiagnostic(
                    SyntaxDiagnostics.MissingForm,
                    "Symbolic value marker has no value.",
                    this.TokenSpan marker
                )
            else
                children.Add(this.ParseElement())
            nodeElement SyntaxKind.SymbolicValueNode (children.ToArray())

        member this.ParseTaggedLiteral() : GreenElement =
            let children = ResizeArray<GreenElement>()
            let marker = this.Current
            children.Add(this.TokenElement marker)
            this.Advance()
            this.CollectTrivia children

            if this.Eof || (this.CurrentKind |> Option.exists SyntaxFacts.isClose) then
                this.AddDiagnostic(
                    SyntaxDiagnostics.MissingForm,
                    "Tagged literal needs a tag and a value.",
                    this.TokenSpan marker
                )
                nodeElement SyntaxKind.TaggedLiteralNode (children.ToArray())
            else
                children.Add(this.ParseElement())
                this.CollectTrivia children
                if this.Eof || (this.CurrentKind |> Option.exists SyntaxFacts.isClose) then
                    this.AddDiagnostic(
                        SyntaxDiagnostics.MissingForm,
                        "Tagged literal has no value.",
                        this.TokenSpan marker
                    )
                else
                    children.Add(this.ParseElement())
                nodeElement SyntaxKind.TaggedLiteralNode (children.ToArray())

        member this.ParseNamespacedMap() : GreenElement =
            let children = ResizeArray<GreenElement>()
            let marker = this.Current
            children.Add(this.TokenElement marker)
            this.Advance()
            this.CollectTrivia children

            if not this.Eof && this.Current.Kind = SyntaxKind.Symbol then
                this.AddCurrent children
                this.CollectTrivia children

            if not this.Eof && this.Current.Kind = SyntaxKind.OpenBrace then
                children.Add(this.ParseDelimited(SyntaxKind.Map, SyntaxKind.CloseBrace))
            else
                this.AddDiagnostic(
                    SyntaxDiagnostics.MalformedDispatch,
                    "A namespaced map needs a map literal.",
                    this.TokenSpan marker
                )

            nodeElement SyntaxKind.NamespacedMapNode (children.ToArray())

        member this.ParseElement() : GreenElement =
            if this.Eof then
                nodeElement SyntaxKind.ErrorNode [||]
            else
                match this.Current.Kind with
                | SyntaxKind.OpenParen -> this.ParseDelimited(SyntaxKind.List, SyntaxKind.CloseParen)
                | SyntaxKind.OpenBracket -> this.ParseDelimited(SyntaxKind.Vector, SyntaxKind.CloseBracket)
                | SyntaxKind.OpenBrace -> this.ParseDelimited(SyntaxKind.Map, SyntaxKind.CloseBrace)
                | SyntaxKind.SetStart -> this.ParseDelimited(SyntaxKind.Set, SyntaxKind.CloseBrace)
                | SyntaxKind.AnonFnStart -> this.ParseDelimited(SyntaxKind.AnonymousFnNode, SyntaxKind.CloseParen)
                | SyntaxKind.Quote -> this.ParsePrefix(SyntaxKind.QuoteNode)
                | SyntaxKind.SyntaxQuote -> this.ParsePrefix(SyntaxKind.SyntaxQuoteNode)
                | SyntaxKind.Unquote -> this.ParsePrefix(SyntaxKind.UnquoteNode)
                | SyntaxKind.UnquoteSplicing -> this.ParsePrefix(SyntaxKind.UnquoteSplicingNode)
                | SyntaxKind.Deref -> this.ParsePrefix(SyntaxKind.DerefNode)
                | SyntaxKind.VarQuote -> this.ParsePrefix(SyntaxKind.VarQuoteNode)
                | SyntaxKind.Discard -> this.ParsePrefix(SyntaxKind.DiscardNode)
                | SyntaxKind.Metadata
                | SyntaxKind.HashCaret -> this.ParseMetadata()
                | SyntaxKind.HashEquals -> this.ParsePrefix(SyntaxKind.ReaderEvalNode)
                | SyntaxKind.ReaderConditional
                | SyntaxKind.ReaderConditionalSplicing -> this.ParsePrefix(SyntaxKind.ReaderConditionalNode)
                | SyntaxKind.HashHash -> this.ParseSymbolicValue()
                | SyntaxKind.Hash -> this.ParseTaggedLiteral()
                | SyntaxKind.HashColon
                | SyntaxKind.HashDoubleColon -> this.ParseNamespacedMap()
                | kind when SyntaxFacts.isLeaf kind ->
                    let element = this.TokenElement this.Current
                    this.Advance()
                    element
                | kind ->
                    let token = this.Current
                    this.AddDiagnostic(
                        SyntaxDiagnostics.UnexpectedToken,
                        sprintf "Unexpected token %O." kind,
                        this.TokenSpan token
                    )
                    let error = nodeElement SyntaxKind.ErrorNode [| this.TokenElement token |]
                    this.Advance()
                    error

        member this.ParseElements() =
            let elements = ResizeArray<GreenElement>()
            while not this.Eof do
                let kind = this.Current.Kind
                if SyntaxFacts.isTrivia kind then
                    this.AddCurrent elements
                elif SyntaxFacts.isClose kind then
                    let token = this.Current
                    this.AddDiagnostic(
                        SyntaxDiagnostics.UnexpectedClose,
                        sprintf "Unexpected closing delimiter %O." kind,
                        this.TokenSpan token
                    )
                    elements.Add(nodeElement SyntaxKind.ErrorNode [| this.TokenElement token |])
                    this.Advance()
                else
                    let before = position
                    elements.Add(this.ParseElement())
                    if this.Position = before then this.Advance()
            elements.ToArray()

    let parseElements (source: SourceText) (start: int) (finish: int) =
        let tokens, lexerDiagnostics = Lexer.lexRange source start finish
        let diagnostics = ResizeArray<Diagnostic>(lexerDiagnostics)
        let parser = ParserState(tokens, diagnostics)
        let elements = parser.ParseElements()
        elements, diagnostics |> Seq.toList |> List.sortBy (fun d -> d.Span.Start, d.Span.Length)

    let parseFile (source: SourceText) =
        let elements, diagnostics = parseElements source 0 source.Length
        Green.makeNode SyntaxKind.SourceFile elements, diagnostics
