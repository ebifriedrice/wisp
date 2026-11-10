namespace Wisp.Syntax

open System
open Wisp.Core

[<AbstractClass>]
type SyntaxElement() =
    abstract Kind: SyntaxKind
    abstract Offset: int
    abstract Span: TextSpan
    /// Used by incremental tests and clients that need to know whether a green
    /// node was reused after an edit. Nodes have no parent pointers.
    abstract GreenIdentity: obj

[<Sealed>]
type SyntaxToken internal (green: GreenToken, offset: int) =
    inherit SyntaxElement()

    member _.Text = green.Text
    member _.Green = green

    override _.Kind = green.Kind
    override _.Offset = offset
    override _.Span = TextSpan.Create(offset, green.Text.Length)
    override _.GreenIdentity = box green

[<Sealed>]
type SyntaxNode internal (green: GreenNode, offset: int) =
    inherit SyntaxElement()

    let children =
        let built = Array.zeroCreate<SyntaxElement> green.Children.Length
        let mutable childOffset = offset
        for index in 0 .. green.Children.Length - 1 do
            match green.Children.[index] with
            | GreenElement.Token token ->
                built.[index] <- SyntaxToken(token, childOffset) :> SyntaxElement
                childOffset <- childOffset + token.Text.Length
            | GreenElement.Node node ->
                built.[index] <- SyntaxNode(node, childOffset) :> SyntaxElement
                childOffset <- childOffset + node.Width
        built

    member _.Children = children
    member _.Green = green

    member this.DescendantTokens() : seq<SyntaxToken> =
        seq {
            for child in children do
                match child with
                | :? SyntaxToken as token -> yield token
                | :? SyntaxNode as node -> yield! node.DescendantTokens()
                | _ -> ()
        }

    member this.DescendantNodes() : seq<SyntaxNode> =
        seq {
            for child in children do
                match child with
                | :? SyntaxNode as node ->
                    yield node
                    yield! node.DescendantNodes()
                | _ -> ()
        }

    override _.Kind = green.Kind
    override _.Offset = offset
    override _.Span = TextSpan.Create(offset, green.Width)
    override _.GreenIdentity = box green

type TextChange =
    { Start: int
      OldLength: int
      NewText: string }

type SyntaxTree internal (source: SourceText, root: GreenNode, diagnostics: Diagnostic list) =
    let rootNode = SyntaxNode(root, 0)

    member _.Source = source
    member _.Root = rootNode
    member _.Diagnostics = diagnostics
    member _.Text = source.Text

    member _.TextOf(element: SyntaxElement) = source.Slice(element.Span)

    member _.Tokens = rootNode.DescendantTokens()
    member _.Nodes = rootNode.DescendantNodes()

    member this.TopLevelElements = rootNode.Children

    member this.ApplyChange(change: TextChange) =
        if isNull change.NewText then nullArg "change.NewText"
        if change.Start < 0 then invalidArg "change.Start" "A change cannot start before zero."
        if change.OldLength < 0 then invalidArg "change.OldLength" "A change cannot remove a negative length."
        if change.Start + change.OldLength > source.Length then
            invalidArg "change" "A change cannot extend past the end of the source."

        if change.OldLength = 0 && change.NewText.Length = 0 then
            this
        else
            let oldText = source.Text
            let editEnd = change.Start + change.OldLength
            let newText =
                oldText.Substring(0, change.Start)
                + change.NewText
                + oldText.Substring(editEnd)
            let newSource =
                match source.Path with
                | Some path -> SourceText.Create(newText, path)
                | None -> SourceText.Create newText
            let delta = newText.Length - oldText.Length

            let elements = root.Children
            let elementCount = elements.Length
            let starts = Array.zeroCreate<int> (elementCount + 1)
            for index in 0 .. elementCount - 1 do
                starts.[index + 1] <- starts.[index] + Green.elementWidth elements.[index]
            let oldLength = starts.[elementCount]

            let elementKind element =
                match element with
                | GreenElement.Token token -> token.Kind
                | GreenElement.Node node -> node.Kind
            let isForm index = not (SyntaxFacts.isTrivia (elementKind elements.[index]))
            let isContained index =
                isForm index && starts.[index] <= change.Start && editEnd <= starts.[index + 1]
            let strictContained index =
                isForm index && starts.[index] < change.Start && editEnd < starts.[index + 1]

            let initialWindow =
                let isolated =
                    if change.OldLength > 0 then
                        seq { 0 .. elementCount - 1 } |> Seq.tryFind isContained
                    else
                        seq { 0 .. elementCount - 1 } |> Seq.tryFind strictContained
                match isolated with
                | Some formIndex ->
                    let startIndex =
                        if formIndex > 0 && isForm (formIndex - 1) then formIndex - 1 else formIndex
                    startIndex, formIndex + 1
                | None ->
                    let firstTouched =
                        let mutable index = 0
                        while index < elementCount && starts.[index + 1] <= change.Start do
                            index <- index + 1
                        index
                    let lastTouched =
                        let mutable index = firstTouched
                        while index < elementCount && starts.[index] < editEnd do
                            index <- index + 1
                        max (firstTouched - 1) (index - 1)

                    let forms = ResizeArray<int>()
                    for index in 0 .. elementCount - 1 do
                        if isForm index then forms.Add index

                    let firstForm =
                        forms
                        |> Seq.tryFind (fun index -> index >= firstTouched)
                        |> Option.defaultValue (if forms.Count > 0 then forms.[forms.Count - 1] else -1)
                    let lastForm =
                        forms
                        |> Seq.filter (fun index -> index <= lastTouched)
                        |> Seq.tryLast
                        |> Option.defaultValue firstForm

                    let startForm =
                        if firstForm < 0 then None
                        else
                            forms
                            |> Seq.filter (fun index -> index < firstForm)
                            |> Seq.tryLast
                            |> Option.orElse (Some firstForm)
                    let endForm =
                        if lastForm < 0 then None
                        else
                            forms
                            |> Seq.tryFind (fun index -> index > lastForm)
                            |> Option.orElse (Some lastForm)

                    let startIndex = startForm |> Option.defaultValue 0
                    let suffixIndex =
                        match endForm with
                        | Some index -> index + 1
                        | None -> elementCount
                    startIndex, suffixIndex

            let initialStartIndex, initialSuffixIndex = initialWindow

            let nextSuffix index =
                let mutable formIndex = index
                while formIndex < elementCount && not (isForm formIndex) do
                    formIndex <- formIndex + 1
                if formIndex < elementCount then formIndex + 1 else elementCount

            let rec firstToken (element: GreenElement) =
                match element with
                | GreenElement.Token token -> Some token
                | GreenElement.Node node ->
                    let mutable result = None
                    let mutable index = 0
                    while result.IsNone && index < node.Children.Length do
                        result <- firstToken node.Children.[index]
                        index <- index + 1
                    result

            let rec lastToken (element: GreenElement) =
                match element with
                | GreenElement.Token token -> Some token
                | GreenElement.Node node ->
                    let mutable result = None
                    let mutable index = node.Children.Length - 1
                    while result.IsNone && index >= 0 do
                        result <- lastToken node.Children.[index]
                        index <- index - 1
                    result

            let leftBoundaryMerges reparseStartIndex (parsedElements: GreenElement[]) =
                if reparseStartIndex <= 0 || parsedElements.Length = 0 then
                    false
                else
                    let regionStart = starts.[reparseStartIndex]
                    let left = lastToken elements.[reparseStartIndex - 1]
                    let right = firstToken parsedElements.[0]
                    match left, right with
                    | Some left, Some right ->
                        let next = if regionStart < newText.Length then newText.[regionStart] else '\000'
                        match left.Kind with
                        | SyntaxKind.Whitespace -> right.Kind = SyntaxKind.Whitespace
                        | SyntaxKind.Comment
                        | SyntaxKind.Shebang -> next <> '\n'
                        | SyntaxKind.Character when left.Text = "\\" -> true
                        | SyntaxKind.Character
                        | SyntaxKind.Symbol
                        | SyntaxKind.Number
                        | SyntaxKind.Keyword
                        | SyntaxKind.Nil
                        | SyntaxKind.True
                        | SyntaxKind.False -> not (Lexer.isAtomTerminator next)
                        | SyntaxKind.Hash -> not (Char.IsWhiteSpace next || next = ',')
                        | SyntaxKind.HashColon
                        | SyntaxKind.HashDoubleColon -> true
                        | SyntaxKind.Unquote
                        | SyntaxKind.ReaderConditional -> next = '@'
                        | _ -> false
                    | _ -> false

            let tokenBoundaryMerges (parsedElements: GreenElement[]) suffixNewStart =
                if suffixNewStart >= newText.Length || parsedElements.Length = 0 then
                    false
                else
                    let last = lastToken parsedElements.[parsedElements.Length - 1]
                    let next = newText.[suffixNewStart]
                    match last with
                    | None -> false
                    | Some token ->
                        match token.Kind with
                        | SyntaxKind.Whitespace -> Char.IsWhiteSpace next
                        | SyntaxKind.Comment
                        | SyntaxKind.Shebang -> next <> '\n'
                        | SyntaxKind.Character when token.Text = "\\" -> true
                        | SyntaxKind.Character
                        | SyntaxKind.Symbol
                        | SyntaxKind.Number
                        | SyntaxKind.Keyword
                        | SyntaxKind.Nil
                        | SyntaxKind.True
                        | SyntaxKind.False -> not (Lexer.isAtomTerminator next)
                        | SyntaxKind.Hash -> not (Char.IsWhiteSpace next || next = ',')
                        | SyntaxKind.HashColon
                        | SyntaxKind.HashDoubleColon -> true
                        | SyntaxKind.Unquote
                        | SyntaxKind.ReaderConditional -> next = '@'
                        | _ -> false

            let rec parseRegion reparseStartIndex suffixIndex attempts =
                let regionStart = if reparseStartIndex < elementCount then starts.[reparseStartIndex] else 0
                let suffixOldStart = if suffixIndex < elementCount then starts.[suffixIndex] else oldLength
                let suffixNewStart = suffixOldStart + delta
                if suffixNewStart < regionStart || suffixNewStart > newText.Length then
                    None
                else
                    let parsedElements, parsedDiagnostics = Parser.parseElements newSource regionStart suffixNewStart
                    let structuralFailure =
                        parsedDiagnostics
                        |> List.exists (fun diagnostic ->
                            diagnostic.Code = SyntaxDiagnostics.MissingClose
                            || diagnostic.Code = SyntaxDiagnostics.UnexpectedClose
                            || diagnostic.Code = SyntaxDiagnostics.MissingForm
                            || diagnostic.Code = SyntaxDiagnostics.MalformedDispatch
                            || diagnostic.Code = SyntaxDiagnostics.UnterminatedString
                            || diagnostic.Code = SyntaxDiagnostics.UnterminatedRegex)
                    let boundaryFailed = structuralFailure || tokenBoundaryMerges parsedElements suffixNewStart
                    let emptyBoundary =
                        parsedElements.Length = 0 && reparseStartIndex > 0 && suffixIndex < elementCount
                    if (emptyBoundary || leftBoundaryMerges reparseStartIndex parsedElements) && reparseStartIndex > 0 && attempts < elementCount + 2 then
                        parseRegion (reparseStartIndex - 1) suffixIndex (attempts + 1)
                    elif boundaryFailed && suffixIndex < elementCount && attempts < elementCount + 2 then
                        parseRegion reparseStartIndex (nextSuffix suffixIndex) (attempts + 1)
                    else
                        Some(reparseStartIndex, suffixIndex, regionStart, suffixOldStart, parsedElements, parsedDiagnostics)

            match parseRegion initialStartIndex initialSuffixIndex 0 with
            | None -> SyntaxTree.Parse newSource
            | Some(startIndex, suffixIndex, regionStart, suffixOldStart, parsedElements, parsedDiagnostics) ->
                if regionStart > change.Start || suffixOldStart < editEnd then
                    SyntaxTree.Parse newSource
                else
                    let prefix =
                        if startIndex > 0 then elements.[0 .. startIndex - 1] else Array.empty<GreenElement>
                    let suffix =
                        if suffixIndex < elementCount then elements.[suffixIndex ..] else Array.empty<GreenElement>
                    let children = Array.concat [| prefix; parsedElements; suffix |]
                    let newRoot = Green.makeNode SyntaxKind.SourceFile children

                    if newRoot.Width <> newText.Length then
                        SyntaxTree.Parse newSource
                    else
                        let shiftSpan (span: TextSpan) =
                            { span with Start = span.Start + delta }
                        let shiftDiagnostic (diagnostic: Diagnostic) =
                            { diagnostic with
                                Span = shiftSpan diagnostic.Span
                                Labels = diagnostic.Labels |> List.map (fun label -> { label with Span = shiftSpan label.Span }) }
                        let reusedPrefix =
                            diagnostics |> List.filter (fun diagnostic -> diagnostic.Span.End <= regionStart)
                        let reusedSuffix =
                            diagnostics
                            |> List.filter (fun diagnostic -> diagnostic.Span.Start >= suffixOldStart)
                            |> List.map shiftDiagnostic
                        let combined =
                            reusedPrefix @ parsedDiagnostics @ reusedSuffix
                            |> List.sortBy (fun diagnostic -> diagnostic.Span.Start, diagnostic.Span.Length)
                        SyntaxTree(newSource, newRoot, combined)

    static member Parse(source: SourceText) =
        let root, diagnostics = Parser.parseFile source
        SyntaxTree(source, root, diagnostics)

    static member Parse(text: string) = SyntaxTree.Parse(SourceText.Create text)
