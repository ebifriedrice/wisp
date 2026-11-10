namespace Wisp.Reader

open System
open System.Collections.Generic
open System.Globalization
open System.Numerics
open System.Text
open System.Text.RegularExpressions
open Wisp.Core

type ReaderOptions =
    { CurrentNamespace: string
      ResolveAlias: string -> string option
      AttachSourceMetadata: bool
      Features: Set<string> }

    static member Default =
        { CurrentNamespace = "user"
          ResolveAlias = fun _ -> None
          AttachSourceMetadata = true
          Features = Set.ofList [ "wisp"; "clr" ] }

type ReadResult =
    { Forms: obj list
      Diagnostics: Diagnostic list
      Position: int }

type ReadOneResult =
    { Form: obj option
      Forms: obj list
      Diagnostics: Diagnostic list
      Position: int }

type ReadFormsResult =
    { Forms: obj list
      Diagnostics: Diagnostic list
      Position: int }

module ReaderDiagnostics =
    [<Literal>]
    let UnexpectedEof = "WISP1001"

    [<Literal>]
    let UnmatchedDelimiter = "WISP1002"

    [<Literal>]
    let MismatchedDelimiter = "WISP1003"

    [<Literal>]
    let InvalidNumber = "WISP1004"

    [<Literal>]
    let InvalidEscape = "WISP1005"

    [<Literal>]
    let InvalidCharacter = "WISP1006"

    [<Literal>]
    let InvalidKeyword = "WISP1007"

    [<Literal>]
    let InvalidSymbol = "WISP1008"

    [<Literal>]
    let InvalidMetadata = "WISP1009"

    [<Literal>]
    let MetadataNotSupported = "WISP1010"

    [<Literal>]
    let OddMap = "WISP1011"

    [<Literal>]
    let DuplicateMapKey = "WISP1012"

    [<Literal>]
    let DuplicateSetElement = "WISP1013"

    [<Literal>]
    let UnknownDispatch = "WISP1014"

    [<Literal>]
    let InvalidRegex = "WISP1015"

    [<Literal>]
    let ReaderConditionalUnsupported = "WISP1016"

    [<Literal>]
    let ReaderEvalUnsupported = "WISP1017"

    [<Literal>]
    let AnonymousFnError = "WISP1018"

    [<Literal>]
    let NamespacedMapError = "WISP1019"

    [<Literal>]
    let UnknownAlias = "WISP1020"

    [<Literal>]
    let InvalidTaggedLiteral = "WISP1021"

    [<Literal>]
    let InvalidToken = "WISP1022"

    [<Literal>]
    let InvalidReaderConditional = "WISP1023"

    [<Literal>]
    let ReaderConditionalSpliceContext = "WISP1024"

    [<Literal>]
    let InvalidSyntaxQuote = "WISP1025"

module private Tokens =
    let isWhitespace (ch: char) = Char.IsWhiteSpace ch || ch = ','

    let isTerminating (ch: char) =
        isWhitespace ch
        || ch = ';'
        || ch = '"'
        || ch = '('
        || ch = ')'
        || ch = '['
        || ch = ']'
        || ch = '{'
        || ch = '}'
        || ch = '\\'
        || ch = '^'
        || ch = '@'
        || ch = '`'
        || ch = '~'

    let isClosing (ch: char) = ch = ')' || ch = ']' || ch = '}'

    let looksNumeric (token: string) =
        if String.IsNullOrEmpty token then false
        elif Char.IsDigit token.[0] then true
        elif (token.[0] = '+' || token.[0] = '-') && token.Length > 1 && Char.IsDigit token.[1] then true
        elif token.[0] = '.' && token.Length > 1 && Char.IsDigit token.[1] then true
        else false

    let digitValue (ch: char) =
        if ch >= '0' && ch <= '9' then int ch - int '0'
        elif ch >= 'a' && ch <= 'z' then int ch - int 'a' + 10
        elif ch >= 'A' && ch <= 'Z' then int ch - int 'A' + 10
        else -1

exception private ReadException of Diagnostic

[<Sealed>]
type private Splice(items: obj list) =
    member _.Items = items

type private AnonFnState(id: int) =
    let parameters = Dictionary<int, Symbol>()
    let mutable maxArgument = 0
    let mutable hasRest = false

    member _.Id = id
    member _.MaxArgument = maxArgument
    member _.HasRest = hasRest

    member this.Parameter(index: int) =
        maxArgument <- max maxArgument index
        match parameters.TryGetValue index with
        | true, symbol -> symbol
        | false, _ ->
            let symbol = Symbol.Create(sprintf "p%d__%d#" index id)
            parameters.[index] <- symbol
            symbol

    member this.Rest =
        hasRest <- true
        Symbol.Create(sprintf "rest__%d#" id)

type private ReaderState(source: SourceText, options: ReaderOptions, startPosition: int) =
    let text = source.Text
    let diagnostics = ResizeArray<Diagnostic>()
    let anonFns = ResizeArray<AnonFnState>()
    let mutable position = startPosition
    let mutable nextAnonId = 0
    let mutable syntaxQuoteDepth = 0
    let mutable nextSyntaxQuoteId = 0

    member _.Position = position
    member _.Diagnostics = diagnostics |> Seq.toList
    member _.Eof = position >= text.Length

    member _.TryPeek(offset: int) =
        let index = position + offset
        if index >= 0 && index < text.Length then Some text.[index] else None

    member _.Advance() =
        if position < text.Length then position <- position + 1

    member _.Span(start: int, length: int) =
        let line, column = source.LineColumn start
        TextSpan.Create(start, max 0 (min length (text.Length - start)), source.Path, line, column)

    member this.Fail(code: string, message: string, span: TextSpan) : 'T =
        raise (ReadException(Diagnostic.create code DiagnosticSeverity.Error message span))

    member this.SkipLine() =
        while not this.Eof && text.[position] <> '\n' do
            position <- position + 1
        if not this.Eof then position <- position + 1

    member this.SkipWhitespace() =
        let mutable skipping = true
        while skipping && not this.Eof do
            let ch = text.[position]
            if Tokens.isWhitespace ch then
                position <- position + 1
            elif ch = ';' then
                this.SkipLine()
            else
                skipping <- false

    member this.ReadToken() =
        let start = position
        let builder = StringBuilder()
        let mutable running = true
        while running && not this.Eof do
            let ch = text.[position]
            if Tokens.isTerminating ch then
                running <- false
            else
                builder.Append ch |> ignore
                position <- position + 1
        builder.ToString(), start

    member this.ReadRequired(context: string) =
        match this.ReadNext() with
        | Some (:? Splice) ->
            this.Fail(
                ReaderDiagnostics.ReaderConditionalSpliceContext,
                sprintf "A splicing reader conditional is not valid after %s." context,
                this.Span(position, 0)
            )
        | Some form -> form
        | None ->
            this.Fail(
                ReaderDiagnostics.UnexpectedEof,
                sprintf "Unexpected end of input after %s." context,
                this.Span(position, 0)
            )

    member private this.AttachSource(value: obj, start: int, finish: int) =
        if not options.AttachSourceMetadata then
            value
        else
            match value with
            | :? IObj as target ->
                let line, column = source.LineColumn start
                let sourceMetadata =
                    let withStart = PersistentMap.Empty.Assoc(SourceMetadata.startKey, box start)
                    let withEnd = withStart.Assoc(SourceMetadata.endKey, box finish)
                    let withLine = withEnd.Assoc(SourceMetadata.lineKey, box line)
                    let withColumn = withLine.Assoc(SourceMetadata.columnKey, box column)
                    match source.Path with
                    | Some path -> withColumn.Assoc(SourceMetadata.pathKey, box path)
                    | None -> withColumn
                target.WithMeta(Meta.merge target.Meta (Some sourceMetadata))
            | _ -> value

    member this.ReadNext() : obj option =
        this.SkipWhitespace()
        if this.Eof then
            None
        else
            let start = position
            match this.ReadForm() with
            | Some value -> Some(this.AttachSource(value, start, position))
            | None -> None

    member private this.ReadForm() : obj option =
        if this.Eof then
            None
        else
            let ch = text.[position]
            match ch with
            | ')' | ']' | '}' ->
                this.Fail(
                    ReaderDiagnostics.UnmatchedDelimiter,
                    sprintf "Unmatched delimiter '%c'." ch,
                    this.Span(position, 1)
                )
            | '(' ->
                let start = position
                this.Advance()
                let items = this.ReadDelimited(')', start, "list")
                Some(PersistentList.OfSeq items :> obj)
            | '[' ->
                let start = position
                this.Advance()
                let items = this.ReadDelimited(']', start, "vector")
                Some(PersistentVector.OfSeq items :> obj)
            | '{' ->
                let start = position
                this.Advance()
                Some(this.ReadMapBody('}', start, id))
            | '\'' ->
                let start = position
                this.Advance()
                Some(this.ReadQuoted("quote", start))
            | '`' ->
                let start = position
                this.Advance()
                Some(this.ReadSyntaxQuote start)
            | '~' ->
                let start = position
                if syntaxQuoteDepth = 0 then
                    this.Fail(
                        ReaderDiagnostics.InvalidSyntaxQuote,
                        "Unquote is only valid inside a syntax quote.",
                        this.Span(position, 1)
                    )
                this.Advance()
                if this.TryPeek 0 = Some '@' then
                    this.Advance()
                    Some(this.ReadQuoted("unquote-splicing", start))
                else
                    Some(this.ReadQuoted("unquote", start))
            | '@' ->
                let start = position
                this.Advance()
                Some(this.ReadQuoted("deref", start))
            | '^' ->
                let start = position
                this.Advance()
                Some(this.ReadMetadata start)
            | '\\' ->
                Some(this.ReadCharacter())
            | '"' ->
                Some(this.ReadString() :> obj)
            | ':' ->
                Some(this.ReadKeyword() :> obj)
            | '#' ->
                this.ReadDispatch()
            | _ ->
                Some(this.ReadAtom())

    member private this.ReadPrefixTarget(context: string) =
        match this.ReadNext() with
        | Some target -> target
        | None ->
            this.Fail(
                ReaderDiagnostics.UnexpectedEof,
                sprintf "Unexpected end of input after %s." context,
                this.Span(position, 0)
            )

    member private this.WrapPrefix(name: string, start: int, target: obj) =
        match target with
        | :? Splice as splice ->
            let wrap item =
                let form = PersistentList.OfSeq [ Symbol.Create name; item ] :> obj
                this.AttachSource(form, start, position)
            Splice(splice.Items |> List.map wrap) :> obj
        | _ -> PersistentList.OfSeq [ Symbol.Create name; target ] :> obj

    member this.ReadQuoted(name: string, start: int) =
        this.WrapPrefix(name, start, this.ReadPrefixTarget name)

    member private this.SyntaxCall(name: string, arguments: obj list) =
        PersistentList.OfSeq (Symbol.Create name :: arguments) :> obj

    member this.ReadSyntaxQuote(start: int) : obj =
        syntaxQuoteDepth <- syntaxQuoteDepth + 1
        let target =
            try this.ReadPrefixTarget "syntax quote"
            finally syntaxQuoteDepth <- syntaxQuoteDepth - 1
        this.ExpandSyntaxQuote(start, target)

    member private this.ExpandSyntaxQuote(start: int, target: obj) : obj =
        match target with
        | :? Splice as splice ->
            Splice(splice.Items |> List.map (fun item -> this.ExpandSyntaxQuote(start, item))) :> obj
        | _ -> this.ExpandSyntaxQuoteSingle(start, target)

    member private this.ExpandSyntaxQuoteSingle(start: int, target: obj) : obj =
        let quoteId = nextSyntaxQuoteId
        nextSyntaxQuoteId <- nextSyntaxQuoteId + 1
        let gensyms = Dictionary<string, Symbol>()

        let spanOf (form: obj) =
            SourceMetadata.trySpan form |> Option.defaultValue (this.Span(start, max 1 (position - start)))

        let fail form message : 'T =
            this.Fail(ReaderDiagnostics.InvalidSyntaxQuote, message, spanOf form)

        let specialList (form: obj) =
            match form with
            | :? PersistentList as list ->
                let items = list.Items |> Seq.toList
                match items with
                | head :: rest ->
                    match head with
                    | :? Symbol as symbol when symbol.Namespace.IsNone -> Some(symbol.Name, rest)
                    | _ -> None
                | [] -> None
            | _ -> None

        let isSpliceable (value: obj) =
            match value with
            | null -> true
            | :? ISeqable
            | :? ISeq
            | :? IReadOnlyList<obj> -> true
            | _ -> false

        // Nested syntax quotes are read and expanded inside-out by the reader;
        // the outer quote then sees the inner expansion as ordinary data. This
        // also leaves escaped nested unquotes available to the outer level.
        let rec expand (form: obj) : obj =
            match specialList form with
            | Some("unquote", [ value ]) -> value
            | Some("unquote", _) -> fail form "Unquote expects exactly one form."
            | Some("unquote-splicing", _) ->
                fail form "Unquote-splicing is only valid as an element of a collection template."
            | _ ->
                match form with
                | :? PersistentList as list ->
                    let items = list.Items |> Seq.toList
                    if items.IsEmpty then
                        this.SyntaxCall("list", [])
                    else
                        this.SyntaxCall("concat", expandParts items)
                | :? PersistentVector as vector ->
                    let concatenated = this.SyntaxCall("concat", expandParts (vector.Elements |> Seq.toList))
                    this.SyntaxCall("apply", [ Symbol.Create "vector" :> obj; concatenated ])
                | :? PersistentSet as set ->
                    let concatenated = this.SyntaxCall("concat", expandParts (set.Elements |> Seq.toList))
                    this.SyntaxCall("apply", [ Symbol.Create "hash-set" :> obj; concatenated ])
                | :? PersistentMap as map ->
                    // Splicing is at the flat key/value sequence level, matching
                    // hash-map's argument list, so a splice may supply both a
                    // value and the following key. Literal maps carry their
                    // source-ordered forms because hash order is not stable.
                    let forms =
                        match map.Meta with
                        | Some metadata ->
                            match metadata.TryGet(SourceMetadata.mapFormsKey) with
                            | Some (:? PersistentVector as ordered) -> ordered.Elements |> Seq.toList
                            | _ ->
                                map.Entries
                                |> Seq.toList
                                |> List.collect (fun entry -> [ entry.Key; entry.Value ])
                        | None ->
                            map.Entries
                            |> Seq.toList
                            |> List.collect (fun entry -> [ entry.Key; entry.Value ])
                    let concatenated = this.SyntaxCall("concat", expandParts forms)
                    this.SyntaxCall("apply", [ Symbol.Create "hash-map" :> obj; concatenated ])
                | :? Symbol as symbol ->
                    if symbol.Namespace.IsNone && symbol.Name.EndsWith("#", StringComparison.Ordinal) then
                        let baseName = symbol.Name.Substring(0, symbol.Name.Length - 1)
                        if baseName.Length = 0 then fail form "Auto-gensym symbol needs a name before #."
                        let generated =
                            match gensyms.TryGetValue symbol.Name with
                            | true, existing -> existing
                            | false, _ ->
                                let generated = Symbol.Create(sprintf "%s__%d__auto__" baseName quoteId)
                                gensyms.[symbol.Name] <- generated
                                generated
                        this.SyntaxCall("quote", [ generated :> obj ])
                    else
                        this.SyntaxCall("quote", [ symbol :> obj ])
                | _ -> form

        and expandParts (items: obj list) : obj list =
            [ for item in items ->
                  match specialList item with
                  | Some("unquote", [ value ]) -> this.SyntaxCall("list", [ value ])
                  | Some("unquote", _) -> fail item "Unquote expects exactly one form."
                  | Some("unquote-splicing", [ value ]) -> value
                  | Some("unquote-splicing", _) -> fail item "Unquote-splicing expects exactly one form."
                  | _ -> this.SyntaxCall("list", [ expand item ]) ]

        let expanded = expand target
        if isSpliceable expanded then
            let line, column = source.LineColumn start
            let sourceMetadata =
                let withStart = PersistentMap.Empty.Assoc(SourceMetadata.startKey, box start)
                let withEnd = withStart.Assoc(SourceMetadata.endKey, box position)
                let withLine = withEnd.Assoc(SourceMetadata.lineKey, box line)
                withLine.Assoc(SourceMetadata.columnKey, box column)
            match expanded with
            | :? IObj as obj -> obj.WithMeta(Meta.merge obj.Meta (Some sourceMetadata)) :> obj
            | _ -> expanded
        else
            expanded

    member this.ReadReaderConditional(splicing: bool, start: int) : obj =
        let conditional = this.ReadRequired "reader conditional"
        let span =
            SourceMetadata.trySpan conditional
            |> Option.defaultValue (this.Span(start, max 1 (position - start)))

        let fail message (form: obj) : 'T =
            this.Fail(
                ReaderDiagnostics.InvalidReaderConditional,
                message,
                SourceMetadata.trySpan form |> Option.defaultValue span
            )

        match conditional with
        | :? PersistentList as clauses ->
            let items = clauses.Items |> Seq.toList
            if items.Length % 2 <> 0 then
                fail "A reader conditional needs feature/form pairs." conditional
            let rec choose (remaining: obj list) : obj option =
                match remaining with
                | feature :: form :: rest ->
                    match (feature: obj) with
                    | :? Keyword as keyword ->
                        let matches =
                            (keyword.Namespace.IsNone && keyword.Name = "default")
                            || options.Features.Contains keyword.FullName
                            || (keyword.Namespace.IsNone && options.Features.Contains keyword.Name)
                        if matches then Some form else choose rest
                    | _ -> fail "Reader conditional features must be keywords." feature
                | [] -> None
                | _ -> fail "A reader conditional needs feature/form pairs." conditional

            match choose items with
            | None -> if splicing then Splice [] :> obj else null
            | Some selected when not splicing -> selected
            | Some selected ->
                let elements =
                    match selected with
                    | null -> []
                    | :? ISeqable
                    | :? ISeq
                    | :? IReadOnlyList<obj> -> Seqs.toList selected
                    | _ -> fail "A splicing reader conditional must select a collection." selected
                Splice elements :> obj
        | _ -> fail "A reader conditional must be followed by a list of feature/form pairs." conditional

    member this.ReadDelimited(close: char, openStart: int, kind: string) =
        let items = ResizeArray<obj>()
        let mutable finished = false
        while not finished do
            this.SkipWhitespace()
            if this.Eof then
                this.Fail(
                    ReaderDiagnostics.UnexpectedEof,
                    sprintf "Unexpected end of input while reading %s." kind,
                    this.Span(openStart, position - openStart)
                )
            else
                let ch = text.[position]
                if ch = close then
                    this.Advance()
                    finished <- true
                elif Tokens.isClosing ch then
                    this.Fail(
                        ReaderDiagnostics.MismatchedDelimiter,
                        sprintf "Mismatched delimiter: expected '%c', found '%c'." close ch,
                        this.Span(position, 1)
                    )
                else
                    match this.ReadNext() with
                    | Some (:? Splice as splice) -> items.AddRange splice.Items
                    | Some form -> items.Add form
                    | None -> ()
        items |> Seq.toList

    member this.BuildMap(forms: obj list, start: int) =
        if forms.Length % 2 <> 0 then
            this.Fail(
                ReaderDiagnostics.OddMap,
                "A map literal needs an even number of forms.",
                this.Span(start, position - start)
            )
        let mutable map = PersistentMap.Empty
        let rec loop remaining =
            match remaining with
            | [] -> map
            | key :: value :: rest ->
                if map.ContainsKey key then
                    this.Fail(
                        ReaderDiagnostics.DuplicateMapKey,
                        sprintf "Duplicate map key %O." key,
                        this.Span(start, position - start)
                    )
                map <- map.Assoc(key, value)
                loop rest
            | _ -> map
        let ordered = PersistentVector.OfSeq forms :> obj
        let metadata = PersistentMap.Empty.Assoc(SourceMetadata.mapFormsKey, ordered)
        (loop forms).WithMeta(Some metadata)

    member this.ReadMapBody(close: char, start: int, transform: obj -> obj) =
        let forms = this.ReadDelimited(close, start, "map")
        forms |> List.map transform |> fun forms -> this.BuildMap(forms, start)

    member this.BuildSet(forms: obj list, start: int) =
        let mutable set = PersistentSet.Empty
        for form in forms do
            if set.Contains form then
                this.Fail(
                    ReaderDiagnostics.DuplicateSetElement,
                    sprintf "Duplicate set element %O." form,
                    this.Span(start, position - start)
                )
            set <- set.Add form
        set

    member this.ReadKeyword() =
        let start = position
        this.Advance()
        let autoResolved = this.TryPeek 0 = Some ':'
        if autoResolved then this.Advance()
        let token, _ = this.ReadToken()
        if String.IsNullOrEmpty token then
            this.Fail(ReaderDiagnostics.InvalidKeyword, "A keyword cannot be empty.", this.Span(start, position - start))
        let slash = token.IndexOf '/'
        let hasNamespace = slash > 0 && slash < token.Length - 1
        if (slash = 0 || slash = token.Length - 1) then
            this.Fail(
                ReaderDiagnostics.InvalidKeyword,
                sprintf "Invalid keyword ':%s'." token,
                this.Span(start, position - start)
            )
        if autoResolved then
            if hasNamespace then
                let alias = token.Substring(0, slash)
                let name = token.Substring(slash + 1)
                match options.ResolveAlias alias with
                | Some target -> Keyword.Intern(target, name)
                | None ->
                    this.Fail(
                        ReaderDiagnostics.UnknownAlias,
                        sprintf "Unknown namespace alias '%s'." alias,
                        this.Span(start, position - start)
                    )
            elif String.IsNullOrEmpty options.CurrentNamespace then
                Keyword.Intern token
            else
                Keyword.Intern(options.CurrentNamespace, token)
        elif hasNamespace then
            Keyword.Intern(token.Substring(0, slash), token.Substring(slash + 1))
        else
            Keyword.Intern(token)

    member this.ReadAtom() : obj =
        let token, start = this.ReadToken()
        if String.IsNullOrEmpty token then
            this.Fail(ReaderDiagnostics.InvalidToken, "Expected a token.", this.Span(start, max 1 (position - start)))
        match token with
        | "nil" -> null
        | "true" -> box true
        | "false" -> box false
        | _ when Tokens.looksNumeric token -> this.ReadNumber(token, start)
        | _ when token.StartsWith("%", StringComparison.Ordinal) && anonFns.Count > 0 -> this.ReadAnonymousArgument(token, start)
        | _ -> this.ReadSymbol(token, start)

    member this.ReadAnonymousArgument(token: string, start: int) =
        let current = anonFns.[anonFns.Count - 1]
        if token = "%" then
            current.Parameter 1 :> obj
        elif token = "%&" then
            current.Rest :> obj
        else
            match Int32.TryParse(token.Substring 1, NumberStyles.None, CultureInfo.InvariantCulture) with
            | true, index when index > 0 -> current.Parameter index :> obj
            | _ ->
                this.Fail(
                    ReaderDiagnostics.AnonymousFnError,
                    sprintf "Invalid anonymous function argument '%s'." token,
                    this.Span(start, token.Length)
                )

    member this.ReadSymbol(token: string, start: int) =
        if token = "/" then
            Symbol.Create "/" :> obj
        elif token.IndexOf '/' >= 0 then
            let slash = token.IndexOf '/'
            if slash = 0 || slash = token.Length - 1 then
                this.Fail(
                    ReaderDiagnostics.InvalidSymbol,
                    sprintf "Invalid symbol '%s'." token,
                    this.Span(start, token.Length)
                )
            Symbol.Create(token.Substring(0, slash), token.Substring(slash + 1)) :> obj
        else
            Symbol.Create token :> obj

    member this.ReadNumber(token: string, start: int) : obj =
        let span = this.Span(start, token.Length)
        let invalid message =
            this.Fail(ReaderDiagnostics.InvalidNumber, sprintf "Invalid number '%s': %s" token message, span)
        let boxInteger (value: BigInteger) =
            if value >= BigInteger Int64.MinValue && value <= BigInteger Int64.MaxValue then
                box (int64 value)
            else
                box value
        let applySign negative (value: BigInteger) =
            if negative then -value else value

        let signing, body =
            if token.StartsWith("-", StringComparison.Ordinal) then true, token.Substring 1
            elif token.StartsWith("+", StringComparison.Ordinal) then false, token.Substring 1
            else false, token

        if body.Length = 0 then invalid "the number has no digits."
        elif body.Contains "/" then
            let parts = body.Split '/'
            if parts.Length <> 2 || parts.[0].Length = 0 || parts.[1].Length = 0 then
                invalid "a ratio needs a numerator and denominator."
            let parsePart (value: string) =
                match BigInteger.TryParse(value, NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture) with
                | true, number -> number
                | _ -> invalid "a ratio needs integer parts."
            let numerator = applySign signing (parsePart parts.[0])
            let denominator = parsePart parts.[1]
            if denominator.IsZero then invalid "a ratio denominator cannot be zero."
            Ratio.Create(numerator, denominator) :> obj
        elif body.EndsWith("N", StringComparison.Ordinal) then
            let digits = body.Substring(0, body.Length - 1)
            if digits.Length = 0 then invalid "the bigint has no digits."
            match BigInteger.TryParse(digits, NumberStyles.None, CultureInfo.InvariantCulture) with
            | true, value -> applySign signing value |> box
            | _ -> invalid "the bigint has invalid digits."
        elif body.EndsWith("M", StringComparison.Ordinal) then
            let digits = body.Substring(0, body.Length - 1)
            if digits.Length = 0 then invalid "the decimal has no digits."
            match Decimal.TryParse(digits, NumberStyles.Float, CultureInfo.InvariantCulture) with
            | true, value -> (if signing then -value else value) |> box
            | _ -> invalid "the decimal has invalid digits."
        elif body.StartsWith("0x", StringComparison.OrdinalIgnoreCase) then
            let digits = body.Substring 2
            if digits.Length = 0 then invalid "the hexadecimal literal has no digits."
            let mutable value = BigInteger.Zero
            for ch in digits do
                let digit = Tokens.digitValue ch
                if digit < 0 || digit >= 16 then invalid "the hexadecimal literal has an invalid digit."
                value <- value * BigInteger(16) + BigInteger(digit)
            applySign signing value |> boxInteger
        else
            let radixIndex = body.IndexOfAny([| 'r'; 'R' |])
            if radixIndex > 0 then
                let radixText = body.Substring(0, radixIndex)
                let digits = body.Substring(radixIndex + 1)
                match Int32.TryParse(radixText, NumberStyles.None, CultureInfo.InvariantCulture) with
                | true, radix when radix >= 2 && radix <= 36 && digits.Length > 0 ->
                    let mutable value = BigInteger.Zero
                    for ch in digits do
                        let digit = Tokens.digitValue ch
                        if digit < 0 || digit >= radix then invalid (sprintf "digit '%c' is not valid in radix %d." ch radix)
                        value <- value * BigInteger(radix) + BigInteger(digit)
                    applySign signing value |> boxInteger
                | _ -> invalid "the radix literal is malformed."
            elif body.Length > 1 && body.[0] = '0' && body |> Seq.forall (fun ch -> ch >= '0' && ch <= '7') then
                let mutable value = BigInteger.Zero
                for ch in body do
                    value <- value * BigInteger(8) + BigInteger(Tokens.digitValue ch)
                applySign signing value |> boxInteger
            elif body.Contains "." || body.Contains "e" || body.Contains "E" then
                match Double.TryParse(body, NumberStyles.Float, CultureInfo.InvariantCulture) with
                | true, value -> (if signing then -value else value) |> box
                | _ -> invalid "the floating point literal has invalid digits."
            else
                match Int64.TryParse(body, NumberStyles.None, CultureInfo.InvariantCulture) with
                | true, value ->
                    if signing then
                        if value = Int64.MinValue then box value else box (-value)
                    else box value
                | _ ->
                    match BigInteger.TryParse(body, NumberStyles.None, CultureInfo.InvariantCulture) with
                    | true, value -> applySign signing value |> boxInteger
                    | _ -> invalid "the integer literal has invalid digits."

    member this.ReadString() : string =
        let start = position
        this.Advance()
        let builder = StringBuilder()
        let mutable finished = false
        while not finished do
            if this.Eof then
                this.Fail(
                    ReaderDiagnostics.UnexpectedEof,
                    "Unexpected end of input while reading a string.",
                    this.Span(start, position - start)
                )
            let ch = text.[position]
            if ch = '"' then
                this.Advance()
                finished <- true
            elif ch = '\\' then
                let escapeStart = position
                this.Advance()
                if this.Eof then
                    this.Fail(
                        ReaderDiagnostics.UnexpectedEof,
                        "Unexpected end of input in a string escape.",
                        this.Span(escapeStart, 1)
                    )
                let escaped = text.[position]
                this.Advance()
                match escaped with
                | 't' -> builder.Append '\t' |> ignore
                | 'r' -> builder.Append '\r' |> ignore
                | 'n' -> builder.Append '\n' |> ignore
                | 'b' -> builder.Append '\b' |> ignore
                | 'f' -> builder.Append '\f' |> ignore
                | '\\' -> builder.Append '\\' |> ignore
                | '"' -> builder.Append '"' |> ignore
                | '\'' -> builder.Append '\'' |> ignore
                | 'u' ->
                    let mutable value = 0
                    for _ in 1 .. 4 do
                        if this.Eof then
                            this.Fail(ReaderDiagnostics.UnexpectedEof, "Unicode escape is incomplete.", this.Span(escapeStart, position - escapeStart))
                        let digit = Tokens.digitValue text.[position]
                        if digit < 0 || digit >= 16 then
                            this.Fail(ReaderDiagnostics.InvalidEscape, "Unicode escape needs four hexadecimal digits.", this.Span(escapeStart, position - escapeStart + 1))
                        value <- value * 16 + digit
                        this.Advance()
                    builder.Append(char value) |> ignore
                | 'o' ->
                    let mutable value = 0
                    let mutable count = 0
                    while count < 3 && not this.Eof && text.[position] >= '0' && text.[position] <= '7' do
                        value <- value * 8 + int text.[position] - int '0'
                        this.Advance()
                        count <- count + 1
                    if count = 0 then
                        this.Fail(ReaderDiagnostics.InvalidEscape, "Octal escape needs at least one octal digit.", this.Span(escapeStart, position - escapeStart))
                    if value > 255 then
                        this.Fail(ReaderDiagnostics.InvalidEscape, "Octal escape must be in the range 0-377.", this.Span(escapeStart, position - escapeStart))
                    builder.Append(char value) |> ignore
                | other ->
                    this.Fail(
                        ReaderDiagnostics.InvalidEscape,
                        sprintf "Unsupported string escape '\\%c'." other,
                        this.Span(escapeStart, max 2 (position - escapeStart))
                    )
            else
                builder.Append ch |> ignore
                this.Advance()
        builder.ToString()

    member this.ReadCharacter() : obj =
        let start = position
        this.Advance()
        if this.Eof then
            this.Fail(ReaderDiagnostics.UnexpectedEof, "Unexpected end of input after a character marker.", this.Span(start, 1))
        let first = text.[position]
        if Tokens.isWhitespace first || Tokens.isClosing first || first = '"' || first = ';' || first = '\\' || first = '@' || first = '^' || first = '`' || first = '~' then
            this.Advance()
            box first
        else
            let token, _ = this.ReadToken()
            let span = this.Span(start, position - start)
            if token.Length = 1 then
                box token.[0]
            elif token.StartsWith("u", StringComparison.Ordinal) && token.Length = 5 then
                let mutable value = 0
                let mutable valid = true
                for ch in token.Substring 1 do
                    let digit = Tokens.digitValue ch
                    if digit < 0 || digit >= 16 then valid <- false
                    else value <- value * 16 + digit
                if valid then box (char value)
                else this.Fail(ReaderDiagnostics.InvalidCharacter, sprintf "Invalid unicode character literal '\\%s'." token, span)
            elif token.StartsWith("o", StringComparison.Ordinal) && token.Length >= 2 && token.Length <= 4 then
                let mutable value = 0
                let mutable valid = true
                for ch in token.Substring 1 do
                    if ch < '0' || ch > '7' then valid <- false
                    else value <- value * 8 + int ch - int '0'
                if valid then box (char value)
                else this.Fail(ReaderDiagnostics.InvalidCharacter, sprintf "Invalid octal character literal '\\%s'." token, span)
            else
                let named =
                    match token with
                    | "newline" -> Some '\n'
                    | "space" -> Some ' '
                    | "tab" -> Some '\t'
                    | "backspace" -> Some '\b'
                    | "formfeed" -> Some '\f'
                    | "return" -> Some '\r'
                    | _ -> None
                match named with
                | Some value -> box value
                | None -> this.Fail(ReaderDiagnostics.InvalidCharacter, sprintf "Unsupported character literal '\\%s'." token, span)

    member this.ReadRegex(start: int) : obj =
        let builder = StringBuilder()
        let mutable finished = false
        while not finished do
            if this.Eof then
                this.Fail(ReaderDiagnostics.UnexpectedEof, "Unexpected end of input while reading a regex.", this.Span(start, position - start))
            let ch = text.[position]
            if ch = '"' then
                this.Advance()
                finished <- true
            elif ch = '\\' then
                builder.Append '\\' |> ignore
                this.Advance()
                if this.Eof then
                    this.Fail(ReaderDiagnostics.UnexpectedEof, "Unexpected end of input in a regex escape.", this.Span(start, position - start))
                builder.Append text.[position] |> ignore
                this.Advance()
            else
                builder.Append ch |> ignore
                this.Advance()
        let pattern = builder.ToString()
        try
            Regex pattern :> obj
        with error ->
            this.Fail(ReaderDiagnostics.InvalidRegex, sprintf "Invalid regex: %s" error.Message, this.Span(start, position - start))

    member this.ReadMetadata(start: int) : obj =
        let metadataForm = this.ReadRequired "metadata"
        let target = this.ReadPrefixTarget "metadata target"
        let metadata =
            match metadataForm with
            | :? Keyword as keyword -> PersistentMap.Empty.Assoc(keyword, box true)
            | :? Symbol as symbol -> PersistentMap.Empty.Assoc(Keyword.Intern "tag", symbol)
            | :? string as text -> PersistentMap.Empty.Assoc(Keyword.Intern "tag", text)
            | :? PersistentVector as vector -> PersistentMap.Empty.Assoc(Keyword.Intern "tag", vector)
            | :? PersistentMap as map -> map
            | _ ->
                this.Fail(
                    ReaderDiagnostics.InvalidMetadata,
                    "Metadata must be a keyword, symbol, string, vector, or map.",
                    this.Span(start, position - start)
                )

        let applyTo (target: obj) =
            let existing =
                match target with
                | :? IMeta as meta -> meta.Meta
                | _ -> None
            let merged = Meta.merge existing (Some metadata)
            match target with
            | :? IObj as obj -> obj.WithMeta merged :> obj
            | _ ->
                this.Fail(
                    ReaderDiagnostics.MetadataNotSupported,
                    "Metadata can only be applied to symbols, lists, vectors, maps, and sets.",
                    this.Span(start, position - start)
                )

        match target with
        | :? Splice as splice ->
            Splice(
                splice.Items
                |> List.map (fun item -> this.AttachSource(applyTo item, start, position))
            ) :> obj
        | _ -> applyTo target

    member this.ReadDispatch() : obj option =
        let start = position
        this.Advance()
        if this.Eof then
            this.Fail(ReaderDiagnostics.UnexpectedEof, "Unexpected end of input after '#'.", this.Span(start, 1))
        let ch = text.[position]
        match ch with
        | '{' ->
            this.Advance()
            let forms = this.ReadDelimited('}', start, "set")
            Some(this.BuildSet(forms, start) :> obj)
        | '(' ->
            this.Advance()
            this.ReadAnonymousFunction start
        | '_' ->
            this.Advance()
            match this.ReadNext() with
            | Some _ -> ()
            | None ->
                this.Fail(
                    ReaderDiagnostics.UnexpectedEof,
                    "Unexpected end of input after discard.",
                    this.Span(position, 0)
                )
            None
        | '\'' ->
            this.Advance()
            Some(this.WrapPrefix("var", start, this.ReadPrefixTarget "var quote"))
        | '"' ->
            this.Advance()
            Some(this.ReadRegex start)
        | '?' ->
            this.Advance()
            let splicing = this.TryPeek 0 = Some '@'
            if splicing then this.Advance()
            Some(this.ReadReaderConditional(splicing, start))
        | ':' ->
            this.Advance()
            Some(this.ReadNamespacedMap start)
        | '^' ->
            this.Advance()
            Some(this.ReadMetadata start)
        | '#' ->
            this.Advance()
            Some(this.ReadSymbolicValue start)
        | '!' ->
            this.SkipLine()
            None
        | '=' ->
            this.Fail(
                ReaderDiagnostics.ReaderEvalUnsupported,
                "Reader evaluation (#=) is deliberately unsupported.",
                this.Span(start, 1)
            )
        | other when Tokens.isWhitespace other ->
            this.Fail(ReaderDiagnostics.UnknownDispatch, "No dispatch macro follows '#'.", this.Span(start, 1))
        | _ ->
            Some(this.ReadTaggedLiteral start)

    member this.ReadAnonymousFunction(start: int) : obj option =
        if anonFns.Count > 0 then
            this.Fail(
                ReaderDiagnostics.AnonymousFnError,
                "Nested #() forms are not allowed.",
                this.Span(start, position - start)
            )
        let state = AnonFnState(nextAnonId)
        nextAnonId <- nextAnonId + 1
        anonFns.Add state
        try
            let forms = this.ReadDelimited(')', start, "anonymous function")
            let parameters = ResizeArray<obj>()
            for index in 1 .. state.MaxArgument do
                parameters.Add(state.Parameter index)
            if state.HasRest then
                parameters.Add(Symbol.Create "&")
                parameters.Add(state.Rest)
            let parameterVector = PersistentVector.OfSeq parameters
            let body = PersistentList.OfSeq forms
            let form = PersistentList.OfSeq [ Symbol.Create "fn*"; parameterVector :> obj; body :> obj ]
            Some(form :> obj)
        finally
            anonFns.RemoveAt(anonFns.Count - 1)

    member this.ReadSymbolicValue(start: int) : obj =
        let token, _ = this.ReadToken()
        match token with
        | "Inf" -> box Double.PositiveInfinity
        | "-Inf" -> box Double.NegativeInfinity
        | "NaN" -> box Double.NaN
        | _ ->
            this.Fail(
                ReaderDiagnostics.InvalidToken,
                sprintf "Unknown symbolic value ##%s." token,
                this.Span(start, position - start)
            )

    member this.ReadTaggedLiteral(start: int) : obj =
        let token, _ = this.ReadToken()
        if String.IsNullOrEmpty token then
            this.Fail(ReaderDiagnostics.InvalidTaggedLiteral, "A tagged literal needs a tag.", this.Span(start, position - start))
        let tag = Symbol.Parse token
        let form = this.ReadPrefixTarget (sprintf "tag #%s" token)
        let convert (form: obj) : obj =
            match token with
            | "inst" ->
                match form with
                | :? string as text ->
                    match DateTimeOffset.TryParse(text, CultureInfo.InvariantCulture, DateTimeStyles.RoundtripKind) with
                    | true, value -> box value
                    | _ ->
                        this.Fail(
                            ReaderDiagnostics.InvalidTaggedLiteral,
                            sprintf "Invalid #inst value '%s'." text,
                            this.Span(start, position - start)
                        )
                | _ ->
                    this.Fail(
                        ReaderDiagnostics.InvalidTaggedLiteral,
                        "#inst expects a string.",
                        this.Span(start, position - start)
                    )
            | "uuid" ->
                match form with
                | :? string as text ->
                    match Guid.TryParse text with
                    | true, value -> box value
                    | _ ->
                        this.Fail(
                            ReaderDiagnostics.InvalidTaggedLiteral,
                            sprintf "Invalid #uuid value '%s'." text,
                            this.Span(start, position - start)
                        )
                | _ ->
                    this.Fail(
                        ReaderDiagnostics.InvalidTaggedLiteral,
                        "#uuid expects a string.",
                        this.Span(start, position - start)
                    )
            | _ -> TaggedLiteral(tag, form) :> obj

        match form with
        | :? Splice as splice -> Splice(splice.Items |> List.map convert) :> obj
        | _ -> convert form

    member this.ReadNamespacedMap(start: int) : obj =
        let autoResolved = this.TryPeek 0 = Some ':'
        if autoResolved then this.Advance()
        let token, _ = this.ReadToken()
        this.SkipWhitespace()
        if this.TryPeek 0 <> Some '{' then
            this.Fail(
                ReaderDiagnostics.NamespacedMapError,
                "A namespaced map must be followed by a map literal.",
                this.Span(start, position - start)
            )
        this.Advance()
        let namespaceName =
            if autoResolved then
                if String.IsNullOrEmpty token then
                    if String.IsNullOrEmpty options.CurrentNamespace then
                        this.Fail(
                            ReaderDiagnostics.NamespacedMapError,
                            "An auto-resolved namespaced map needs a current namespace.",
                            this.Span(start, position - start)
                        )
                    options.CurrentNamespace
                else
                    match options.ResolveAlias token with
                    | Some target -> target
                    | None ->
                        this.Fail(
                            ReaderDiagnostics.UnknownAlias,
                            sprintf "Unknown namespace alias '%s'." token,
                            this.Span(start, position - start)
                        )
            else
                if String.IsNullOrEmpty token then
                    this.Fail(
                        ReaderDiagnostics.NamespacedMapError,
                        "A namespaced map needs a namespace.",
                        this.Span(start, position - start)
                    )
                token
        let qualify (value: obj) =
            match value with
            | :? Keyword as keyword when keyword.Namespace.IsNone -> Keyword.Intern(namespaceName, keyword.Name) :> obj
            | :? Symbol as symbol when symbol.Namespace.IsNone -> Symbol.Create(namespaceName, symbol.Name) :> obj
            | _ -> value
        this.ReadMapBody('}', start, qualify) :> obj

    member this.ReadAll() =
        let forms = ResizeArray<obj>()
        let mutable running = true
        while running do
            this.SkipWhitespace()
            if this.Eof then
                running <- false
            else
                let start = position
                try
                    match this.ReadNext() with
                    | Some (:? Splice as splice) -> forms.AddRange splice.Items
                    | Some form -> forms.Add form
                    | None -> ()
                with ReadException diagnostic ->
                    diagnostics.Add diagnostic
                    if position <= start then position <- min text.Length (start + 1)
                    running <- false
        { Forms = forms |> Seq.toList
          Diagnostics = diagnostics |> Seq.toList
          Position = position }

    member this.ReadForms() =
        this.SkipWhitespace()
        if this.Eof then
            { Forms = []
              Diagnostics = []
              Position = position }
        else
            let start = position
            try
                let forms = ResizeArray<obj>()
                // Discards and shebangs consume input without producing a form;
                // keep reading until this step yields forms or reaches EOF.
                while forms.Count = 0 && not this.Eof do
                    match this.ReadNext() with
                    | Some (:? Splice as splice) -> forms.AddRange splice.Items
                    | Some form -> forms.Add form
                    | None -> ()
                { Forms = forms |> Seq.toList
                  Diagnostics = diagnostics |> Seq.toList
                  Position = position }
            with ReadException diagnostic ->
                diagnostics.Add diagnostic
                if position <= start then position <- min text.Length (start + 1)
                { Forms = []
                  Diagnostics = diagnostics |> Seq.toList
                  Position = position }

    member this.ReadOne() =
        let result = this.ReadForms()
        { Form =
            match result.Forms with
            | [ form ] -> Some form
            | _ -> None
          Forms = result.Forms
          Diagnostics = result.Diagnostics
          Position = result.Position }

module Reader =
    let readAll (source: SourceText) (options: ReaderOptions) =
        ReaderState(source, options, 0).ReadAll()

    let readOne (source: SourceText) (options: ReaderOptions) =
        ReaderState(source, options, 0).ReadOne()

    let readOneFrom (source: SourceText) (options: ReaderOptions) (offset: int) =
        if offset < 0 || offset > source.Length then
            invalidArg (nameof offset) "Reader offset is outside the source text."
        ReaderState(source, options, offset).ReadOne()

    let readFormsFrom (source: SourceText) (options: ReaderOptions) (offset: int) =
        if offset < 0 || offset > source.Length then
            invalidArg (nameof offset) "Reader offset is outside the source text."
        ReaderState(source, options, offset).ReadForms()
