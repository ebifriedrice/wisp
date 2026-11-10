namespace Wisp.Syntax

open System
open Wisp.Core

/// Both tokens and internal nodes use one kind set. Trivia kinds are tokens the
/// parser preserves but does not treat as forms.
[<RequireQualifiedAccess>]
type SyntaxKind =
    | Whitespace
    | Comma
    | Comment
    | Shebang
    | OpenParen
    | CloseParen
    | OpenBracket
    | CloseBracket
    | OpenBrace
    | CloseBrace
    | Quote
    | SyntaxQuote
    | Unquote
    | UnquoteSplicing
    | Deref
    | Metadata
    | SetStart
    | AnonFnStart
    | Discard
    | VarQuote
    | Regex
    | HashColon
    | HashDoubleColon
    | ReaderConditional
    | ReaderConditionalSplicing
    | HashCaret
    | HashHash
    | HashEquals
    | Hash
    | Symbol
    | Keyword
    | Number
    | String
    | Character
    | Nil
    | True
    | False
    | SourceFile
    | List
    | Vector
    | Map
    | Set
    | QuoteNode
    | SyntaxQuoteNode
    | UnquoteNode
    | UnquoteSplicingNode
    | DerefNode
    | VarQuoteNode
    | MetadataNode
    | DiscardNode
    | AnonymousFnNode
    | NamespacedMapNode
    | TaggedLiteralNode
    | SymbolicValueNode
    | ReaderConditionalNode
    | ReaderEvalNode
    | ErrorNode

module SyntaxFacts =
    let isTrivia kind =
        match kind with
        | SyntaxKind.Whitespace
        | SyntaxKind.Comma
        | SyntaxKind.Comment
        | SyntaxKind.Shebang -> true
        | _ -> false

    let isClose kind =
        match kind with
        | SyntaxKind.CloseParen
        | SyntaxKind.CloseBracket
        | SyntaxKind.CloseBrace -> true
        | _ -> false

    let isLeaf kind =
        match kind with
        | SyntaxKind.Regex
        | SyntaxKind.Symbol
        | SyntaxKind.Keyword
        | SyntaxKind.Number
        | SyntaxKind.String
        | SyntaxKind.Character
        | SyntaxKind.Nil
        | SyntaxKind.True
        | SyntaxKind.False -> true
        | _ -> false

module SyntaxDiagnostics =
    [<Literal>]
    let UnterminatedString = "WISP2001"

    [<Literal>]
    let UnterminatedRegex = "WISP2002"

    [<Literal>]
    let MissingClose = "WISP2003"

    [<Literal>]
    let UnexpectedClose = "WISP2004"

    [<Literal>]
    let MissingForm = "WISP2005"

    [<Literal>]
    let OddMap = "WISP2006"

    [<Literal>]
    let MalformedDispatch = "WISP2007"

    [<Literal>]
    let UnexpectedToken = "WISP2008"

/// Green token: text plus kind, with no absolute position. Reuse across edits is
/// safe because positions are only materialized in the red tree.
type GreenToken =
    { Kind: SyntaxKind
      Text: string }

/// Green node: children plus total UTF-16 width.
type GreenElement =
    | Token of GreenToken
    | Node of GreenNode

and GreenNode =
    { Kind: SyntaxKind
      Children: GreenElement[]
      Width: int }

module Green =
    let elementWidth element =
        match element with
        | Token token -> token.Text.Length
        | Node node -> node.Width

    let makeNode kind (children: GreenElement[]) =
        { Kind = kind
          Children = children
          Width = children |> Array.sumBy elementWidth }

    let rec textOf (element: GreenElement) =
        match element with
        | Token token -> token.Text
        | Node node -> node.Children |> Array.map textOf |> String.concat ""
