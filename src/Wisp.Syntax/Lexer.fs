namespace Wisp.Syntax

open System
open System.Text
open Wisp.Core

module internal Lexer =
    type Token =
        { Kind: SyntaxKind
          Text: string
          Start: int }

    let private isWhitespace (ch: char) = Char.IsWhiteSpace ch

    let isAtomTerminator (ch: char) =
        Char.IsWhiteSpace ch
        || ch = ','
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

    let private looksNumeric (token: string) =
        if String.IsNullOrEmpty token then false
        elif Char.IsDigit token.[0] then true
        elif (token.[0] = '+' || token.[0] = '-') && token.Length > 1 && Char.IsDigit token.[1] then true
        elif token.[0] = '.' && token.Length > 1 && Char.IsDigit token.[1] then true
        else false

    /// Lexes [start, finish) into a flat token stream. The caller must choose
    /// boundaries at top-level element edges so a token is never cut in half.
    let lexRange (source: SourceText) (start: int) (finish: int) =
        let text = source.Text
        let tokens = ResizeArray<Token>()
        let diagnostics = ResizeArray<Diagnostic>()
        let mutable position = start

        let has offset = position + offset < finish

        let current offset =
            if has offset then text.[position + offset] else '\000'

        let add kind tokenStart tokenFinish =
            tokens.Add(
                { Kind = kind
                  Text = text.Substring(tokenStart, tokenFinish - tokenStart)
                  Start = tokenStart }
            )

        let addDiagnostic code message diagnosticStart diagnosticFinish =
            diagnostics.Add(
                Diagnostic.create
                    code
                    DiagnosticSeverity.Error
                    message
                    (TextSpan.Create(diagnosticStart, max 1 (diagnosticFinish - diagnosticStart)))
            )

        let scanComment () =
            let tokenStart = position
            while position < finish && text.[position] <> '\n' do
                position <- position + 1
            add SyntaxKind.Comment tokenStart position

        let scanShebang () =
            let tokenStart = position
            while position < finish && text.[position] <> '\n' do
                position <- position + 1
            add SyntaxKind.Shebang tokenStart position

        let scanString () =
            let tokenStart = position
            position <- position + 1
            let mutable finished = false
            while not finished && position < finish do
                let ch = text.[position]
                if ch = '\\' then
                    position <- position + 1
                    if position < finish then position <- position + 1
                elif ch = '"' then
                    position <- position + 1
                    finished <- true
                else
                    position <- position + 1
            if not finished then
                addDiagnostic
                    SyntaxDiagnostics.UnterminatedString
                    "Unterminated string literal."
                    tokenStart
                    position
            add SyntaxKind.String tokenStart position

        let scanRegex () =
            let tokenStart = position
            position <- position + 2
            let mutable finished = false
            while not finished && position < finish do
                let ch = text.[position]
                if ch = '\\' then
                    position <- position + 1
                    if position < finish then position <- position + 1
                elif ch = '"' then
                    position <- position + 1
                    finished <- true
                else
                    position <- position + 1
            if not finished then
                addDiagnostic
                    SyntaxDiagnostics.UnterminatedRegex
                    "Unterminated regex literal."
                    tokenStart
                    position
            add SyntaxKind.Regex tokenStart position

        let scanCharacter () =
            let tokenStart = position
            position <- position + 1
            if position >= finish then
                add SyntaxKind.Character tokenStart position
            else
                let next = text.[position]
                let isStandalone =
                    isWhitespace next
                    || next = ','
                    || next = '"'
                    || next = ';'
                    || next = '('
                    || next = ')'
                    || next = '['
                    || next = ']'
                    || next = '{'
                    || next = '}'
                    || next = '\\'
                    || next = '@'
                    || next = '^'
                    || next = '`'
                    || next = '~'
                    || next = '#'
                if isStandalone then
                    position <- position + 1
                else
                    while position < finish && not (isAtomTerminator text.[position]) do
                        position <- position + 1
                add SyntaxKind.Character tokenStart position

        let scanAtom () =
            let tokenStart = position
            while position < finish && not (isAtomTerminator text.[position]) do
                position <- position + 1
            let tokenText = text.Substring(tokenStart, position - tokenStart)
            let kind =
                match tokenText with
                | "nil" -> SyntaxKind.Nil
                | "true" -> SyntaxKind.True
                | "false" -> SyntaxKind.False
                | _ when looksNumeric tokenText -> SyntaxKind.Number
                | _ -> SyntaxKind.Symbol
            add kind tokenStart position

        while position < finish do
            let ch = text.[position]
            if isWhitespace ch then
                let tokenStart = position
                while position < finish && isWhitespace text.[position] do
                    position <- position + 1
                add SyntaxKind.Whitespace tokenStart position
            elif ch = ',' then
                let tokenStart = position
                position <- position + 1
                add SyntaxKind.Comma tokenStart position
            elif ch = ';' then
                scanComment ()
            elif ch = '(' then
                add SyntaxKind.OpenParen position (position + 1)
                position <- position + 1
            elif ch = ')' then
                add SyntaxKind.CloseParen position (position + 1)
                position <- position + 1
            elif ch = '[' then
                add SyntaxKind.OpenBracket position (position + 1)
                position <- position + 1
            elif ch = ']' then
                add SyntaxKind.CloseBracket position (position + 1)
                position <- position + 1
            elif ch = '{' then
                add SyntaxKind.OpenBrace position (position + 1)
                position <- position + 1
            elif ch = '}' then
                add SyntaxKind.CloseBrace position (position + 1)
                position <- position + 1
            elif ch = '\'' then
                add SyntaxKind.Quote position (position + 1)
                position <- position + 1
            elif ch = '`' then
                add SyntaxKind.SyntaxQuote position (position + 1)
                position <- position + 1
            elif ch = '~' then
                if has 1 && text.[position + 1] = '@' then
                    add SyntaxKind.UnquoteSplicing position (position + 2)
                    position <- position + 2
                else
                    add SyntaxKind.Unquote position (position + 1)
                    position <- position + 1
            elif ch = '@' then
                add SyntaxKind.Deref position (position + 1)
                position <- position + 1
            elif ch = '^' then
                add SyntaxKind.Metadata position (position + 1)
                position <- position + 1
            elif ch = ':' then
                let tokenStart = position
                position <- position + 1
                if has 0 && text.[position] = ':' then position <- position + 1
                while position < finish && not (isAtomTerminator text.[position]) do
                    position <- position + 1
                add SyntaxKind.Keyword tokenStart position
            elif ch = '"' then
                scanString ()
            elif ch = '\\' then
                scanCharacter ()
            elif ch = '#' then
                if has 1 && text.[position + 1] = '!' then
                    scanShebang ()
                elif has 1 && text.[position + 1] = '{' then
                    add SyntaxKind.SetStart position (position + 2)
                    position <- position + 2
                elif has 1 && text.[position + 1] = '(' then
                    add SyntaxKind.AnonFnStart position (position + 2)
                    position <- position + 2
                elif has 1 && text.[position + 1] = '_' then
                    add SyntaxKind.Discard position (position + 2)
                    position <- position + 2
                elif has 1 && text.[position + 1] = '\'' then
                    add SyntaxKind.VarQuote position (position + 2)
                    position <- position + 2
                elif has 1 && text.[position + 1] = '"' then
                    scanRegex ()
                elif has 2 && text.[position + 1] = '?' && text.[position + 2] = '@' then
                    add SyntaxKind.ReaderConditionalSplicing position (position + 3)
                    position <- position + 3
                elif has 1 && text.[position + 1] = '?' then
                    add SyntaxKind.ReaderConditional position (position + 2)
                    position <- position + 2
                elif has 2 && text.[position + 1] = ':' && text.[position + 2] = ':' then
                    add SyntaxKind.HashDoubleColon position (position + 3)
                    position <- position + 3
                elif has 1 && text.[position + 1] = ':' then
                    add SyntaxKind.HashColon position (position + 2)
                    position <- position + 2
                elif has 1 && text.[position + 1] = '^' then
                    add SyntaxKind.HashCaret position (position + 2)
                    position <- position + 2
                elif has 1 && text.[position + 1] = '#' then
                    add SyntaxKind.HashHash position (position + 2)
                    position <- position + 2
                elif has 1 && text.[position + 1] = '=' then
                    add SyntaxKind.HashEquals position (position + 2)
                    position <- position + 2
                else
                    add SyntaxKind.Hash position (position + 1)
                    position <- position + 1
            else
                scanAtom ()

        tokens.ToArray(), diagnostics |> Seq.toList
