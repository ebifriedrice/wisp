namespace Wisp.Compiler

open System
open Wisp.Core

type MacroExpansion =
    { MacroName: string
      Invocation: obj
      Result: obj
      Span: TextSpan option }

type MacroExpansionResult =
    { Form: obj
      Expansions: MacroExpansion list }

module MacroExpander =
    let private macroKey = Keyword.Intern "macro"

    let private isMacro (variable: Var) =
        match variable.Meta with
        | Some metadata -> Object.Equals(metadata.ValAt macroKey, box true)
        | None -> false

    let private findMacro (runtime: RuntimeState) (currentNamespace: string) (symbol: Symbol) =
        let namespaceName = symbol.Namespace |> Option.defaultValue currentNamespace
        runtime.Registry.FindVar(namespaceName, symbol.Name) |> Option.filter isMacro

    let private attachInvocationSource (invocation: obj) (result: obj) : obj =
        match result with
        | :? IObj as target when (SourceMetadata.trySpan result).IsNone ->
            match invocation with
            | :? IMeta as source -> target.WithMeta(Meta.merge source.Meta target.Meta) :> obj
            | _ -> result
        | _ -> result

    let private preserveMetadata (original: obj) (rebuilt: obj) : obj =
        match original, rebuilt with
        | (:? IMeta as source), (:? IObj as target) ->
            target.WithMeta(Meta.merge source.Meta target.Meta) :> obj
        | _ -> rebuilt

    let private describeExpansions (expansions: MacroExpansion list) =
        expansions
        |> List.map (fun expansion ->
            sprintf "%s => %s" (Print.printString expansion.Invocation) (Print.printString expansion.Result))

    let expand (runtime: RuntimeState) (currentNamespace: string) (form: obj) : Result<MacroExpansionResult, CompilerError list> =
        let expansions = ResizeArray<MacroExpansion>()

        let rec loop depth namespaceName (form: obj) : Result<obj, CompilerError list> =
            if depth > 100 then
                Error
                    [ CompilerError.create
                          "WISP3102"
                          "Macro expansion exceeded 100 nested expansions."
                          (SourceMetadata.trySpan form) ]
            else
                let expandChildren (items: obj list) =
                    let rec collect acc remaining =
                        match remaining with
                        | [] -> Ok(List.rev acc)
                        | item :: rest ->
                            match loop (depth + 1) namespaceName item with
                            | Ok expanded -> collect (expanded :: acc) rest
                            | Error errors -> Error errors
                    collect [] items

                match form with
                | :? PersistentList as list ->
                    let items = list.Items |> Seq.toList
                    match items with
                    | head :: arguments ->
                        match head with
                        | :? Symbol as symbol when symbol.Namespace.IsNone && symbol.Name = "quote" -> Ok form
                        | :? Symbol as symbol ->
                            match findMacro runtime namespaceName symbol with
                            | Some variable ->
                                try
                                    let macroFunction = variable.Root :?> IFn
                                    let result =
                                        match macroFunction.Invoke(List.toArray arguments) with
                                        | :? PersistentList as list -> list :> obj
                                        | :? ISeq as sequence -> PersistentList.OfSeq(Seqs.toList (sequence :> obj)) :> obj
                                        | value -> value
                                    let result = attachInvocationSource form result
                                    expansions.Add(
                                        { MacroName = variable.FullName
                                          Invocation = form
                                          Result = result
                                          Span = SourceMetadata.trySpan form }
                                    )
                                    loop (depth + 1) namespaceName result
                                with
                                | :? EvaluationException as error ->
                                    let diagnostic =
                                        CompilerError.create
                                            "WISP3101"
                                            (sprintf "Macro %s failed: %s" variable.FullName error.Message)
                                            (SourceMetadata.trySpan form)
                                    let diagnostic =
                                        diagnostic
                                        |> CompilerError.withNote ("invocation: " + Print.printString form)
                                    let notes = describeExpansions (expansions |> Seq.toList)
                                    Error [ { diagnostic with Notes = diagnostic.Notes @ notes } ]
                                | error ->
                                    Error
                                        [ CompilerError.create
                                              "WISP3101"
                                              (sprintf "Macro %s failed: %s" variable.FullName error.Message)
                                              (SourceMetadata.trySpan form) ]
                            | None ->
                                match expandChildren items with
                                | Ok expandedItems -> Ok(preserveMetadata form (PersistentList.OfSeq expandedItems))
                                | Error errors -> Error errors
                        | _ ->
                            match expandChildren items with
                            | Ok expandedItems -> Ok(preserveMetadata form (PersistentList.OfSeq expandedItems))
                            | Error errors -> Error errors
                    | [] -> Ok form
                | :? ISeq as sequence ->
                    let asList = PersistentList.OfSeq(Seqs.toList (sequence :> obj)) :> obj
                    loop depth namespaceName (preserveMetadata form asList)
                | :? PersistentVector as vector ->
                    match expandChildren (vector.Elements |> Seq.toList) with
                    | Ok expanded -> Ok(preserveMetadata form (PersistentVector.OfSeq expanded))
                    | Error errors -> Error errors
                | :? PersistentMap as map ->
                    let entries = map.Entries |> Seq.toList
                    let forms = entries |> List.collect (fun entry -> [ entry.Key; entry.Value ])
                    match expandChildren forms with
                    | Ok expanded ->
                        let mutable rebuilt = PersistentMap.Empty
                        let rec add pairs =
                            match pairs with
                            | key :: value :: rest ->
                                rebuilt <- rebuilt.Assoc(key, value)
                                add rest
                            | _ -> ()
                        add expanded
                        Ok(preserveMetadata form (rebuilt :> obj))
                    | Error errors -> Error errors
                | :? PersistentSet as set ->
                    match expandChildren (set.Elements |> Seq.toList) with
                    | Ok expanded -> Ok(preserveMetadata form (PersistentSet.OfSeq expanded))
                    | Error errors -> Error errors
                | _ -> Ok form

        match loop 0 currentNamespace form with
        | Ok expanded ->
            Ok
                { Form = expanded
                  Expansions = expansions |> Seq.toList }
        | Error errors -> Error errors
