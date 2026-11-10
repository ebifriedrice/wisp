namespace Wisp.Compiler

open System
open System.Collections.Concurrent
open System.Reflection
open Wisp.Core

/// Union case metadata is registered during desugaring so match patterns can
/// validate arity and field names before the emitted case type exists.
module UnionRegistry =
    let private cases = ConcurrentDictionary<string, string list>(StringComparer.Ordinal)
    let private unionCases = ConcurrentDictionary<string, string list>(StringComparer.Ordinal)
    let private caseUnions = ConcurrentDictionary<string, string>(StringComparer.Ordinal)

    let register caseName fieldNames = cases.[caseName] <- fieldNames

    let registerUnion unionName caseNames =
        unionCases.[unionName] <- caseNames
        for caseName in caseNames do
            caseUnions.[caseName] <- unionName

    let tryFind caseName =
        match cases.TryGetValue caseName with
        | true, fields -> Some fields
        | false, _ -> None

    let casesOf unionName =
        match unionCases.TryGetValue unionName with
        | true, names -> Some names
        | false, _ -> None

    let unionOf caseName =
        match caseUnions.TryGetValue caseName with
        | true, unionName -> Some unionName
        | false, _ -> None

/// Documentation registered by definition forms and consumed when vars are
/// interned.
module DefinitionDocs =
    let private docs = ConcurrentDictionary<string, string>(StringComparer.Ordinal)

    let typeKey (name: string) = "type:" + name
    let methodKey (typeName: string) (methodName: string) = "method:" + typeName + "." + methodName
    let constructorKey (typeName: string) (arity: int) = sprintf "constructor:%s/%d" typeName arity

    let register key (doc: string) = docs.[key] <- doc

    let registerOption key doc =
        match doc with
        | Some text -> register key text
        | None -> docs.TryRemove key |> ignore

    let tryFind key =
        match docs.TryGetValue key with
        | true, doc -> Some doc
        | false, _ -> None

type CompilerError =
    { Code: string
      Message: string
      Span: TextSpan option
      Notes: string list }

module CompilerError =
    let create (code: string) (message: string) (span: TextSpan option) : CompilerError =
        { Code = code
          Message = message
          Span = span
          Notes = [] }

    let withNote note (error: CompilerError) = { error with Notes = error.Notes @ [ note ] }

type RecordField =
    { Name: string
      FieldType: Type
      /// Raw Hiccup hint for generic records, resolved once type parameters
      /// exist. Non-generic records resolve hints during desugaring.
      Hint: obj option }

type InterfaceParameter =
    { ParameterType: Type
      Attributes: ParameterAttributes }

type InterfaceMethod =
    { Name: string
      ReturnType: Type
      Parameters: InterfaceParameter list }

type InterfaceDefinition =
    { Name: string
      Methods: InterfaceMethod list }

type DelegateDefinition =
    { Name: string
      ReturnType: Type
      Parameters: InterfaceParameter list }

type AsyncResultKind =
    | AsyncTask
    | AsyncValueTask
    | AsyncValueTaskOf of Type

/// Core IR after special-form desugaring. It is deliberately small so later
/// passes (optimization, CLR lowering) can be added one at a time.
type CoreExpr =
    { Span: TextSpan option
      Node: CoreNode }

and CoreNode =
    | Const of obj
    | Local of string
    | VarRef of Symbol
    | If of CoreExpr * CoreExpr * CoreExpr
    | Do of CoreExpr list
    | Let of name: string * value: CoreExpr * body: CoreExpr
    | Lambda of name: string option * parameters: string list * rest: string option * body: CoreExpr
    | Call of callee: CoreExpr * arguments: CoreExpr list
    | Def of Symbol * CoreExpr
    | Quote of obj
    | SetNamespace of string * string option
    | ImportType of alias: string * fullName: string
    | RequireNamespace of alias: string * namespaceName: string
    | ReferVar of localName: string * namespaceName: string * varName: string
    | UseNamespace of namespaceName: string
    | UseNamespaceOnly of namespaceName: string * names: string list
    | StaticCall of clrType: Type * memberName: string * arguments: CoreExpr list
    | InstanceCall of target: CoreExpr * memberName: string * arguments: CoreExpr list
    | InstanceProperty of target: CoreExpr * memberName: string
    | Construct of clrType: Type * arguments: CoreExpr list
    /// A constructor whose vector type hint is resolved at evaluation time so a
    /// deftype earlier in the same top-level form can define the type.
    | ConstructHint of hintForm: obj * arguments: CoreExpr list
    | TypeTestHint of hintForm: obj * value: CoreExpr
    | DefineType of name: string * fields: RecordField list * definition: TypeDefinition option * isStruct: bool
    | DefineInterface of InterfaceDefinition
    | DefineDelegate of DelegateDefinition
    | CreateDelegate of delegateType: Type * callback: CoreExpr
    | SetVar of Symbol * value: CoreExpr
    | Async of returnKind: AsyncResultKind * body: CoreExpr
    | Await of value: CoreExpr
    | DefineMethod of typeName: string * methodName: string * value: CoreExpr * isPrivate: bool * namespaceName: string
    | DefineConstructor of typeName: string * value: CoreExpr * parameterHints: Type option list * isPrivate: bool * namespaceName: string
    | Loop of bindings: (string * CoreExpr) list * body: CoreExpr
    | Recur of arguments: CoreExpr list
    | Try of tryBody: CoreExpr * catches: CatchClause list * finallyBody: CoreExpr option
    | Throw of CoreExpr

and CatchClause =
    { ExceptionType: Type option
      Binding: string
      Body: CoreExpr }

module Core =
    let make span node = { Span = span; Node = node }
    let nil span = make span (Const null)

module Truth =
    let isTruthy (value: obj) =
        match value with
        | null -> false
        | :? bool as flag -> flag
        | _ -> true

type ExecutionBackend =
    | Interpreter
    | ReflectionEmit

type Pass<'Input, 'Output> =
    { Name: string
      Run: 'Input -> Result<'Output, CompilerError list> }

module Pass =
    let run pass input = pass.Run input

type EvaluationException(message: string, span: TextSpan option) =
    inherit Exception(message)

    member _.Span = span

type RecurSignal(values: obj[]) =
    inherit Exception("recur")

    member _.Values = values
