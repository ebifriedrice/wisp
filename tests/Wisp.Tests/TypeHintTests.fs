namespace Wisp.Tests

open System
open Xunit
open Wisp.Core
open Wisp.Tests.TestHelpers

type TypeHintTests() =
    let parseHint text =
        match TypeHints.parse (form text) with
        | Ok hint -> hint
        | Error errors -> failwithf "Type hint parse failed: %A" errors

    let resolveText text =
        match TypeHints.resolve (parseHint text) with
        | Ok resolved -> resolved
        | Error errors -> failwithf "Type hint resolution failed: %A" errors

    [<Fact>]
    member _.``resolves hiccup generic type hints``() =
        Assert.Equal(typeof<System.Collections.Generic.List<int>>, resolveText "[:List [:Int]]")
        Assert.Equal(
            typeof<System.Collections.Generic.List<System.Collections.Generic.List<int>>>,
            resolveText "[:List [:List :Int]]"
        )
        Assert.Equal(
            typeof<System.Collections.Generic.Dictionary<string, System.Collections.Generic.List<int>>>,
            resolveText "[:Dictionary :String [:List :Int]]"
        )
        Assert.Equal(
            typeof<System.Collections.Generic.Dictionary<string, obj>>,
            resolveText "[:Dictionary {:arity 2} :String :Object]"
        )

    [<Fact>]
    member _.``resolves C#-shaped symbol type hints``() =
        Assert.Equal(
            typeof<System.Collections.Generic.List<System.Collections.Generic.List<System.IComparable>>>,
            resolveText "[List [List [IComparable]]]"
        )
        Assert.Equal(
            typeof<System.Collections.Generic.Dictionary<string, int>>,
            resolveText "[Dictionary [String Int]]"
        )
        Assert.Equal(
            typeof<System.Collections.Generic.List<System.IComparable<int>>>,
            resolveText "[List [IComparable Int]]"
        )
        Assert.Equal(typeof<System.IComparable>, resolveText "[IComparable]")
        Assert.Equal(typeof<int>.MakeByRefType(), resolveText "Int32&")
        Assert.Equal(typeof<int>.MakeByRefType(), resolveText "Int32&:readonly")
        Assert.Equal(typeof<int>.MakeByRefType(), resolveText "Int32&:out")
        Assert.Equal(typeof<string>.MakeByRefType(), resolveText "String&")
        Assert.Equal(
            typeof<System.Collections.Generic.List<int>>.MakeByRefType(),
            resolveText "[byref [List [Int]]]"
        )
        Assert.Equal(
            typeof<System.Collections.Generic.Dictionary<string, int>>.MakeByRefType(),
            resolveText "[byref [Dictionary [String Int]]]"
        )
        Assert.Equal(typeof<int>.MakePointerType(), resolveText "Int32*")
        Assert.Equal(typeof<string>.MakePointerType(), resolveText "String*")

    [<Fact>]
    member _.``resolves arrays pointers and byrefs``() =
        Assert.Equal(typeof<int[]>, resolveText "[:array :int]")
        Assert.Equal(typeof<int[,]>, resolveText "[:array {:rank 2} :int]")
        Assert.Equal(typeof<int[][]>, resolveText "[:array [:array :int]]")
        Assert.Equal(typeof<int>.MakeByRefType(), resolveText "[:byref :int]")
        Assert.Equal(typeof<int>.MakePointerType(), resolveText "[:pointer :int]")

    [<Fact>]
    member _.``resolves dotted CLR names and ARITY fallback``() =
        Assert.Equal(
            typeof<System.Collections.Generic.List<int>>,
            resolveText "[:System.Collections.Generic.List [:System.Int32]]"
        )
        Assert.Equal(typeof<System.IDisposable>, resolveText ":System.IDisposable")

    [<Fact>]
    member _.``type hint errors point at the failing path``() =
        match TypeHints.parse (form "[:List {:arity 2} :Int]") with
        | Error errors ->
            Assert.NotEmpty(errors)
            Assert.Contains("arity", errors.Head.Message)
        | Ok _ -> failwith "Expected arity mismatch."
        match TypeHints.parse (form "[:List {:rank 2} :Int]") with
        | Error errors -> Assert.Contains("Unknown type hint attribute", errors.Head.Message)
        | Ok _ -> failwith "Expected unknown attribute error."
        match TypeHints.parse (form "[:No.Such.Type]") with
        | Ok hint ->
            match TypeHints.resolve hint with
            | Error errors -> Assert.Contains("Unknown CLR type", errors.Head.Message)
            | Ok _ -> failwith "Expected unknown type error."
        | Error errors -> failwithf "Unexpected parse failure: %A" errors
        match TypeHints.parse (form "[:param T]") with
        | Ok hint ->
            match TypeHints.resolve hint with
            | Error errors -> Assert.Contains("unbound", errors.Head.Message)
            | Ok _ -> failwith "Expected unbound type parameter."
        | Error errors -> failwithf "Unexpected parse failure: %A" errors

    [<Fact>]
    member _.``parses generic definitions with variance and constraints``() =
        let hint =
            parseHint
                "[:def {:name \"My.Box\" :arity 1} [:param {:name T :variance :out}] [:where T :class :new [:interface :System.IDisposable]] [:base :object] [:interface :System.IComparable]]"
        match hint with
        | TypeDefinitionHint definition ->
            Assert.Equal("My.Box", definition.Name)
            Assert.Equal(1, definition.Arity)
            let parameter = definition.TypeParameters.Head
            Assert.Equal("T", parameter.Name)
            Assert.Equal(Covariant, parameter.Variance)
            Assert.True(List.contains ReferenceTypeConstraint parameter.Constraints)
            Assert.True(List.contains DefaultConstructorConstraint parameter.Constraints)
            let interfaceType =
                parameter.Constraints
                |> List.pick (function
                    | InterfaceConstraint expression -> Some expression
                    | _ -> None)
            match interfaceType with
            | NamedType(name, None, []) -> Assert.Equal("System.IDisposable", name)
            | other -> failwithf "Unexpected interface constraint: %A" other
            match definition.BaseType with
            | Some(NamedType(name, None, [])) -> Assert.Equal("object", name)
            | other -> failwithf "Unexpected base type: %A" other
            Assert.Single(definition.Interfaces) |> ignore
        | TypeReference _ -> failwith "Expected a type definition."

    [<Fact>]
    member _.``type definition validation rejects duplicates unknown parameters and arity drift``() =
        let expectError text =
            match TypeHints.parse (form text) with
            | Error errors -> Assert.NotEmpty(errors)
            | Ok _ -> failwithf "Expected a parse error for %s" text
        expectError "[:def {:name \"X\"} [:param T] [:param T]]"
        expectError "[:def {:name \"X\"} [:where U :class]]"
        expectError "[:def {:name \"X\" :arity 2} [:param T]]"
        expectError "[:def {:name \"X\"} [:unknown]]"

    [<Fact>]
    member _.``metadata tags resolve through the hiccup parser``() =
        let metadata = PersistentMap.Empty.Assoc(Keyword.Intern "tag", form "[:List [:Int]]")
        match TypeHints.tryParseMetadata metadata with
        | Ok(Some(TypeReference _)) -> ()
        | other -> failwithf "Expected a type reference, got %A" other
        match TypeHints.tryResolveMetadata metadata with
        | Ok(Some resolved) -> Assert.Equal(typeof<System.Collections.Generic.List<int>>, resolved)
        | other -> failwithf "Expected a resolved type, got %A" other

    [<Fact>]
    member _.``definitions are declarations rather than runtime types``() =
        let hint = parseHint "[:def {:name \"X\"} [:param T]]"
        match TypeHints.resolve hint with
        | Error errors -> Assert.Contains("declaration", errors.Head.Message)
        | Ok _ -> failwith "Expected a declaration error."
