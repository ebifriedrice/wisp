namespace Wisp.Tests

open System
open System.IO
open System.Numerics
open Xunit
open Wisp.Core
open Wisp.Compiler
open Wisp.Tests.TestHelpers

type IWispMarker = interface end

type IWispGreeter =
    abstract Greet: unit -> string

type IWispAdder =
    abstract Add: int * int -> int

type CustomRecordBase(typeName: string, fieldNames: string[], values: obj[]) =
    inherit WispRecordBase(typeName, fieldNames, values)

type WispEventSource() =
    let ping = Event<EventHandler, EventArgs>()

    [<CLIEvent>]
    member _.Ping = ping.Publish

    member _.RaisePing() = ping.Trigger(null, EventArgs.Empty)

type CompilerTests() =
    let session () = Session(output = new StringWriter())

    let output (session: Session) =
        match session.Runtime.Output with
        | :? StringWriter as writer -> writer.ToString()
        | _ -> ""

    let lastValue (session: Session) (text: string) =
        let result = session.EvalText text
        if not result.Diagnostics.IsEmpty then
            failwithf "Evaluation diagnostics: %A" (result.Diagnostics |> List.map (fun diagnostic -> diagnostic.Code, diagnostic.Message))
        match List.tryLast result.Values with
        | Some value -> value
        | None -> failwith "Expected at least one value."

    let expectInt (session: Session) (text: string) =
        Convert.ToInt64(lastValue session text, Globalization.CultureInfo.InvariantCulture)

    [<Fact>]
    member _.``arithmetic follows the numeric tower``() =
        let s = session ()
        Assert.Equal(6L, expectInt s "(+ 1 2 3)")
        Assert.Equal(5L, expectInt s "(- 10 3 2)")
        Assert.Equal(24L, expectInt s "(* 2 3 4)")
        Assert.Equal(2L, expectInt s "(/ 4 2)")
        Assert.Equal(3.5, unbox<double> (lastValue s "(+ 1.5 2)"))
        Assert.Equal(1.5m, unbox<decimal> (lastValue s "(+ 1M 0.5M)"))
        let ratio = lastValue s "(/ 1 2)" :?> Ratio
        Assert.Equal(BigInteger.One, ratio.Numerator)
        Assert.Equal(BigInteger(2), ratio.Denominator)
        Assert.Equal<obj>(box true, lastValue s "(< 1 2 3)")
        Assert.Equal<obj>(box false, lastValue s "(= 1 2)")

    [<Fact>]
    member _.``defs functions recursion closures and let work``() =
        let s = session ()
        Assert.Equal(10L, expectInt s "(def x 10) x")
        Assert.Equal(12L, expectInt s "(defn add [a b] (+ a b)) (add 5 7)")
        Assert.Equal(720L, expectInt s "(defn fact [n] (if (= n 0) 1 (* n (fact (- n 1))))) (fact 6)")
        Assert.Equal(12L, expectInt s "(defn adder [x] (fn [y] (+ x y))) ((adder 5) 7)")
        Assert.Equal<obj list>([ box 2L; box 5L; box 10L ], vectorItems (lastValue s "(let [x 2 y (+ x 3)] [x y (* x y)])"))
        Assert.Equal(120L, expectInt s "(def r (fn [n] (if (= n 0) 1 (* n (r (- n 1)))))) (r 5)")

    [<Fact>]
    member _.``conditionals when cond and boolean macros preserve semantics``() =
        let s = session ()
        Assert.Equal<obj>(box 2L, lastValue s "(if nil 1 2)")
        Assert.Equal<obj>(box 1L, lastValue s "(if 0 1 2)")
        Assert.Equal<obj>(box 3L, lastValue s "(when true 1 2 3)")
        Assert.Null(lastValue s "(when false 1 2)")
        Assert.Equal<obj>(box 2L, lastValue s "(cond false 1 (= 2 2) 2 :else 3)")
        Assert.Equal<obj>(box 3L, lastValue s "(and 1 2 3)")
        Assert.Null(lastValue s "(and 1 nil 3)")
        Assert.Equal<obj>(box 4L, lastValue s "(or nil false 4)")

    [<Fact>]
    member _.``collection literals evaluate their elements and builtins operate on them``() =
        let s = session ()
        Assert.Equal<obj list>([ box 1L; box 5L ], vectorItems (lastValue s "[1 (+ 2 3)]"))
        let map = asMap (lastValue s "{:a (+ 1 1)}")
        Assert.Equal<obj>(box 2L, map.ValAt(key "a"))
        Assert.Equal(3L, expectInt s "(count [1 2 3])")
        Assert.Equal(1L, expectInt s "(get {:a 1} :a)")
        Assert.Equal(2L, expectInt s "(get {:a 1} :b 2)")
        Assert.Equal(3L, expectInt s "(count (conj [1 2] 3))")
        Assert.Equal<obj list>([ box 1L; box 2L ], Seqs.toList (lastValue s "(cons 1 (list 2))"))
        Assert.Equal<obj>(box 1L, lastValue s "(first [1 2 3])")
        Assert.Equal<obj list>([ box 2L; box 3L ], Seqs.toList (lastValue s "(rest [1 2 3])"))
        Assert.Equal(6L, expectInt s "(apply + [1 2 3])")
        Assert.Equal<obj list>([ box 2L; box 3L; box 4L ], Seqs.toList (lastValue s "(map inc [1 2 3])"))
        Assert.Equal(6L, expectInt s "(reduce + [1 2 3])")

    [<Fact>]
    member _.``quote prevents evaluation and anonymous functions desugar``() =
        let s = session ()
        let quoted = listItems (lastValue s "'(+ 1 2)")
        Assert.Equal("+", (asSymbol quoted.[0]).FullName)
        Assert.Equal(5L, expectInt s "(#(+ % 2) 3)")
        let variadic = lastValue s "(defn collect [a & rest] [a rest]) (collect 1 2 3)"
        let vector = vectorItems variadic
        Assert.Equal<obj>(box 1L, vector.[0])
        Assert.Equal<obj list>([ box 2L; box 3L ], Seqs.toList vector.[1])

    [<Fact>]
    member _.``namespaces switch at read and eval time``() =
        let s = session ()
        Assert.Equal(1L, expectInt s "(ns my.app) (def x 1) x")
        Assert.Equal(2L, expectInt s "(ns other.app) (def x 2) x")
        Assert.True((s.Runtime.Registry.FindVar("my.app", "x")).IsSome)
        Assert.True((s.Runtime.Registry.FindVar("other.app", "x")).IsSome)
        let keyword = lastValue s "(ns my.app) ::thing" :?> Keyword
        Assert.Equal("my.app/thing", keyword.FullName)

    [<Fact>]
    member _.``native CLR calls constructors methods properties and generic types``() =
        let s = session ()
        Assert.Equal(5L, expectInt s "(System.Math/Abs -5)")
        Assert.Equal(Math.PI, unbox<double> (lastValue s "(System.Math/PI)"))
        Assert.Equal<obj>(box "ab", lastValue s "(System.String/Concat \"a\" \"b\")")
        Assert.Equal(2L, expectInt s "(def sb (System.Text.StringBuilder. \"a\")) (.Append sb \"b\") (.-Length sb)")
        Assert.Equal<obj>(box "ab", lastValue s "(def sb2 (System.Text.StringBuilder. \"a\")) (.Append sb2 \"b\") (.ToString sb2)")
        Assert.Equal<obj>(box "x", lastValue s "(.ToString (new System.Text.StringBuilder \"x\"))")
        let list = lastValue s "(new [:List :Int])"
        Assert.Equal(typeof<System.Collections.Generic.List<int>>, list.GetType())
        Assert.Equal(0L, expectInt s "(.-Count (new [:List :Int]))")
        let hinted = lastValue s "(def ^[:List [:Int]] xs (new [:List :Int]))" :?> Var
        match hinted.Meta with
        | Some metadata ->
            match TypeHints.tryResolveMetadata metadata with
            | Ok(Some clrType) -> Assert.Equal(typeof<System.Collections.Generic.List<int>>, clrType)
            | other -> failwithf "Expected a resolved type hint, got %A" other
        | None -> failwith "Expected var metadata."

    [<Fact>]
    member _.``compiler and reader diagnostics have source spans``() =
        let s = session ()
        let missing = s.EvalText "(println missing)"
        Assert.NotEmpty(missing.Diagnostics)
        Assert.Equal("WISP3001", missing.Diagnostics.Head.Code)
        Assert.Equal(9, missing.Diagnostics.Head.Span.Start)
        Assert.Equal(7, missing.Diagnostics.Head.Span.Length)

        let malformed = s.CompileText "(if)"
        match malformed with
        | Error diagnostics ->
            Assert.Equal("WISP3004", diagnostics.Head.Code)
            Assert.Equal(0, diagnostics.Head.Span.Start)
        | Ok _ -> failwith "Expected an if arity error."

        let invalidType = s.CompileText "(new No.Such.Type)"
        match invalidType with
        | Error diagnostics -> Assert.Equal("WISP3013", diagnostics.Head.Code)
        | Ok _ -> failwith "Expected an unknown type error."

        let incomplete = s.EvalText "(+"
        Assert.NotEmpty(incomplete.Diagnostics)
        Assert.Equal("WISP1001", incomplete.Diagnostics.Head.Code)

    [<Fact>]
    member _.``optimizer folds constant conditionals and lets``() =
        let s = session ()
        let compile text =
            match s.CompileText text with
            | Ok expression -> expression
            | Error errors -> failwithf "Compile failed: %A" errors
        match (compile "(if true 1 2)").Node with
        | Const value -> Assert.Equal<obj>(box 1L, value)
        | other -> failwithf "Expected a constant after optimizing if, got %A" other
        match (compile "(if false 1 2)").Node with
        | Const value -> Assert.Equal<obj>(box 2L, value)
        | other -> failwithf "Expected a constant after optimizing if, got %A" other
        match (compile "(let [x 1] x)").Node with
        | Const value -> Assert.Equal<obj>(box 1L, value)
        | other -> failwithf "Expected a constant after optimizing let, got %A" other
        match (compile "(do 1)").Node with
        | Const value -> Assert.Equal<obj>(box 1L, value)
        | other -> failwithf "Expected a constant after optimizing do, got %A" other

    [<Fact>]
    member _.``macros expand with raw forms and report expansion traces``() =
        let s = session ()
        Assert.Equal(1L, expectInt s "(defmacro unless [test then else] (list 'if test else then)) (unless false 1 2)")
        Assert.Equal(2L, expectInt s "(unless true 1 2)")
        Assert.Single(s.LastMacroExpansions) |> ignore
        Assert.Equal("user/unless", s.LastMacroExpansions.Head.MacroName)
        Assert.Equal(6L, expectInt s "(defmacro all [& xs] (cons '+ xs)) (all 1 2 3)")
        let quoted = listItems (lastValue s "'(unless false 1 2)")
        Assert.Equal("unless", (asSymbol quoted.[0]).FullName)
        Assert.Empty(s.LastMacroExpansions)

        let broken = s.EvalText "(defmacro boom [] (throw \"macro boom\")) (boom)"
        Assert.NotEmpty(broken.Diagnostics)
        Assert.Equal("WISP3101", broken.Diagnostics.Head.Code)
        Assert.Contains("user/boom", broken.Diagnostics.Head.Message)
        Assert.True(broken.Diagnostics.Head.Notes |> List.exists (fun note -> note.Contains("invocation: (boom)")))

        let generated = s.EvalText "(defmacro bad [] '(missing-symbol)) (bad)"
        Assert.NotEmpty(generated.Diagnostics)
        Assert.Equal("WISP3001", generated.Diagnostics.Head.Code)
        Assert.True(generated.Diagnostics.Head.Notes |> List.exists (fun note -> note.Contains("macro user/bad")))

    [<Fact>]
    member _.``loop recur and try catch run on both execution backends``() =
        let run backend (text: string) =
            let writer = new StringWriter()
            let session = Session(output = writer, backend = backend)
            let result = session.EvalText text
            Assert.Empty(result.Diagnostics)
            List.last result.Values, writer.ToString()

        for backend in [ ExecutionBackend.Interpreter; ExecutionBackend.ReflectionEmit ] do
            let loopValue, _ = run backend "(loop [i 0 acc 0] (if (= i 10) acc (recur (inc i) (+ acc i))))"
            Assert.Equal(45L, Convert.ToInt64(loopValue, Globalization.CultureInfo.InvariantCulture))

            let nestedLoop, _ = run backend "(loop [i 0 total 0] (if (= i 3) total (recur (inc i) (+ total (loop [j 0 sum 0] (if (= j 3) sum (recur (inc j) (+ sum j))))))))"
            Assert.Equal(9L, Convert.ToInt64(nestedLoop, Globalization.CultureInfo.InvariantCulture))

            let caught, _ = run backend "(try (throw \"boom\") (catch e (.-Message e)))"
            Assert.Equal<obj>(box "boom", caught)

            let caughtWithFinally, outputText =
                run backend "(try (throw \"x\") (catch e (.-Message e)) (finally (println \"cleanup\")))"
            Assert.Equal<obj>(box "x", caughtWithFinally)
            Assert.Equal("cleanup" + Environment.NewLine, outputText)

            let normalWithFinally, normalOutput = run backend "(try 42 (finally (println \"done\")))"
            Assert.Equal<obj>(box 42L, normalWithFinally)
            Assert.Equal("done" + Environment.NewLine, normalOutput)

            let interopCaught, _ = run backend "(try (System.Int32/Parse \"nope\") (catch e 7))"
            Assert.Equal<obj>(box 7L, interopCaught)

        let compiler = session ()
        match compiler.CompileText "(recur 1)" with
        | Error diagnostics ->
            Assert.Equal("WISP3026", diagnostics.Head.Code)
            Assert.Equal(0, diagnostics.Head.Span.Start)
        | Ok _ -> failwith "Expected recur outside loop to fail at compile time."

        match compiler.CompileText "(loop [i 0] (+ 1 (recur (inc i))))" with
        | Error diagnostics -> Assert.Equal("WISP3026", diagnostics.Head.Code)
        | Ok _ -> failwith "Expected non-tail recur to fail at compile time."

    [<Fact>]
    member _.``typed and multiple catch clauses match CLR exception types on both backends``() =
        let run backend (text: string) =
            let writer = new StringWriter()
            let session = Session(output = writer, backend = backend)
            let result = session.EvalText text
            Assert.Empty(result.Diagnostics)
            List.last result.Values, writer.ToString()

        for backend in [ ExecutionBackend.Interpreter; ExecutionBackend.ReflectionEmit ] do
            let exact, _ =
                run
                    backend
                    "(try (throw (new System.InvalidOperationException \"io\")) (catch System.ArgumentException e :arg) (catch System.InvalidOperationException e (.-Message e)) (catch System.Exception e :other))"
            Assert.Equal<obj>(box "io", exact)

            let firstMatch, _ =
                run
                    backend
                    "(try (throw (new System.InvalidOperationException \"x\")) (catch System.Exception e :first) (catch System.InvalidOperationException e :second))"
            Assert.Equal<obj>(box (Keyword.Intern "first"), firstMatch)

            let untypedLast, _ =
                run
                    backend
                    "(try (throw (new System.ArgumentException \"arg\")) (catch System.InvalidOperationException e :io) (catch e (.-Message e)))"
            Assert.Equal<obj>(box "arg", untypedLast)

            let caughtWithFinally, cleanupOutput =
                run
                    backend
                    "(try (throw (new System.InvalidOperationException \"io\")) (catch System.ArgumentException e :no) (catch System.InvalidOperationException e (.-Message e)) (finally (println \"cleanup\")))"
            Assert.Equal<obj>(box "io", caughtWithFinally)
            Assert.Equal("cleanup" + Environment.NewLine, cleanupOutput)

            let interopTyped, _ =
                run
                    backend
                    "(try (System.Int32/Parse \"nope\") (catch System.FormatException e :format) (catch System.Exception e :other))"
            Assert.Equal<obj>(box (Keyword.Intern "format"), interopTyped)

            let divideTyped, _ =
                run backend "(try (/ 1 0) (catch System.DivideByZeroException e :zero) (catch System.Exception e :other))"
            Assert.Equal<obj>(box (Keyword.Intern "zero"), divideTyped)

            let nestedNoMatch, _ =
                run
                    backend
                    "(try (try (throw (new System.InvalidOperationException \"deep\")) (catch System.ArgumentException e :nope)) (catch System.InvalidOperationException e (.-Message e)))"
            Assert.Equal<obj>(box "deep", nestedNoMatch)

            let nonExceptionValue, _ = run backend "(try (throw 42) (catch Exception e (.-Message e)))"
            Assert.Equal<obj>(box "42", nonExceptionValue)

            let untypedSymbolBody, _ =
                run backend "(do (def fallback :fallback) (try (throw \"x\") (catch e fallback)))"
            Assert.Equal<obj>(box (Keyword.Intern "fallback"), untypedSymbolBody)

        let compiler = session ()

        let expectError code (text: string) =
            match compiler.CompileText text with
            | Error diagnostics ->
                Assert.Equal(code, diagnostics.Head.Code)
                Assert.True(diagnostics.Head.Span.Length > 0)
                diagnostics.Head
            | Ok _ -> failwithf "Expected %s for %s" code text

        let invalidType = expectError "WISP3030" "(try 1 (catch System.String e 2))"
        Assert.Equal(TextSpan.Create(14, 13), invalidType.Span)
        Assert.Contains("not an exception type", invalidType.Message)

        let unknownType = expectError "WISP3030" "(try 1 (catch NoSuchException e 2))"
        Assert.Equal(TextSpan.Create(14, 15), unknownType.Span)

        let untypedBeforeTyped =
            expectError "WISP3031" "(try 1 (catch e :untyped) (catch Exception e2 :typed))"
        Assert.Equal(26, untypedBeforeTyped.Span.Start)

        let catchAfterFinally = expectError "WISP3027" "(try 1 (finally :done) (catch Exception e 2))"
        Assert.Equal(23, catchAfterFinally.Span.Start)

    [<Fact>]
    member _.``destructuring works in let fn and loop on both backends``() =
        let run backend (text: string) =
            let session = Session(output = new StringWriter(), backend = backend)
            let result = session.EvalText text
            Assert.Empty(result.Diagnostics)
            List.last result.Values

        for backend in [ ExecutionBackend.Interpreter; ExecutionBackend.ReflectionEmit ] do
            let vectorLet = run backend "(let [[a b & rest] [1 2 3 4]] [a b rest])"
            let vectorValues = vectorItems vectorLet
            Assert.Equal<obj>(box 1L, vectorValues.[0])
            Assert.Equal<obj>(box 2L, vectorValues.[1])
            Assert.Equal<obj list>([ box 3L; box 4L ], Seqs.toList vectorValues.[2])

            let nestedLet = run backend "(let [[a [b c]] [1 [2 3]]] (+ a b c))"
            Assert.Equal(6L, Convert.ToInt64(nestedLet, Globalization.CultureInfo.InvariantCulture))

            let asRest = run backend "(let [[a & rest :as all] [1 2 3]] [a rest all])"
            let asValues = vectorItems asRest
            Assert.Equal<obj>(box 1L, asValues.[0])
            Assert.Equal<obj list>([ box 2L; box 3L ], Seqs.toList asValues.[1])
            Assert.Equal<obj list>([ box 1L; box 2L; box 3L ], vectorItems asValues.[2])

            let mapLet = run backend "(let [{:keys [a b] :or {b 9} :as m} {:a 1}] [a b m])"
            let mapValues = vectorItems mapLet
            Assert.Equal<obj>(box 1L, mapValues.[0])
            Assert.Equal<obj>(box 9L, mapValues.[1])
            Assert.Equal<obj>(box 1L, (asMap mapValues.[2]).ValAt(key "a"))

            let explicitMap = run backend "(let [{x :x} {:x 5}] x)"
            Assert.Equal(5L, Convert.ToInt64(explicitMap, Globalization.CultureInfo.InvariantCulture))

            let nestedMap = run backend "(let [{{x :x} :nested} {:nested {:x 4}}] x)"
            Assert.Equal(4L, Convert.ToInt64(nestedMap, Globalization.CultureInfo.InvariantCulture))

            let stringKeys = run backend "(let [{:strs [a]} {\"a\" 8}] a)"
            Assert.Equal(8L, Convert.ToInt64(stringKeys, Globalization.CultureInfo.InvariantCulture))

            let functionVector = run backend "(defn f [[a b]] (+ a b)) (f [1 2])"
            Assert.Equal(3L, Convert.ToInt64(functionVector, Globalization.CultureInfo.InvariantCulture))

            let functionMap = run backend "(defn f [{:keys [x]}] x) (f {:x 7})"
            Assert.Equal(7L, Convert.ToInt64(functionMap, Globalization.CultureInfo.InvariantCulture))

            let functionRest = run backend "(defn f [a & [b c]] (+ a b c)) (f 1 2 3)"
            Assert.Equal(6L, Convert.ToInt64(functionRest, Globalization.CultureInfo.InvariantCulture))

            let destructuredRecur = run backend "(defn f [[n acc]] (if (= n 0) acc (recur [(dec n) (+ acc n)]))) (f [5 0])"
            Assert.Equal(15L, Convert.ToInt64(destructuredRecur, Globalization.CultureInfo.InvariantCulture))

            let destructuredRestRecur = run backend "(defn f [a & [b c]] (if (= a 0) (+ b c) (recur (dec a) [b c]))) (f 2 10 20)"
            Assert.Equal(30L, Convert.ToInt64(destructuredRestRecur, Globalization.CultureInfo.InvariantCulture))

            let loopVector = run backend "(loop [[i acc] [0 0]] (if (= i 3) acc (recur [(inc i) (+ acc i)])))"
            Assert.Equal(3L, Convert.ToInt64(loopVector, Globalization.CultureInfo.InvariantCulture))

            let loopMap = run backend "(loop [{:keys [i acc]} {:i 0 :acc 0}] (if (= i 3) acc (recur {:i (inc i) :acc (+ acc i)})))"
            Assert.Equal(3L, Convert.ToInt64(loopMap, Globalization.CultureInfo.InvariantCulture))

            let loopSequential = run backend "(loop [[a b] [1 2] c (+ a b)] c)"
            Assert.Equal(3L, Convert.ToInt64(loopSequential, Globalization.CultureInfo.InvariantCulture))

        let compiler = session ()
        let expectPatternError text =
            match compiler.CompileText text with
            | Error diagnostics ->
                Assert.Equal("WISP3029", diagnostics.Head.Code)
                Assert.True(diagnostics.Head.Span.Length > 0)
            | Ok _ -> failwithf "Expected a destructuring error for %s" text

        expectPatternError "(let [[a &] [1]] a)"
        expectPatternError "(let [{:keys x} {}] x)"
        expectPatternError "(fn [[a & b c]] a)"
        expectPatternError "(loop [[a & b c] [1 2 3]] a)"

    [<Fact>]
    member _.``function-level recur rebinds parameters on both backends``() =
        let run backend (text: string) =
            let session = Session(output = new StringWriter(), backend = backend)
            let result = session.EvalText text
            Assert.Empty(result.Diagnostics)
            List.last result.Values

        for backend in [ ExecutionBackend.Interpreter; ExecutionBackend.ReflectionEmit ] do
            let fact = run backend "(defn fact [n acc] (if (= n 0) acc (recur (dec n) (* acc n)))) (fact 5 1)"
            Assert.Equal(120L, Convert.ToInt64(fact, Globalization.CultureInfo.InvariantCulture))

            let anonymous = run backend "((fn [n acc] (if (= n 0) acc (recur (dec n) (+ acc n)))) 5 0)"
            Assert.Equal(15L, Convert.ToInt64(anonymous, Globalization.CultureInfo.InvariantCulture))

            let variadic = run backend "(defn sum [acc & xs] (if (empty? xs) acc (recur (+ acc (first xs)) (rest xs)))) (sum 0 1 2 3 4)"
            Assert.Equal(10L, Convert.ToInt64(variadic, Globalization.CultureInfo.InvariantCulture))

            let deep = run backend "(defn count-down [n] (if (= n 0) :done (recur (dec n)))) (count-down 20000)"
            Assert.Equal<obj>(box (Keyword.Intern "done"), deep)

            let nested = run backend "(defn outer [n] ((fn [m] (if (= m 0) n (recur (dec m)))) 3)) (outer 9)"
            Assert.Equal<obj>(box 9L, nested)

        let compiler = session ()
        match compiler.CompileText "(defn bad [n] (+ 1 (recur n)))" with
        | Error diagnostics -> Assert.Equal("WISP3026", diagnostics.Head.Code)
        | Ok _ -> failwith "Expected non-tail function recur to fail at compile time."

        match compiler.CompileText "(defn bad [n] (recur n n))" with
        | Error diagnostics -> Assert.Equal("WISP3026", diagnostics.Head.Code)
        | Ok _ -> failwith "Expected wrong-arity recur to fail at compile time."

    [<Fact>]
    member _.``syntax quote macros and reader conditionals execute end to end``() =
        let s = session ()
        Assert.Equal(1L, expectInt s "(defmacro unless [test then else] `(if ~test ~else ~then)) (unless false 1 2)")
        Assert.Equal(10L, expectInt s "(defmacro twice [x] `(let [v# ~x] (+ v# v#))) (twice 5)")
        Assert.Equal(3L, expectInt s "(defmacro all [& forms] `(do ~@forms)) (all 1 2 3)")
        Assert.Equal<obj list>([ box 0L; box 1L; box 2L; box 3L ], vectorItems (lastValue s "[0 #?@(:wisp [1 2] :default [9]) 3]"))
        Assert.Equal(1L, expectInt s "#?(:wisp 1 :clr 2 :default 3)")
        Assert.Equal<obj list>([ box 1L; box 7L; box 3L ], vectorItems (lastValue s "(def x 7) `[1 ~x 3]"))
        let quotedMap = asMap (lastValue s "`{:a ~x}")
        Assert.Equal<obj>(box 7L, quotedMap.ValAt(key "a"))
        let result = s.EvalText "#?@(:wisp [1 2]) 3"
        Assert.Empty(result.Diagnostics)
        Assert.Equal<obj list>([ box 1L; box 2L; box 3L ], result.Values)

    [<Fact>]
    member _.``reader conditionals after prefixes execute end to end``() =
        let s = session ()
        let spliced = s.EvalText "'#?@(:wisp [1 2])"
        Assert.Empty spliced.Diagnostics
        Assert.Equal<obj list>([ box 1L; box 2L ], spliced.Values)

        Assert.Equal<obj list>(
            [ box 0L; box 1L; box 2L; box 3L ],
            vectorItems (lastValue s "[0 '#?@(:wisp [1 2]) 3]")
        )
        Assert.Equal<obj>(box (Keyword.Intern "a"), lastValue s "'#?(:wisp :a :default :b)")

        let discarded = s.EvalText "#_#?@(:wisp [1 2]) 3"
        Assert.Empty discarded.Diagnostics
        Assert.Equal<obj list>([ box 3L ], discarded.Values)

        let syntaxQuoted = s.EvalText "`#?@(:wisp [1 2])"
        Assert.Empty syntaxQuoted.Diagnostics
        Assert.Equal<obj list>([ box 1L; box 2L ], syntaxQuoted.Values)

        Assert.Equal(1L, expectInt s "(def ^:from-conditional #?(:wisp tagged :default other) 1) tagged")
        let variable = s.Runtime.Registry.FindVar("user", "tagged").Value
        match variable.Meta with
        | Some metadata ->
            Assert.Equal(Some(box true), metadata.TryGet(Keyword.Intern "from-conditional"))
        | None -> failwith "Expected metadata on the var defined through a reader conditional."

    [<Fact>]
    member _.``nested syntax quotes and map splicing execute end to end``() =
        let s = session ()
        let doubleQuoted = lastValue s "(def x 7) ``(a ~x)"
        Assert.Equal("(concat (list (quote a)) (list x))", Print.printString doubleQuoted)
        let doubleUnquoted = lastValue s "(def x 7) ``(a ~~x)"
        Assert.Equal("(concat (list (quote a)) (list 7))", Print.printString doubleUnquoted)

        let singleNested = lastValue s "(def c 3) `(a `(b ~c))"
        Assert.Equal("(a (concat (list (quote b)) (list c)))", Print.printString singleNested)

        let nestedSplice = lastValue s "(def xs [1 2]) ``(a ~@xs)"
        Assert.Equal("(concat (list (quote a)) xs)", Print.printString nestedSplice)

        let mapSpliced = asMap (lastValue s "`{1 ~@[2 3] 4 ~@[5 6]}")
        Assert.Equal<obj>(box 2L, mapSpliced.ValAt(box 1L))
        Assert.Equal<obj>(box 4L, mapSpliced.ValAt(box 3L))
        Assert.Equal<obj>(box 6L, mapSpliced.ValAt(box 5L))

        let orderedSplice =
            asMap (lastValue s "`{0 ~@[1] 2 ~@[3] 4 ~@[5] 6 ~@[7] 8 ~@[9]}")
        for key, value in [ 0L, 1L; 2L, 3L; 4L, 5L; 6L, 7L; 8L, 9L ] do
            Assert.Equal<obj>(box value, orderedSplice.ValAt(box key))

        let result =
            s.EvalText "(defmacro outer [x] `(defmacro inner [] `(println ~~x))) (outer \"nested hi\") (inner)"
        Assert.Empty(result.Diagnostics)
        Assert.Equal("nested hi" + Environment.NewLine, output s)

    [<Fact>]
    member _.``macros can be redefined for REPL hot reload``() =
        let s = session ()
        Assert.Equal(1L, expectInt s "(defmacro m [] 1) (m)")
        Assert.Equal(2L, expectInt s "(defmacro m [] 2) (m)")

    [<Fact>]
    member _.``lambda bodies are emitted CLR methods not interpreter closures``() =
        let assertEmittedClosure (value: obj) =
            Assert.IsAssignableFrom<AFn>(value) |> ignore
            let closureType = value.GetType()
            Assert.True(closureType.Assembly.IsDynamic)
            Assert.NotEqual(typeof<Closure>, closureType)
            let invoke = closureType.GetMethod("Invoke", [| typeof<obj[]> |])
            Assert.NotNull(invoke)
            let body = invoke.GetMethodBody()
            Assert.NotNull(body)
            Assert.NotEmpty(body.GetILAsByteArray())

        let s = session ()
        let functionValue = lastValue s "(fn [x] (+ x 1))"
        assertEmittedClosure functionValue
        let callable = functionValue :?> IFn
        Assert.Equal<obj>(box 4L, callable.Invoke([| box 3L |]))

        let captured = lastValue s "(let [a 10] (fn [x] (+ a x)))"
        assertEmittedClosure captured
        Assert.Equal<obj>(box 15L, (captured :?> IFn).Invoke([| box 5L |]))

        let nested = lastValue s "((fn [a] (fn [b] (+ a b))) 10)"
        assertEmittedClosure nested
        Assert.Equal<obj>(box 17L, (nested :?> IFn).Invoke([| box 7L |]))

        let rest = lastValue s "(fn [a & more] [a more])"
        assertEmittedClosure rest
        let restResult = vectorItems ((rest :?> IFn).Invoke([| box 1L; box 2L; box 3L |]))
        Assert.Equal<obj>(box 1L, restResult.[0])
        Assert.Equal<obj list>([ box 2L; box 3L ], Seqs.toList restResult.[1])

        lastValue s "(defn fact [n] (if (= n 0) 1 (* n (fact (- n 1)))))" |> ignore
        let factRoot = (s.Runtime.Registry.FindVar("user", "fact").Value).Root
        assertEmittedClosure factRoot
        Assert.Equal<obj>(box 120L, (factRoot :?> IFn).Invoke([| box 5L |]))

    [<Fact>]
    member _.``records generate real CLR types with separately defined methods``() =
        let s = session ()
        let generatedType = lastValue s "(deftype Point [x y])" :?> Type
        Assert.True(generatedType.IsClass)
        Assert.True(generatedType.Assembly.IsDynamic)
        let point = lastValue s "(def p (->Point 1 2)) p"
        Assert.IsAssignableFrom<IWispRecord>(point) |> ignore
        let record = point :?> IWispRecord
        Assert.Equal("Point", record.TypeName)
        Assert.Equal(1L, expectInt s "(.-x p)")
        Assert.Equal(2L, expectInt s "(.-y p)")
        Assert.NotNull(point.GetType().GetProperty("x"))
        Assert.Equal(3L, expectInt s "(defmethod Point :sum [this] (+ (.-x this) (.-y this))) (.sum p)")
        Assert.Equal(11L, expectInt s "(defmethod Point :sum [this] (+ 10 (.-x this))) (.sum p)")
        Assert.Equal<obj>(box true, lastValue s "(= (->Point 1 2) (->Point 1 2))")
        Assert.Equal("#Point{:x 1 :y 2}", Print.printString point)

    [<Fact>]
    member _.``typed record fields emit typed CLR properties and convert constructor arguments``() =
        let s = session ()
        let point = lastValue s "(deftype TypedPoint [^int x ^long y]) (def p (->TypedPoint 3 4)) p"
        let pointType = point.GetType()
        Assert.Equal(typeof<int>, pointType.GetProperty("x").PropertyType)
        Assert.Equal(typeof<int64>, pointType.GetProperty("y").PropertyType)
        Assert.NotNull(pointType.GetConstructor([| typeof<int>; typeof<int64> |]))
        Assert.Equal(3, unbox<int> (pointType.GetProperty("x").GetValue point))
        Assert.Equal(4L, unbox<int64> (pointType.GetProperty("y").GetValue point))
        Assert.Equal(3L, expectInt s "(.-x p)")
        Assert.Equal(4L, expectInt s "(.-y p)")

        let holder = lastValue s "(deftype Holder [^[:List :Int] items]) (def h (->Holder [1 2 3])) h"
        let holderType = holder.GetType()
        Assert.Equal(typeof<System.Collections.Generic.List<int>>, holderType.GetProperty("items").PropertyType)
        Assert.NotNull(holderType.GetConstructor([| typeof<System.Collections.Generic.List<int>> |]))
        let items = holderType.GetProperty("items").GetValue holder :?> System.Collections.Generic.List<int>
        Assert.Equal<int list>([ 1; 2; 3 ], items |> Seq.toList)
        Assert.Equal(3L, expectInt s "(.-Count (.-items h))")
        Assert.Equal(1L, expectInt s "(get (.-items h) 0)")

        let interpreter = Session(output = new StringWriter(), backend = ExecutionBackend.Interpreter)
        let interpretedPoint = lastValue interpreter "(deftype IPoint [^int x]) (def p (->IPoint 9)) p"
        Assert.Equal(typeof<int>, interpretedPoint.GetType().GetProperty("x").PropertyType)
        Assert.Equal(9, unbox<int> (interpretedPoint.GetType().GetProperty("x").GetValue interpretedPoint))

        let invalid = s.CompileText "(deftype Bad [^[:List {:arity 2} :Int] xs])"
        match invalid with
        | Error diagnostics ->
            Assert.Equal("WISP3028", diagnostics.Head.Code)
            Assert.True(diagnostics.Head.Span.Length > 0)
            Assert.True(diagnostics.Head.Notes |> List.exists (fun note -> note.Contains("Generic arity")))
        | Ok _ -> failwith "Expected an invalid field type hint to fail at compile time."

        let conversionFailure = s.EvalText "(deftype BadValue [^int value]) (->BadValue \"not an int\")"
        Assert.NotEmpty(conversionFailure.Diagnostics)
        Assert.Contains("Cannot convert field value", conversionFailure.Diagnostics.Head.Message)

    [<Fact>]
    member _.``dictionary-valued record fields convert Wisp maps on both backends``() =
        for backend in [ ExecutionBackend.Interpreter; ExecutionBackend.ReflectionEmit ] do
            let session = Session(output = new StringWriter(), backend = backend)
            let eval (text: string) =
                let result = session.EvalText text
                Assert.Empty(result.Diagnostics)
                List.last result.Values

            let holder =
                eval "(deftype DictHolder [^[:Dictionary :String :Int] data]) (def h (->DictHolder {\"a\" 1 \"b\" 2})) h"
            let holderType = holder.GetType()
            Assert.Equal(
                typeof<System.Collections.Generic.Dictionary<string, int>>,
                holderType.GetProperty("data").PropertyType
            )
            let data =
                holderType.GetProperty("data").GetValue holder
                :?> System.Collections.Generic.Dictionary<string, int>
            Assert.Equal(2, data.Count)
            Assert.Equal(1, data.["a"])
            Assert.Equal(2, data.["b"])
            Assert.Equal(2L, Convert.ToInt64(eval "(.-Count (.-data h))", Globalization.CultureInfo.InvariantCulture))
            Assert.Equal(1L, Convert.ToInt64(eval "(get (.-data h) \"a\")", Globalization.CultureInfo.InvariantCulture))

            let readOnly =
                eval "(deftype ReadOnlyDictHolder [^[:IReadOnlyDictionary :String :Int] data]) (def r (->ReadOnlyDictHolder {\"x\" 9})) r"
            let readOnlyType = readOnly.GetType()
            Assert.Equal(
                typeof<System.Collections.Generic.IReadOnlyDictionary<string, int>>,
                readOnlyType.GetProperty("data").PropertyType
            )
            let readOnlyData =
                readOnlyType.GetProperty("data").GetValue readOnly
                :?> System.Collections.Generic.IReadOnlyDictionary<string, int>
            Assert.Equal(9, readOnlyData.["x"])

            let conversionFailure =
                session.EvalText "(deftype BadDict [^[:Dictionary :String :Int] data]) (->BadDict {\"a\" \"nope\"})"
            Assert.NotEmpty(conversionFailure.Diagnostics)
            Assert.Contains("Cannot convert field data", conversionFailure.Diagnostics.Head.Message)

    [<Fact>]
    member _.``generic record definitions emit CLR generic types with typed fields and constraints``() =
        for backend in [ ExecutionBackend.Interpreter; ExecutionBackend.ReflectionEmit ] do
            let session = Session(output = new StringWriter(), backend = backend)
            let eval (text: string) =
                let result = session.EvalText text
                Assert.Empty(result.Diagnostics)
                List.last result.Values

            let genericDefinition =
                eval "(defclass [GenBox T] [^T value])"
                :?> Type
            Assert.True(genericDefinition.IsGenericTypeDefinition)
            Assert.Equal(1, genericDefinition.GetGenericArguments().Length)
            Assert.Equal("T", genericDefinition.GetGenericArguments().[0].Name)

            let boxed = eval "(def b (new [GenBox Int] 42)) b"
            let boxedType = boxed.GetType()
            Assert.True(boxedType.IsGenericType)
            Assert.Equal(typeof<int>, boxedType.GetGenericArguments().[0])
            Assert.Equal(typeof<int>, boxedType.GetProperty("value").PropertyType)
            Assert.Equal(42, unbox<int> (boxedType.GetProperty("value").GetValue boxed))
            Assert.Equal(42L, Convert.ToInt64(eval "(.-value b)", Globalization.CultureInfo.InvariantCulture))

            let named = eval "(def s (new [GenBox String] \"hello\")) s"
            Assert.Equal("hello", unbox<string> (named.GetType().GetProperty("value").GetValue named))

            let constrainedDefinition =
                eval "(defclass [GenRefBox T (where T :class)] [^T value])"
                :?> Type
            let parameter = constrainedDefinition.GetGenericArguments().[0]
            Assert.True(
                parameter.GenericParameterAttributes.HasFlag(
                    Reflection.GenericParameterAttributes.ReferenceTypeConstraint
                )
            )

            let constrained = eval "(def r (new [GenRefBox String] \"ok\")) r"
            Assert.Equal("ok", unbox<string> (constrained.GetType().GetProperty("value").GetValue constrained))

            let constraintFailure = session.EvalText "(new [GenRefBox Int] 1)"
            Assert.NotEmpty(constraintFailure.Diagnostics)
            Assert.Equal("WISP3013", constraintFailure.Diagnostics.Head.Code)
            Assert.True(
                constraintFailure.Diagnostics.Head.Notes
                |> List.exists (fun note -> note.Contains("violates the constraint"))
            )

            let listBox =
                eval "(defclass [GenListBox T] [^[List T] items]) (def lb (new [GenListBox Int] [1 2 3])) lb"
            Assert.Equal(
                typeof<System.Collections.Generic.List<int>>,
                listBox.GetType().GetProperty("items").PropertyType
            )
            Assert.Equal(3, (listBox.GetType().GetProperty("items").GetValue listBox :?> System.Collections.Generic.List<int>).Count)

            eval "(defclass GenWrapper [^[GenBox Int] boxed]) (def w (->GenWrapper (new [GenBox Int] 7))) w"
            |> ignore
            Assert.Equal(7L, Convert.ToInt64(eval "(.-value (.-boxed w))", Globalization.CultureInfo.InvariantCulture))

    [<Fact>]
    member _.``record hints resolve lazily within one top-level form on both backends``() =
        for backend in [ ExecutionBackend.Interpreter; ExecutionBackend.ReflectionEmit ] do
            let suffix =
                match backend with
                | ExecutionBackend.Interpreter -> "I"
                | ExecutionBackend.ReflectionEmit -> "R"
            let early = "Early" + suffix
            let wrapperName = "Wrapper" + suffix
            let later = "Later" + suffix
            let session = Session(output = new StringWriter(), backend = backend)
            let result =
                session.EvalText(
                    sprintf
                        "(do (deftype %s [x]) (def p (->%s 5)) (def n (new [%s] 11)) (deftype %s [^[:%s] item]) (def w (->%s p)) (defclass [%s T] [^T value]) (def l (new [%s Int] 9)) [(.-x (.-item w)) (.-value l) (.-x n)])"
                        early
                        early
                        early
                        wrapperName
                        early
                        wrapperName
                        later
                        later
                )
            Assert.Empty result.Diagnostics
            let values = vectorItems (List.last result.Values)
            Assert.Equal<obj>(box 5L, values.[0])
            Assert.Equal(9L, Convert.ToInt64(values.[1], Globalization.CultureInfo.InvariantCulture))
            Assert.Equal<obj>(box 11L, values.[2])

            let earlyType = TypeHints.tryResolveName early |> Option.get
            let wrapper = (session.Runtime.Registry.FindVar("user", "w").Value).Root
            Assert.Equal(earlyType, wrapper.GetType().GetProperty("item").PropertyType)
            let laterValue = (session.Runtime.Registry.FindVar("user", "l").Value).Root
            Assert.Equal(typeof<int>, laterValue.GetType().GetProperty("value").PropertyType)

            let unknown = session.EvalText "(new [:No.Such.Type] 1)"
            Assert.NotEmpty unknown.Diagnostics
            Assert.Equal("WISP3001", unknown.Diagnostics.Head.Code)
            Assert.True(unknown.Diagnostics.Head.Span.Length > 0)
            Assert.Contains("Unknown CLR type No.Such.Type", unknown.Diagnostics.Head.Message)

    [<Fact>]
    member _.``definition interfaces and base types emit on both backends``() =
        for backend in [ ExecutionBackend.Interpreter; ExecutionBackend.ReflectionEmit ] do
            let suffix =
                match backend with
                | ExecutionBackend.Interpreter -> "I"
                | ExecutionBackend.ReflectionEmit -> "R"
            let session = Session(output = new StringWriter(), backend = backend)
            let eval (text: string) =
                let result = session.EvalText text
                Assert.Empty(result.Diagnostics)
                List.last result.Values
            let root name = (session.Runtime.Registry.FindVar("user", name).Value).Root

            let marker = "Marked" + suffix
            eval (
                sprintf
                    "(defclass %s [x] (implements Wisp.Tests.IWispMarker)) (def m (->%s 1))"
                    marker
                    marker
            )
            |> ignore
            let marked = root "m"
            Assert.True(typeof<IWispMarker>.IsAssignableFrom(marked.GetType()))

            let greeter = "Greeter" + suffix
            eval (
                sprintf
                    "(defclass %s [name] (implements Wisp.Tests.IWispGreeter)) (defmethod Wisp.Tests.IWispGreeter.Greet [^%s this] (str \"hello \" (.-name this))) (def g (->%s \"wisp\"))"
                    greeter
                    greeter
                    greeter
            )
            |> ignore
            let greeted = root "g"
            Assert.True(typeof<IWispGreeter>.IsAssignableFrom(greeted.GetType()))
            Assert.Equal("hello wisp", (greeted :?> IWispGreeter).Greet())

            let adder = "Adder" + suffix
            eval (
                sprintf
                    "(defclass %s [n] (implements Wisp.Tests.IWispAdder)) (defmethod Wisp.Tests.IWispAdder.Add [^%s this a b] (+ (.-n this) a b)) (def a (->%s 10))"
                    adder
                    adder
                    adder
            )
            |> ignore
            let added = root "a"
            Assert.Equal(13, (added :?> IWispAdder).Add(1, 2))

            let based = "Based" + suffix
            let basedValue =
                eval (
                    sprintf
                        "(defclass %s [x] (inherits Wisp.Tests.CustomRecordBase)) (def b (->%s 7)) (.-x b)"
                        based
                        based
                )
            Assert.Equal(7L, Convert.ToInt64(basedValue, Globalization.CultureInfo.InvariantCulture))
            let basedRecord = root "b"
            Assert.IsAssignableFrom<CustomRecordBase>(basedRecord) |> ignore

        let compiler = session ()

        let expectDefinitionError code (text: string) =
            match compiler.CompileText text with
            | Error diagnostics ->
                Assert.Equal(code, diagnostics.Head.Code)
                Assert.True(diagnostics.Head.Span.Length > 0)
                diagnostics.Head
            | Ok _ -> failwithf "Expected %s for %s" code text

        let badBase =
            expectDefinitionError "WISP3032" "(defclass BadBase [x] (inherits System.String))"
        Assert.Contains("derive from WispRecordBase", badBase.Message)

        let badInterface =
            expectDefinitionError
                "WISP3033"
                "(defclass BadInterface [x] (implements System.String))"
        Assert.Contains("not an interface", badInterface.Message)

    [<Fact>]
    member _.``new definition forms use symbol hints and C#-shaped targets on both backends``() =
        for backend in [ ExecutionBackend.Interpreter; ExecutionBackend.ReflectionEmit ] do
            let suffix =
                match backend with
                | ExecutionBackend.Interpreter -> "I"
                | ExecutionBackend.ReflectionEmit -> "R"
            let iface = "ICalc" + suffix
            let calc = "Calc" + suffix
            let twice = "Twice" + suffix
            let pair = "Pair" + suffix
            let plain = "Plain" + suffix
            let gen = "GenBox" + suffix
            let constrained = "CB" + suffix
            let dict = "DictBox" + suffix
            let session = Session(output = new StringWriter(), backend = backend)
            let eval (text: string) =
                let result = session.EvalText text
                Assert.Empty(result.Diagnostics)
                List.last result.Values
            let root name = (session.Runtime.Registry.FindVar("user", name).Value).Root

            let values =
                vectorItems (
                    eval (
                sprintf
                    "(definterface %s (^int Add [this a b])) (defclass %s [n] (implements %s)) (defmethod %s.Add [^%s this a b] (+ (.-n this) a b)) (defmethod %s [^%s this] (* 2 (.-n this))) (defmethod %s.Triple [^%s this] (* 3 (.-n this))) (defmethod %s [n] {:n n}) (defmethod %s [a b] {:n (+ a b)}) (def c (%s 5)) (def c2 (%s 2 3)) (defstruct %s [^int a ^int b]) (def pair (%s {:a 3 :b 4})) (defclass %s [x y]) (def p (%s {:x 1 :y 2})) (defclass [%s T] [^T value]) (def g (new [%s Int] 7)) (defclass [%s T (where [IComparable T])] [^T value]) (def cb (new [%s Int] 8)) (defclass %s [^[Dictionary [String Int]] data]) (def db (->%s {\"a\" 1 \"b\" 2})) [(.-n c) (.Add c 1 2) (.%s c) (.Triple c) (.-n c2) (.-a pair) (.-b pair) (.-x p) (.-y p) (.-value g) (.-value cb) (.-Count (.-data db)) (get (.-data db) \"a\")]"
                    iface
                    calc
                    iface
                    calc
                    calc
                    twice
                    calc
                    calc
                    calc
                    calc
                    calc
                    calc
                    calc
                    pair
                    pair
                    plain
                    plain
                    gen
                    gen
                    constrained
                    constrained
                    dict
                    dict
                    twice
                    )
                )
            let expected = [ 5L; 8L; 10L; 15L; 5L; 3L; 4L; 1L; 2L; 7L; 8L; 2L; 1L ]
            for index in 0 .. expected.Length - 1 do
                Assert.Equal(
                    expected.[index],
                    Convert.ToInt64(values.[index], Globalization.CultureInfo.InvariantCulture)
                )

            let calcValue = root "c"
            Assert.Equal(5L, Convert.ToInt64((calcValue.GetType().GetProperty("n").GetValue calcValue), Globalization.CultureInfo.InvariantCulture))

            let interfaceType = TypeHints.tryResolveName iface |> Option.get
            Assert.True(interfaceType.IsInterface)
            let addMethod = interfaceType.GetMethod("Add")
            Assert.Equal(8, unbox<int> (addMethod.Invoke(calcValue, [| box 1; box 2 |])))

            Assert.False((TypeHints.tryResolveName calc |> Option.get).IsSealed)
            let pairType = TypeHints.tryResolveName pair |> Option.get
            Assert.True(pairType.IsSealed)
            Assert.True(pairType.IsValueType)
            Assert.Equal(typeof<ValueType>, pairType.BaseType)
            Assert.Equal("#" + pair + "{:a 3 :b 4}", Print.printString (root "pair"))
            Assert.Equal<obj>(box true, eval (sprintf "(= pair (%s {:a 3 :b 4}))" pair))
            eval (sprintf "(defmethod %s.Sum [^%s this] (+ (.-a this) (.-b this)))" pair pair) |> ignore
            Assert.Equal(7L, Convert.ToInt64(eval (sprintf "(.Sum pair)"), Globalization.CultureInfo.InvariantCulture))
            Assert.Equal(3, unbox<int> ((root "pair").GetType().GetProperty("a").GetValue(root "pair")))
            Assert.Equal(1L, Convert.ToInt64((root "p").GetType().GetProperty("x").GetValue(root "p"), Globalization.CultureInfo.InvariantCulture))
            Assert.Equal(typeof<int>, (root "g").GetType().GetProperty("value").PropertyType)
            Assert.Equal(7, unbox<int> ((root "g").GetType().GetProperty("value").GetValue(root "g")))
            Assert.Equal(8, unbox<int> ((root "cb").GetType().GetProperty("value").GetValue(root "cb")))
            Assert.Equal(5, Convert.ToInt32((root "c2").GetType().GetProperty("n").GetValue(root "c2")))
            Assert.Equal(
                typeof<System.Collections.Generic.Dictionary<string, int>>,
                (root "db").GetType().GetProperty("data").PropertyType
            )

            let badConstraint = session.EvalText (sprintf "(new [%s Object] nil)" constrained)
            Assert.NotEmpty badConstraint.Diagnostics
            Assert.Contains("constraint", badConstraint.Diagnostics.Head.Message + String.concat " " badConstraint.Diagnostics.Head.Notes)

    [<Fact>]
    member _.``private record methods and inheritance clauses work on both backends``() =
        for backend in [ ExecutionBackend.Interpreter; ExecutionBackend.ReflectionEmit ] do
            let suffix =
                match backend with
                | ExecutionBackend.Interpreter -> "I"
                | ExecutionBackend.ReflectionEmit -> "R"
            let secret = "Secret" + suffix
            let child = "Child" + suffix
            let session = Session(output = new StringWriter(), backend = backend)
            let eval (text: string) =
                let result = session.EvalText text
                Assert.Empty(result.Diagnostics)
                List.last result.Values

            eval (
                sprintf
                    "(ns alpha) (defclass %s [x]) (defmethod ^:private %s.Hidden [this] 1) (defmethod %s.Visible [this] 2) (ns beta) (def s (alpha/%s {:x 1})) s"
                    secret
                    secret
                    secret
                    secret
            )
            |> ignore

            Assert.Equal(2L, Convert.ToInt64(eval "(.Visible s)", Globalization.CultureInfo.InvariantCulture))
            let hidden = session.EvalText "(.Hidden s)"
            Assert.NotEmpty hidden.Diagnostics
            Assert.Contains("private", hidden.Diagnostics.Head.Message)

            eval (
                sprintf
                    "(defclass %s [y] (inherits Wisp.Tests.CustomRecordBase)) (def child (%s {:y 3})) child"
                    child
                    child
            )
            |> ignore
            let childValue = (session.Runtime.Registry.FindVar("beta", "child").Value).Root
            Assert.IsAssignableFrom<CustomRecordBase>(childValue) |> ignore
            Assert.Equal(3L, Convert.ToInt64((childValue.GetType().GetProperty("y").GetValue childValue), Globalization.CultureInfo.InvariantCulture))

            let compiler = Session(output = new StringWriter(), backend = backend)
            match compiler.CompileText "(defstruct BadStruct [x] (inherits Wisp.Tests.CustomRecordBase))" with
            | Error diagnostics -> Assert.Equal("WISP3023", diagnostics.Head.Code)
            | Ok _ -> failwith "Expected defstruct with inheritance to fail."

    [<Fact>]
    member _.``unions active patterns and match clauses run on both backends``() =
        for backend in [ ExecutionBackend.Interpreter; ExecutionBackend.ReflectionEmit ] do
            let suffix =
                match backend with
                | ExecutionBackend.Interpreter -> "I"
                | ExecutionBackend.ReflectionEmit -> "R"
            let unionName = "Shape" + suffix
            let circle = "Circle" + suffix
            let rect = "Rect" + suffix
            let empty = "Empty" + suffix
            let area = "area" + suffix
            let evenActive = "evenActive" + suffix
            let session = Session(output = new StringWriter(), backend = backend)
            let eval (text: string) =
                let result = session.EvalText text
                Assert.Empty(result.Diagnostics)
                List.last result.Values
            let root name = (session.Runtime.Registry.FindVar("user", name).Value).Root

            let values =
                vectorItems (
                    eval (
                        sprintf
                            "(defunion %s (%s [radius]) (%s [w h]) (%s)) (def c (%s {:radius 3})) (def r (%s {:w 4 :h 5})) (def e (%s)) (defn %s [shape] (match shape ((%s radius) (* radius radius)) ((%s w h) (* w h)) ((%s) 0))) (defactive %s [n] (when (= 0 (mod n 2)) n)) [(%s c) (%s r) (%s e) (match 2 (1 :one) (2 :two) (_ :other)) (match [1 2 3] ([a b] (+ a b)) ([a b c] (+ a b c)) (_ -1)) (match {:a 1 :b 2} ({:keys [a b]} (+ a b)) (_ -1)) (match c (^%s x (.-radius x)) (_ 0)) (match 5 (4 :four) (_ :other)) (match 5 ((or 1 5) :five) (_ :other)) (match 4 ((active %s x) (* x 2)) (_ -1)) (match 3 ((active %s x) (* x 2)) (_ -1)) (match c ((%s radius) :when (< radius 0) :negative) (_ :positive))]"
                            unionName
                            circle
                            rect
                            empty
                            circle
                            rect
                            empty
                            area
                            circle
                            rect
                            empty
                            evenActive
                            area
                            area
                            area
                            circle
                            evenActive
                            evenActive
                            circle
                    )
                )
            let expected = [ "9"; "20"; "0"; ":two"; "6"; "3"; "3"; ":other"; ":five"; "8"; "-1"; ":positive" ]
            for index in 0 .. expected.Length - 1 do
                Assert.Equal(expected.[index], Print.printString values.[index])

            let unionType = TypeHints.tryResolveName unionName |> Option.get
            Assert.True(unionType.IsInterface)
            Assert.True(unionType.IsAssignableFrom((root "c").GetType()))
            Assert.True(unionType.IsAssignableFrom((root "e").GetType()))

            let noMatch = session.EvalText "(match :x (1 :one))"
            Assert.NotEmpty noMatch.Diagnostics
            Assert.Contains("no matching pattern", noMatch.Diagnostics.Head.Message)

    [<Fact>]
    member _.``match exhaustiveness covers unions and booleans``() =
        let suffix = Guid.NewGuid().ToString("N").Substring(0, 8)
        let unionName = "ExhUnion" + suffix
        let caseA = "ExhA" + suffix
        let caseB = "ExhB" + suffix
        let session = Session(output = new StringWriter())
        let eval (text: string) =
            let result = session.EvalText text
            Assert.Empty(result.Diagnostics)
            result
        let failed (text: string) =
            let result = session.EvalText text
            Assert.NotEmpty(result.Diagnostics)
            result.Diagnostics.Head

        eval (sprintf "(defunion %s (%s [x]) (%s [x]))" unionName caseA caseB) |> ignore

        let unionError = failed (sprintf "(defn missingUnion [v] (match v ((%s x) x)))" caseA)
        Assert.Equal("WISP3044", unionError.Code)
        Assert.Contains(caseB, unionError.Notes |> String.concat " ")

        eval (
            sprintf
                "(defn completeUnion [v] (match v ((%s x) :a) ((%s y) :b)))"
                caseA
                caseB
        )
        |> ignore

        eval (sprintf "(defn typedUnion [v] (match v (^%s value :shape)))" unionName) |> ignore
        eval (sprintf "(defn typedCases [v] (match v (^%s a :a) (^%s b :b)))" caseA caseB) |> ignore
        let typedMissing = failed (sprintf "(defn typedMissing [v] (match v (^%s a :a)))" caseA)
        Assert.Equal("WISP3044", typedMissing.Code)
        eval "(defn catchAll [v] (match v (value :any)))" |> ignore

        let booleanError = failed "(defn missingBool [v] (match v (true :yes)))"
        Assert.Equal("WISP3044", booleanError.Code)
        Assert.Contains("false", booleanError.Notes |> String.concat " ")

        eval "(defn completeBool [v] (match v (true :yes) (false :no)))" |> ignore
        eval "(defn orBool [v] (match v ((or true false) :bool)))" |> ignore
        eval "(defn typedBool [v] (match v (^Boolean value :bool)))" |> ignore

        let guardedError = failed "(defn guardedBool [v] (match v (true :when false :yes)))"
        Assert.Equal("WISP3044", guardedError.Code)

    [<Fact>]
    member _.``namespaces support imports requires and in-ns on both backends``() =
        for backend in [ ExecutionBackend.Interpreter; ExecutionBackend.ReflectionEmit ] do
            let suffix =
                match backend with
                | ExecutionBackend.Interpreter -> "I"
                | ExecutionBackend.ReflectionEmit -> "R"
            let lib = "lib.core" + suffix
            let app = "app.core" + suffix
            let third = "third.core" + suffix
            let session = Session(output = new StringWriter(), backend = backend)
            let eval (text: string) =
                let result = session.EvalText text
                Assert.Empty(result.Diagnostics)
                List.last result.Values

            let imported =
                vectorItems (
                    eval (
                        sprintf
                            "(ns %s) (def answer 42) (ns %s (:import [System.Text StringBuilder]) (:require [%s :as lib])) (def sb (StringBuilder. \"a\")) (.Append sb \"b\") [(.ToString sb) lib/answer ::thing]"
                            lib
                            app
                            lib
                    )
                )
            Assert.Equal<obj>(box "ab", imported.[0])
            Assert.Equal<obj>(box 42L, imported.[1])
            Assert.Equal<obj>(box (Keyword.Intern(app, "thing")), imported.[2])

            let local = vectorItems (eval (sprintf "(in-ns '%s) (def x 7) [x ::local]" third))
            Assert.Equal<obj>(box 7L, local.[0])
            Assert.Equal<obj>(box (Keyword.Intern(third, "local")), local.[1])
            let appNamespace = session.Runtime.Registry.TryFind app |> Option.get
            Assert.Equal(Some "System.Text.StringBuilder", appNamespace.TryResolveImport "StringBuilder")
            Assert.True((TypeHints.tryResolveName "StringBuilder").IsNone)
            Assert.Equal(third, session.CurrentNamespace)

    [<Fact>]
    member _.``value type structs implement interfaces and dispatch methods on both backends``() =
        for backend in [ ExecutionBackend.Interpreter; ExecutionBackend.ReflectionEmit ] do
            let suffix =
                match backend with
                | ExecutionBackend.Interpreter -> "I"
                | ExecutionBackend.ReflectionEmit -> "R"
            let structName = "SortStruct" + suffix
            let session = Session(output = new StringWriter(), backend = backend)
            let eval (text: string) =
                let result = session.EvalText text
                Assert.Empty(result.Diagnostics)
                List.last result.Values

            let sorted =
                vectorItems (
                    eval (
                        sprintf
                            "(defstruct %s [x] (implements System.IComparable)) (defmethod System.IComparable.CompareTo [^%s this other] (- (.-x this) (.-x other))) (def arr (System.Array/CreateInstance (type (%s {:x 0})) 3)) (.SetValue arr (%s {:x 5}) 0) (.SetValue arr (%s {:x 2}) 1) (.SetValue arr (%s {:x 9}) 2) (System.Array/Sort arr) [(.-x (.GetValue arr 0)) (.-x (.GetValue arr 1)) (.-x (.GetValue arr 2))]"
                            structName
                            structName
                            structName
                            structName
                            structName
                            structName
                    )
                )
            Assert.Equal<obj list>([ box 2L; box 5L; box 9L ], sorted)
            Assert.True((TypeHints.tryResolveName structName |> Option.get).IsValueType)

    [<Fact>]
    member _.``imports are namespace scoped and refer use resolve vars on both backends``() =
        for backend in [ ExecutionBackend.Interpreter; ExecutionBackend.ReflectionEmit ] do
            let suffix =
                match backend with
                | ExecutionBackend.Interpreter -> "I"
                | ExecutionBackend.ReflectionEmit -> "R"
            let lib = "scope.lib" + suffix
            let app = "scope.app" + suffix
            let other = "scope.other" + suffix
            let used = "scope.use" + suffix
            let only = "scope.only" + suffix
            let session = Session(output = new StringWriter(), backend = backend)
            let eval (text: string) =
                let result = session.EvalText text
                Assert.Empty(result.Diagnostics)
                List.last result.Values

            let values =
                vectorItems (
                    eval (
                        sprintf
                            "(ns %s) (def pub 10) (def ^:private hidden 20) (defn add1 [x] (+ x 1)) (ns %s (:import [System.Text StringBuilder]) (:require [%s :refer [pub add1]])) (def sb (StringBuilder. \"x\")) [pub (add1 5) (.ToString sb)]"
                            lib
                            app
                            lib
                    )
                )
            Assert.Equal<obj>(box 10L, values.[0])
            Assert.Equal<obj>(box 6L, values.[1])
            Assert.Equal<obj>(box "x", values.[2])

            let leaked = session.EvalText (sprintf "(ns %s) (StringBuilder. \"y\")" other)
            Assert.NotEmpty leaked.Diagnostics
            Assert.Contains("Unknown CLR type StringBuilder", leaked.Diagnostics.Head.Message)

            let usedValues = vectorItems (eval (sprintf "(ns %s (:use %s)) [(+ pub 1) (add1 2)]" used lib))
            Assert.Equal<obj>(box 11L, usedValues.[0])
            Assert.Equal<obj>(box 3L, usedValues.[1])
            let privateLeak = session.EvalText "hidden"
            Assert.NotEmpty privateLeak.Diagnostics

            let onlyValues = vectorItems (eval (sprintf "(ns %s (:use [%s :only [pub]])) [(+ pub 2)]" only lib))
            Assert.Equal<obj>(box 12L, onlyValues.[0])
            let onlyMissing = session.EvalText "add1"
            Assert.NotEmpty onlyMissing.Diagnostics

    [<Fact>]
    member _.``interface in out and ref parameters emit byref signatures on both backends``() =
        for backend in [ ExecutionBackend.Interpreter; ExecutionBackend.ReflectionEmit ] do
            let suffix =
                match backend with
                | ExecutionBackend.Interpreter -> "I"
                | ExecutionBackend.ReflectionEmit -> "R"
            let iface = "IRefs" + suffix
            let counter = "Counter" + suffix
            let session = Session(output = new StringWriter(), backend = backend)
            let eval (text: string) =
                let result = session.EvalText text
                Assert.Empty(result.Diagnostics)
                List.last result.Values

            eval (
                sprintf
                    "(definterface %s (^int Bump [this ^int& value]) (^bool TryGet [this ^:out ^int& value]) (^int Peek [this ^:readonly ^int& value])) (defclass %s [n] (implements %s)) (defmethod %s.Bump [^%s this value] [(+ (.-n this) value) (+ value 1)]) (defmethod %s.TryGet [^%s this value] [true (.-n this)]) (defmethod %s.Peek [^%s this value] (+ (.-n this) value)) (def c (%s {:n 10}))"
                    iface
                    counter
                    iface
                    iface
                    counter
                    iface
                    counter
                    iface
                    counter
                    counter
            )
            |> ignore

            let instance = (session.Runtime.Registry.FindVar("user", "c").Value).Root
            let interfaceType = TypeHints.tryResolveName iface |> Option.get
            let bump = interfaceType.GetMethod("Bump")
            let bumpParameter = bump.GetParameters().[0]
            Assert.True(bumpParameter.ParameterType.IsByRef)
            Assert.False(bumpParameter.IsIn)
            Assert.False(bumpParameter.IsOut)
            let bumpArguments = [| box 5 |]
            Assert.Equal(15, unbox<int> (bump.Invoke(instance, bumpArguments)))
            Assert.Equal(6, unbox<int> bumpArguments.[0])

            let tryGet = interfaceType.GetMethod("TryGet")
            let tryGetParameter = tryGet.GetParameters().[0]
            Assert.True(tryGetParameter.ParameterType.IsByRef)
            Assert.True(tryGetParameter.IsOut)
            Assert.False(tryGetParameter.IsIn)
            let tryGetArguments = [| box 0 |]
            Assert.Equal<obj>(box true, tryGet.Invoke(instance, tryGetArguments))
            Assert.Equal(10, unbox<int> tryGetArguments.[0])

            let peek = interfaceType.GetMethod("Peek")
            let peekParameter = peek.GetParameters().[0]
            Assert.True(peekParameter.ParameterType.IsByRef)
            Assert.True(peekParameter.IsIn)
            Assert.False(peekParameter.IsOut)
            let peekArguments = [| box 7 |]
            Assert.Equal(17, unbox<int> (peek.Invoke(instance, peekArguments)))
            Assert.Equal(7, unbox<int> peekArguments.[0])

            let stringInterface = "IStrRefs" + suffix
            let stringClass = "StrHolder" + suffix
            eval (
                sprintf
                    "(definterface %s (^String Replace [this ^String& value])) (defclass %s [n] (implements %s)) (defmethod %s.Replace [^%s this value] [(str \"new-\" value) (str \"old-\" value)]) (def s (%s {:n 1}))"
                    stringInterface
                    stringClass
                    stringInterface
                    stringInterface
                    stringClass
                    stringClass
            )
            |> ignore
            let stringInstance = (session.Runtime.Registry.FindVar("user", "s").Value).Root
            let stringMethod = (TypeHints.tryResolveName stringInterface |> Option.get).GetMethod("Replace")
            Assert.Equal(typeof<string>, stringMethod.GetParameters().[0].ParameterType.GetElementType())
            let stringArguments = [| box "x" |]
            Assert.Equal<obj>(box "new-x", stringMethod.Invoke(stringInstance, stringArguments))
            Assert.Equal<obj>(box "old-x", stringArguments.[0])

    [<Fact>]
    member _.``delegates support declaration assignment invocation events and removal on both backends``() =
        for backend in [ ExecutionBackend.Interpreter; ExecutionBackend.ReflectionEmit ] do
            let suffix =
                match backend with
                | ExecutionBackend.Interpreter -> "I"
                | ExecutionBackend.ReflectionEmit -> "R"
            let delegateName = "IntUnary" + suffix
            let holder = "Holder" + suffix
            let session = Session(output = new StringWriter(), backend = backend)
            let eval (text: string) =
                let result = session.EvalText text
                Assert.Empty(result.Diagnostics)
                List.last result.Values

            eval (
                sprintf
                    "(defdelegate ^int %s [^int x]) (def twice (delegate %s (fn [x] (* x 2)))) (def inc (delegate %s (fn [x] (+ x 1)))) (set! twice inc) (defclass %s [^%s callback]) (def h (%s {:callback (fn [x] (* x 3))})) (def src (Wisp.Tests.WispEventSource.)) (def hits 0) (def handler (delegate System.EventHandler (fn [sender args] (set! hits (+ hits 1))))) (.add_Ping src handler) (.RaisePing src) (set! twice nil)"
                    delegateName
                    delegateName
                    delegateName
                    holder
                    delegateName
                    holder
            )
            |> ignore

            Assert.Equal<obj>(box null, eval "twice")
            Assert.Equal(42L, Convert.ToInt64(eval "((.-callback h) 14)", Globalization.CultureInfo.InvariantCulture))
            Assert.Equal(1L, Convert.ToInt64(eval "hits", Globalization.CultureInfo.InvariantCulture))
            eval "(.RaisePing src)" |> ignore
            Assert.Equal(2L, Convert.ToInt64(eval "hits", Globalization.CultureInfo.InvariantCulture))
            eval "(.remove_Ping src handler) (.RaisePing src)" |> ignore
            Assert.Equal(2L, Convert.ToInt64(eval "hits", Globalization.CultureInfo.InvariantCulture))

            let invokeAfterClear = session.EvalText "(twice 1)"
            Assert.NotEmpty invokeAfterClear.Diagnostics

    [<Fact>]
    member _.``delegates support byref pointer and Wisp-defined types on both backends``() =
        for backend in [ ExecutionBackend.Interpreter; ExecutionBackend.ReflectionEmit ] do
            let suffix =
                match backend with
                | ExecutionBackend.Interpreter -> "I"
                | ExecutionBackend.ReflectionEmit -> "R"
            let refDelegate = "RefFn" + suffix
            let pointDelegate = "MovePoint" + suffix
            let pointerDelegate = "PtrFn" + suffix
            let pointStruct = "Point" + suffix
            let session = Session(output = new StringWriter(), backend = backend)
            let eval (text: string) =
                let result = session.EvalText text
                Assert.Empty(result.Diagnostics)
                List.last result.Values

            eval (
                sprintf
                    "(defdelegate ^int %s [^int& x ^:out ^int& y]) (def f (delegate %s (fn [x y] [(* x 2) (+ x 1) 99]))) (defstruct %s [^int x]) (defdelegate ^%s %s [^%s p]) (def mp (delegate %s (fn [p] (%s {:x (+ (.-x p) 1)})))) (defdelegate ^int %s [^Int32* p]) (def pfn (delegate %s (fn [p] (System.Runtime.InteropServices.Marshal/ReadInt32 p))))"
                    refDelegate
                    refDelegate
                    pointStruct
                    pointStruct
                    pointDelegate
                    pointStruct
                    pointDelegate
                    pointStruct
                    pointerDelegate
                    pointerDelegate
            )
            |> ignore

            let byRef = (session.Runtime.Registry.FindVar("user", "f").Value).Root :?> Delegate
            let byRefMethod = byRef.GetType().GetMethod("Invoke")
            Assert.True(byRefMethod.GetParameters().[0].ParameterType.IsByRef)
            Assert.True(byRefMethod.GetParameters().[1].ParameterType.IsByRef)
            Assert.True(byRefMethod.GetParameters().[1].IsOut)
            let byRefArguments = [| box 5; box 0 |]
            Assert.Equal(10, unbox<int> (byRefMethod.Invoke(byRef, byRefArguments)))
            Assert.Equal(6, unbox<int> byRefArguments.[0])
            Assert.Equal(99, unbox<int> byRefArguments.[1])

            let pointValue = eval (sprintf "(%s {:x 4})" pointStruct)
            let movePoint = (session.Runtime.Registry.FindVar("user", "mp").Value).Root :?> Delegate
            let moved = movePoint.GetType().GetMethod("Invoke").Invoke(movePoint, [| pointValue |])
            Assert.Equal(5, unbox<int> (moved.GetType().GetProperty("x").GetValue moved))

            let pointer = (session.Runtime.Registry.FindVar("user", "pfn").Value).Root :?> Delegate
            let pointerMethod = pointer.GetType().GetMethod("Invoke")
            Assert.True(pointerMethod.GetParameters().[0].ParameterType.IsPointer)
            let memory = Runtime.InteropServices.Marshal.AllocHGlobal(sizeof<int>)
            try
                Runtime.InteropServices.Marshal.WriteInt32(memory, 42)
                Assert.Equal(42, unbox<int> (pointerMethod.Invoke(pointer, [| memory |])))
            finally
                Runtime.InteropServices.Marshal.FreeHGlobal memory

    [<Fact>]
    member _.``definition forms attach docstrings to var and method metadata on both backends``() =
        for backend in [ ExecutionBackend.Interpreter; ExecutionBackend.ReflectionEmit ] do
            let suffix =
                match backend with
                | ExecutionBackend.Interpreter -> "I"
                | ExecutionBackend.ReflectionEmit -> "R"
            let name baseName = baseName + suffix
            let session = Session(output = new StringWriter(), backend = backend)
            let eval (text: string) =
                let result = session.EvalText text
                Assert.Empty(result.Diagnostics)
                List.last result.Values
            let docOf formName = eval (sprintf "(get (meta %s) :doc)" formName) :?> string

            eval (
                sprintf
                    "(def answer%s \"answer doc\" 42) (def empty%s \"empty doc\") (defn add%s \"add doc\" [a b] (+ a b)) (defactive even%s \"active doc\" [n] (when (= 0 (mod n 2)) n)) (defmacro twice%s \"macro doc\" [x] (do x x)) (defclass Person%s \"class doc\" [^String name]) (deftype Alias%s \"alias doc\" [x]) (defstruct Point%s \"struct doc\" [^int x]) (definterface Greeter%s \"interface doc\" (^String Greet [this])) (defdelegate ^int Unary%s \"delegate doc\" [^int x]) (defunion Shape%s \"union doc\" (Circle%s \"case doc\" [radius]) (Empty%s)) (defmethod Person%s :greet \"method doc\" [^Person%s this] (.-name this)) (defmethod Person%s \"constructor doc\" [^String name] {:name name})"
                    suffix
                    suffix
                    suffix
                    suffix
                    suffix
                    suffix
                    suffix
                    suffix
                    suffix
                    suffix
                    suffix
                    suffix
                    suffix
                    suffix
                    suffix
                    suffix
            )
            |> ignore

            Assert.Equal("answer doc", docOf (name "answer"))
            Assert.Equal("empty doc", docOf (name "empty"))
            Assert.Equal("add doc", docOf (name "add"))
            Assert.Equal("active doc", docOf (name "even"))
            Assert.Equal("macro doc", docOf (name "twice"))
            Assert.Equal("class doc", docOf (name "Person"))
            Assert.Equal("alias doc", docOf (name "Alias"))
            Assert.Equal("struct doc", docOf (name "Point"))
            Assert.Equal("interface doc", docOf (name "Greeter"))
            Assert.Equal("delegate doc", docOf (name "Unary"))
            Assert.Equal("union doc", docOf (name "Shape"))
            Assert.Equal("case doc", docOf (name "Circle"))

            match session.Runtime.TryFindMethod(name "Person", "greet") with
            | Some methodInfo -> Assert.Equal(Some "method doc", methodInfo.Documentation)
            | None -> failwith "Expected the greet method to be registered."

            match session.Runtime.TryFindConstructor(name "Person", [| box "wisp" |], "user") with
            | Some constructor -> Assert.Equal(Some "constructor doc", constructor.Documentation)
            | None -> failwith "Expected the Person constructor to be registered."

            eval (sprintf "(doc %s)" (name "add")) |> ignore
            eval "(doc defn)" |> ignore
            let printed =
                match session.Runtime.Output with
                | :? StringWriter as writer -> writer.ToString()
                | _ -> ""
            Assert.Contains("add doc", printed)
            Assert.Contains("Defines a function", printed)

            let missing = session.EvalText "(doc does-not-exist)"
            Assert.Empty(missing.Diagnostics)

    [<Fact>]
    member _.``namespace docs render markdown and references on both backends``() =
        for backend in [ ExecutionBackend.Interpreter; ExecutionBackend.ReflectionEmit ] do
            let suffix =
                match backend with
                | ExecutionBackend.Interpreter -> "I"
                | ExecutionBackend.ReflectionEmit -> "R"
            let namespaceName = "docs.markdown" + suffix
            let output = new StringWriter()
            let session = Session(output = output, backend = backend)
            let eval (text: string) =
                let result = session.EvalText text
                Assert.Empty(result.Diagnostics)
                List.last result.Values

            eval (
                sprintf
                    "(ns %s \"A **rich** namespace with `add`.\") (defn add \"Adds **two** numbers.\\n\\n- one\\n- two\\n\\nSee `mul` and [the mul docs](doc:mul).\" [a b] (+ a b)) (defn mul \"Multiplies. See [add](add).\" [a b] (* a b))"
                    namespaceName
            )
            |> ignore

            eval (sprintf "(doc %s)" namespaceName) |> ignore
            eval "(doc add)" |> ignore
            eval "(doc mul)" |> ignore
            for formName in
                [ "def"; "defn"; "defactive"; "defmacro"; "defclass"; "deftype"; "defstruct"
                  "definterface"; "defdelegate"; "defunion"; "defmethod"; "ns" ] do
                eval (sprintf "(doc %s)" formName) |> ignore

            let printed = output.ToString()
            Assert.Contains("A rich namespace", printed)
            Assert.Contains("Adds two numbers", printed)
            Assert.Contains("• one", printed)
            Assert.Contains("See also:", printed)
            Assert.Contains(namespaceName + "/mul", printed)
            Assert.Contains("(defn add", printed)
            Assert.DoesNotContain("**rich**", printed)
            Assert.DoesNotContain("has no documentation.", printed)
            Assert.Equal(
                "A **rich** namespace with `add`.",
                eval (sprintf "(get (meta %s) :doc)" namespaceName) :?> string
            )

    [<Fact>]
    member _.``markdown docs use ANSI styling in the repl``() =
        let output = new StringWriter()
        let session = Session(output = output, renderMarkdown = true)
        let result = session.EvalText "(defn loud \"**bold** and `code`\" [] 1) (doc loud)"
        Assert.Empty(result.Diagnostics)
        let printed = output.ToString()
        Assert.Contains("\u001b[1mbold\u001b[0m", printed)
        Assert.Contains("\u001b[36mcode\u001b[0m", printed)

    [<Fact>]
    member _.``async await works on both backends and compiler paths``() =
        for backend in [ ExecutionBackend.Interpreter; ExecutionBackend.ReflectionEmit ] do
            for forceCompiler in [ false; true ] do
                if not forceCompiler || AsyncSupport.runtimeAsyncAvailable then
                    let session =
                        Session(
                            output = new StringWriter(),
                            backend = backend,
                            forceCompilerAsync = forceCompiler
                        )
                    let eval (text: string) =
                        let result = session.EvalText text
                        Assert.Empty(result.Diagnostics)
                        List.last result.Values

                    Assert.Equal(
                        30L,
                        Convert.ToInt64(
                            eval "(def t (async (let [a (await (async 10)) b (await (async 20))] (+ a b)))) (await t)",
                            Globalization.CultureInfo.InvariantCulture
                        )
                    )
                    Assert.Equal(
                        "yes",
                        eval "(await (async (let [x (await (async \"yes\"))] x)))" :?> string
                    )
                    Assert.Equal(
                        "done",
                        eval "(await (async (do (await (System.Threading.Tasks.Task/Delay 1)) \"done\")))" :?> string
                    )
                    Assert.Equal(
                        "caught",
                        eval "(await (async (try (await (async (throw \"boom\"))) (catch e \"caught\"))))" :?> string
                    )
                    Assert.Equal(
                        "caught",
                        eval "(await (async (try (do (throw \"boom\") (await (System.Threading.Tasks.Task/Delay 1))) (catch e \"caught\"))))" :?> string
                    )

                    Assert.Equal(
                        42L,
                        Convert.ToInt64(
                            eval "(await (new [System.Threading.Tasks.ValueTask Int] 42))",
                            Globalization.CultureInfo.InvariantCulture
                        )
                    )
                    Assert.Null(eval "(await (new System.Threading.Tasks.ValueTask))")

                    let valueTask =
                        eval "(def vtask (^[ValueTask Int] async (await (System.Threading.Tasks.Task/Delay 1)) 42)) vtask"
                    Assert.Equal("System.Threading.Tasks.ValueTask`1", valueTask.GetType().GetGenericTypeDefinition().FullName)
                    Assert.Equal(
                        42L,
                        Convert.ToInt64(eval "(await vtask)", Globalization.CultureInfo.InvariantCulture)
                    )

                    let unitValueTask =
                        eval "(def uvtask (^ValueTask async (await (System.Threading.Tasks.Task/Delay 1)) 42)) uvtask"
                    Assert.Equal("System.Threading.Tasks.ValueTask", unitValueTask.GetType().FullName)
                    Assert.Null(eval "(await uvtask)")

                    eval "(def failed (async (throw \"boom\")))" |> ignore
                    Assert.NotEmpty((session.EvalText "(await failed)").Diagnostics)

                    if backend = ExecutionBackend.ReflectionEmit
                       && not forceCompiler
                       && AsyncSupport.runtimeAsyncAvailable then
                        Assert.Equal(
                            3L,
                            Convert.ToInt64(
                                eval "(await (async (loop [i 0] (if (< i 3) (do (await (System.Threading.Tasks.Task/Delay 1)) (recur (+ i 1))) i))))",
                                Globalization.CultureInfo.InvariantCulture
                            )
                        )

                        let runtimeAsyncMethod =
                            session.Runtime.GeneratedAssemblies
                            |> List.collect (fun assembly -> assembly.GetTypes() |> Array.toList)
                            |> List.filter (fun generated -> generated.Name.StartsWith("Async_", StringComparison.Ordinal))
                            |> List.collect (fun generated ->
                                generated.GetMethods() |> Array.toList)
                            |> List.tryFind (fun methodInfo ->
                                methodInfo.Name = "Start"
                                && methodInfo.MethodImplementationFlags.HasFlag System.Reflection.MethodImplAttributes.Async)
                        Assert.True(runtimeAsyncMethod.IsSome)

    [<Fact>]
    member _.``reflection backend emits dynamic assemblies and hot reloads definitions``() =
        let s = session ()
        Assert.Equal(ExecutionBackend.ReflectionEmit, s.Backend)
        Assert.Empty(s.Runtime.GeneratedAssemblies)
        Assert.Equal(3L, expectInt s "(defn f [x] (+ x 1)) (f 2)")
        Assert.NotEmpty(s.Runtime.GeneratedAssemblies)
        Assert.All(s.Runtime.GeneratedAssemblies, fun assembly -> Assert.True(assembly.IsDynamic))
        Assert.Equal(20L, expectInt s "(defn f [x] (* x 10)) (f 2)")
        Assert.Single(s.Runtime.GeneratedAssemblies) |> ignore

    [<Fact>]
    member _.``interpreter and reflection backends agree``() =
        let interpreter = Session(output = new StringWriter(), backend = ExecutionBackend.Interpreter)
        let reflection = session ()
        let programs =
            [ "(+ 1 2 3)"
              "(let [x 2 y (+ x 3)] [x y (* x y)])"
              "(defn add [a b] (+ a b)) (add 4 5)"
              "(defn fact [n] (if (= n 0) 1 (* n (fact (- n 1))))) (fact 5)"
              "(defn adder [x] (fn [y] (+ x y))) ((adder 3) 9)"
              "(System.Math/Abs -7)"
              "(def sb (System.Text.StringBuilder. \"a\")) (.Append sb \"b\") (.ToString sb)"
              "(new [:List :Int])" ]
        for program in programs do
            let interpreted = lastValue interpreter program
            let emitted = lastValue reflection program
            Assert.Equal(Print.printString interpreted, Print.printString emitted)

    [<Fact>]
    member _.``print and println use the runtime output writer``() =
        let s = session ()
        let result = s.EvalText "(println \"hello\" 42) (str \"a\" 1 nil :b)"
        Assert.Empty(result.Diagnostics)
        Assert.Equal("hello 42" + Environment.NewLine, output s)
        Assert.Equal<obj>(box "a1:b", List.last result.Values)
