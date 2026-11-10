namespace Wisp.Compiler

open System
open System.Numerics
open Wisp.Core

module Numeric =
    let isNumber (value: obj) =
        match value with
        | :? int
        | :? int64
        | :? int16
        | :? sbyte
        | :? byte
        | :? uint16
        | :? uint
        | :? uint64
        | :? single
        | :? double
        | :? decimal
        | :? BigInteger
        | :? Ratio -> true
        | _ -> false

    let private numberName (value: obj) =
        if isNull value then "nil" else Print.printString value

    let private asParts (value: obj) =
        match value with
        | :? int as number -> Some(BigInteger number, BigInteger.One)
        | :? int64 as number -> Some(BigInteger number, BigInteger.One)
        | :? int16 as number -> Some(BigInteger number, BigInteger.One)
        | :? sbyte as number -> Some(BigInteger number, BigInteger.One)
        | :? byte as number -> Some(BigInteger number, BigInteger.One)
        | :? uint16 as number -> Some(BigInteger number, BigInteger.One)
        | :? uint as number -> Some(BigInteger number, BigInteger.One)
        | :? uint64 as number -> Some(BigInteger number, BigInteger.One)
        | :? BigInteger as number -> Some(number, BigInteger.One)
        | :? Ratio as ratio -> Some(ratio.Numerator, ratio.Denominator)
        | _ -> None

    let private asDouble (value: obj) =
        match value with
        | :? double as number -> number
        | :? single as number -> double number
        | :? decimal as number -> double number
        | :? BigInteger as number -> double number
        | :? Ratio as ratio -> double ratio.Numerator / double ratio.Denominator
        | :? int as number -> double number
        | :? int64 as number -> double number
        | :? int16 as number -> double number
        | :? sbyte as number -> double number
        | :? byte as number -> double number
        | :? uint16 as number -> double number
        | :? uint as number -> double number
        | :? uint64 as number -> double number
        | _ -> raise (ArgumentException(sprintf "%s is not a number." (numberName value)))

    let private asDecimal (value: obj) =
        match value with
        | :? decimal as number -> number
        | :? BigInteger as number -> decimal number
        | :? Ratio as ratio -> decimal ratio.Numerator / decimal ratio.Denominator
        | :? int as number -> decimal number
        | :? int64 as number -> decimal number
        | :? int16 as number -> decimal number
        | :? sbyte as number -> decimal number
        | :? byte as number -> decimal number
        | :? uint16 as number -> decimal number
        | :? uint as number -> decimal number
        | :? uint64 as number -> decimal number
        | _ -> raise (ArgumentException(sprintf "%s is not a decimal-compatible number." (numberName value)))

    type private Mode =
        | Exact
        | DecimalMode
        | DoubleMode

    let private modeOf (args: obj list) =
        if args |> List.exists (fun value -> value :? double || value :? single) then DoubleMode
        elif args |> List.exists (fun value -> value :? decimal) then DecimalMode
        else Exact

    let private boxInteger (value: BigInteger) =
        if value >= BigInteger Int64.MinValue && value <= BigInteger Int64.MaxValue then
            box (int64 value)
        else
            box value

    let private boxRatio (value: Ratio) =
        if value.IsInteger then boxInteger value.Numerator else box value

    let private requireParts (value: obj) =
        match asParts value with
        | Some parts -> parts
        | None -> raise (ArgumentException(sprintf "%s is not an exact number." (numberName value)))

    let private exact (numerator: BigInteger) (denominator: BigInteger) =
        if denominator.IsZero then raise (DivideByZeroException "Division by zero.")
        boxRatio (Ratio.Create(numerator, denominator))

    let add (args: obj list) =
        match args with
        | [] -> box 0L
        | [ single ] -> single
        | _ ->
            match modeOf args with
            | DoubleMode -> args |> List.sumBy asDouble |> box
            | DecimalMode -> args |> List.sumBy asDecimal |> box
            | Exact ->
                let mutable numerator = BigInteger.Zero
                let mutable denominator = BigInteger.One
                for value in args do
                    let n, d = requireParts value
                    numerator <- numerator * d + n * denominator
                    denominator <- denominator * d
                exact numerator denominator

    let subtract (args: obj list) =
        match args with
        | [] -> raise (ArgumentException "- expects at least one argument.")
        | [ single ] ->
            match modeOf [ single ] with
            | DoubleMode -> box (-(asDouble single))
            | DecimalMode -> box (-(asDecimal single))
            | Exact ->
                let n, d = requireParts single
                exact (-n) d
        | first :: rest ->
            match modeOf args with
            | DoubleMode -> (asDouble first - (rest |> List.sumBy asDouble)) |> box
            | DecimalMode -> (asDecimal first - (rest |> List.sumBy asDecimal)) |> box
            | Exact ->
                let mutable numerator, denominator = requireParts first
                for value in rest do
                    let n, d = requireParts value
                    numerator <- numerator * d - n * denominator
                    denominator <- denominator * d
                exact numerator denominator

    let multiply (args: obj list) =
        match args with
        | [] -> box 1L
        | _ ->
            match modeOf args with
            | DoubleMode -> args |> List.fold (fun acc value -> acc * asDouble value) 1.0 |> box
            | DecimalMode -> args |> List.fold (fun acc value -> acc * asDecimal value) 1.0m |> box
            | Exact ->
                let mutable numerator = BigInteger.One
                let mutable denominator = BigInteger.One
                for value in args do
                    let n, d = requireParts value
                    numerator <- numerator * n
                    denominator <- denominator * d
                exact numerator denominator

    let divide (args: obj list) =
        match args with
        | [] -> raise (ArgumentException "/ expects at least one argument.")
        | [ single ] ->
            match modeOf [ single ] with
            | DoubleMode -> box (1.0 / asDouble single)
            | DecimalMode -> box (1.0m / asDecimal single)
            | Exact ->
                let n, d = requireParts single
                if n.IsZero then raise (DivideByZeroException "Division by zero.")
                exact d n
        | first :: rest ->
            match modeOf args with
            | DoubleMode ->
                let mutable result = asDouble first
                for value in rest do result <- result / asDouble value
                box result
            | DecimalMode ->
                let mutable result = asDecimal first
                for value in rest do result <- result / asDecimal value
                box result
            | Exact ->
                let mutable numerator, denominator = requireParts first
                for value in rest do
                    let n, d = requireParts value
                    if n.IsZero then raise (DivideByZeroException "Division by zero.")
                    numerator <- numerator * d
                    denominator <- denominator * n
                exact numerator denominator

    let compareValues (left: obj) (right: obj) =
        if not (isNumber left && isNumber right) then
            raise (ArgumentException "Comparison operands must be numbers.")
        match modeOf [ left; right ] with
        | DoubleMode -> compare (asDouble left) (asDouble right)
        | DecimalMode -> compare (asDecimal left) (asDecimal right)
        | Exact ->
            let ln, ld = requireParts left
            let rn, rd = requireParts right
            compare (ln * rd) (rn * ld)

    let equalValues (left: obj) (right: obj) =
        if isNumber left && isNumber right then compareValues left right = 0
        else Object.Equals(left, right)

    let negate (value: obj) = subtract [ value ]
    let increment (value: obj) = add [ value; box 1L ]
    let decrement (value: obj) = subtract [ value; box 1L ]

    let toInt64 (value: obj) =
        match asParts value with
        | Some(numerator, denominator) when denominator.IsOne -> int64 numerator
        | _ -> Convert.ToInt64(value, Globalization.CultureInfo.InvariantCulture)

    let toDouble (value: obj) = asDouble value
