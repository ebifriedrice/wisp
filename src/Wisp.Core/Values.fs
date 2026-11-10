namespace Wisp.Core

open System
open System.Numerics

/// Exact rational number, mirroring Clojure's ratio literals.
[<Sealed>]
type Ratio private (numerator: BigInteger, denominator: BigInteger) =
    static let normalize (n: BigInteger) (d: BigInteger) =
        if d.IsZero then invalidArg "denominator" "A ratio denominator cannot be zero."
        let sign = if d.Sign < 0 then BigInteger.MinusOne else BigInteger.One
        let n = n * sign
        let d = d * sign
        if n.IsZero then BigInteger.Zero, BigInteger.One
        else
            let divisor = BigInteger.GreatestCommonDivisor(BigInteger.Abs n, d)
            n / divisor, d / divisor

    static member Create(numerator: BigInteger, denominator: BigInteger) =
        let n, d = normalize numerator denominator
        Ratio(n, d)

    member _.Numerator = numerator
    member _.Denominator = denominator
    member _.IsInteger = denominator.IsOne

    override _.ToString() = String.Format("{0}/{1}", numerator, denominator)

    override _.Equals(other: obj) =
        match other with
        | :? Ratio as ratio -> numerator = ratio.Numerator && denominator = ratio.Denominator
        | _ -> false

    override _.GetHashCode() = HashCode.Combine(numerator, denominator)
