# About exhaustive pattern matching

Wisp checks whether a `match` covers every case of a finite domain. The check is
static and runs before the match is compiled.

## Finite domains

### Unions

A union declares a closed set of cases:

```clojure
(defunion Shape
  (Circle [radius])
  (Square [side]))

(match shape
  ((Circle radius) (* radius radius))
  ((Square side) (* side side)))
```

Using `Circle` makes the match a `Shape` match. Omitting `Square` reports
`WISP3044` with `missing cases: Square`.

### Booleans

Boolean patterns cover `true` and `false`:

```clojure
(match enabled
  (true "on")
  (false "off"))
```

Omitting one value reports `WISP3044`.

## What counts as coverage

- An unguarded union case or boolean literal covers that case.
- `_` and an untyped binding symbol cover everything.
- `(or true false)` and other `or` alternatives combine their coverage.
- A typed binding covers its type domain: `^Boolean b` covers both booleans,
  and `^Shape s` covers the whole `Shape` union.
- A clause with `:when` is ignored for exhaustiveness: the guard can fail, so
  the case falls through.

## What is not checked

Pattern domains that are not finite cannot be exhaustively checked in general:

- numbers, strings, characters, and keywords;
- vectors and maps (their element domains are open);
- active patterns;
- nested field patterns, unless their outer union case is covered.

A match over an infinite domain still works; use `_` or a binding when a
fallback is intended.

## Diagnostics

`WISP3044` is a compile error. Its notes list the missing union cases or boolean
values:

```text
match is not exhaustive for union Shape.
  missing cases: Square
```

See the language examples in the [README](../../README.md#pattern-matching-unions-and-active-patterns)
and the [project loading explanation](double-pass-projects.md).
