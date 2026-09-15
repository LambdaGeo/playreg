# PlayReg

A regex engine built around Brzozowski derivatives, following the
["A Play on Regular Expressions"](https://dl.acm.org/doi/10.1145/1863543.1863594)
functional pearl (Fischer, Huch, Wilke), extended with a `Semiring`
abstraction so the same matching logic can compute a `Bool`
(accept/reject), an `Int` (count of matching derivations), or a
submatch position via the `Leftmost` and `LeftLong` semirings.

No dependencies beyond `base`.

## Three layers, in increasing sophistication

1. **`Reg`/`accept`** — a naive, exponential-time reference
   implementation via direct case splitting on the string.
2. **`REG`/`match`** — an efficient version based on Brzozowski
   derivatives (`shift`), linear in the length of the input for a
   fixed pattern. `parseReg` parses a small regex syntax (literals,
   `|`, `*`, parenthesized grouping) into a `REG` via a
   recursive-descent parser with standard precedence (`*` binds
   tightest, then concatenation, then `|`).
3. **`REGw`/`matchw`** — the same derivative algorithm generalized
   over an arbitrary `Semiring`, so weighted variants (submatch
   position tracking, counting derivations, etc.) share one
   implementation with plain boolean matching. `submatchw` searches a
   longer string for the leftmost (or leftmost-longest) match of a
   pattern, using the `Leftmost`/`LeftLong` semirings to track
   position.

## Fixed since the initial version

- **`parseReg` alternation bug.** The original tokenizer only split
  the input on `(`/`)`, so `|` or `*` occurring right next to a
  literal — rather than next to a parenthesis — stayed glued to it as
  a single token (`"(ab)|c"` produced a garbled tree, while
  `"(ab)|(c)"` worked). Replaced with a recursive-descent parser
  operating directly on the string, which has no such gap.
- **`LeftLong`'s `(<+>)` crash.** Its `leftlong` helper only handled
  the case where both operands were already a `Range`, missing the
  `NoRange` cases that the analogous `Leftmost` instance already
  handled correctly. Any pattern containing a `Rep` whose body can
  match the empty string (e.g. `(a|b)*`) produced `LeftLong NoRange`
  values during the fold, which crashed with "Non-exhaustive
  patterns in function leftlong". Both `NoRange`/`NoRange` and mixed
  `NoRange`/`Range` cases are now handled, mirroring `Leftmost`.

Both are covered by regression tests in `test/Spec.hs`.

## Usage note

`submatchw`, `matchw`, and the values built from `symi` (`a`, `ab`,
`aaba`, `aib`, ...) are polymorphic over the `Semiring` (and, for
`symi`, `Semiringi`) type class, with no functional dependency tying
that type to anything else in the call. GHC can't infer it from
context alone — an explicit annotation is required at the call site:

```haskell
submatchw ab "xaby" :: LeftLong    -- required, not just cosmetic
```

Omitting it produces an "Ambiguous type variable" error at compile
time, not a runtime failure — this is normal Haskell type-class
behavior, not a bug in the library.

## Build and run

```sh
cabal build
cabal run playreg-demo   # a short tour of the library, printed to stdout
cabal test                # 17 regression tests
```

Or directly with GHC (no dependencies beyond `base`, so no package
setup needed):
```sh
ghc -isrc -iapp -o playreg-demo app/Main.hs
./playreg-demo

ghc -isrc -itest -o playreg-test test/Spec.hs
./playreg-test
```

## Project layout

```
playreg/
├── playreg.cabal
├── src/PlayReg/RegExp.hs   -- the library
├── app/Main.hs              -- demo executable
└── test/Spec.hs             -- 17 regression tests
```
