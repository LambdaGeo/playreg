module Main (main) where

import PlayReg.RegExp
import System.Exit (exitFailure)

type Case = (String, Bool)

cases :: [Case]
cases =
  [ ("accept: brute-force matcher works on a simple Alt/Rep", accept (Alt (Sym 'a') (Rep (Sym 'a'))) "a")
  , ("accept': weighted derivation count for a|a* on \"a\" is 2",
      accept' (weighted (Alt (Sym 'a') (Rep (Sym 'a')))) "a" == (2 :: Int))
  , ("match/parseReg: simple literal", matchPS "a" "a")
  , ("match/parseReg: simple literal rejects mismatch", not (matchPS "a" "b"))
  , ("match/parseReg: alternation, first branch", matchPS "a|b" "a")
  , ("match/parseReg: alternation, second branch", matchPS "a|b" "b")
  , ("match/parseReg: Kleene star matches empty", matchPS "a*" "")
  , ("match/parseReg: Kleene star matches repetition", matchPS "a*" "aaaa")

  -- Regression test for the parseReg tokenizer bug: alternation with
  -- an unparenthesized right-hand side used to fail because the old
  -- subregs/combine tokenizer glued '|' to the following literal
  -- whenever it wasn't adjacent to a parenthesis.
  , ("parseReg regression: \"(ab)|c\" now parses like \"(ab)|(c)\"",
      matchPS "(ab)|c" "ab" && matchPS "(ab)|c" "c" && not (matchPS "(ab)|c" "x"))
  , ("parseReg regression: postfix * before an unparenthesized alternative",
      matchPS "(ab)*|c" "abab" && matchPS "(ab)*|c" "c" && not (matchPS "(ab)*|c" "ababc"))
  , ("parseReg: nested groups from the file's own worked example",
      matchPS "((a|b)*c(a|b)*c)*(a|b)*" "acbc")

  , ("matchw: weighted matcher over Bool behaves like the unweighted one",
      matchw (seqw (symw (== 'a')) (symw (== 'b'))) "ab")

  -- Regression tests for the LeftLong crash: its (<+>) was missing
  -- the NoRange cases that the analogous Leftmost instance already
  -- handled, so any pattern containing a Rep that can match the
  -- empty string (e.g. (a|b)*) crashed with "Non-exhaustive
  -- patterns in function leftlong".
  , ("submatchw/LeftLong regression: (a|b)* no longer crashes",
      (submatchw ab "xaby" :: LeftLong) == LeftLong (Range 1 2))
  , ("submatchw/LeftLong regression: matching only the empty string gives NoRange",
      (submatchw ab "" :: LeftLong) == LeftLong NoRange)
  , ("submatchw/Leftmost: finds the correct start position",
      (submatchw a "xax" :: Leftmost) == Leftmost (Start 1))
  , ("submatchw/LeftLong: finds the correct start/end range",
      (submatchw aaba "xxabaxx" :: LeftLong) == LeftLong (Range 2 4))
  , ("submatchw/LeftLong: leftmost-longest tie-break prefers the longer match",
      (submatchw (a `altw` (a `seqw` a)) "aax" :: LeftLong) == LeftLong (Range 0 1))
  ]

main :: IO ()
main = do
  results <- mapM report cases
  if and results
    then putStrLn "All tests passed."
    else exitFailure
  where
    report (name, ok) = do
      putStrLn ((if ok then "OK     " else "FAILED ") ++ name)
      pure ok
