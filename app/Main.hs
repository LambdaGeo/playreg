module Main (main) where

import PlayReg.RegExp

main :: IO ()
main = do
  putStrLn "== Plain matching (parseReg + match) =="
  let pattern = "((a|b)*c(a|b)*c)*(a|b)*"
  mapM_
    (\input -> putStrLn ("  " ++ show input ++ " matches " ++ show pattern ++ "? " ++ show (matchPS pattern input)))
    ["acbc", "aabb", "ab", "abcx"]

  putStrLn ""
  putStrLn "== Weighted matching: counting derivations (accept' over Int) =="
  let ambiguous = Alt (Sym 'a') (Rep (Sym 'a'))
  putStrLn
    ( "  \"a\" matches (a | a*) in "
        ++ show (accept' (weighted ambiguous) "a" :: Int)
        ++ " distinct ways (the same string can derive from either branch)"
    )

  putStrLn ""
  putStrLn "== Submatch position tracking via semirings =="
  -- 'a' matches a single 'a'; 'ab' matches "a immediately followed by b";
  -- 'aaba' matches "a, then any run of a/b, then a" -- see RegExp.hs.
  let haystack = "xxabaxx"
  putStrLn ("  searching " ++ show haystack ++ " for a(a|b)*a:")
  putStrLn ("    leftmost start position: " ++ show (submatchw aaba haystack :: Leftmost))
  putStrLn ("    leftmost-longest range:  " ++ show (submatchw aaba haystack :: LeftLong))

  let haystack2 = "xaby"
  putStrLn ("  searching " ++ show haystack2 ++ " for a then b:")
  putStrLn ("    leftmost-longest range:  " ++ show (submatchw aib haystack2 :: LeftLong))

  putStrLn ""
  putStrLn "== Regression: the two bugs fixed in this library =="
  putStrLn
    ( "  parseReg \"(ab)|c\" on \"c\": "
        ++ show (matchPS "(ab)|c" "c")
        ++ "  (used to be False -- alternation without parens on the right was mis-tokenized)"
    )
  putStrLn
    ( "  submatchw over (a|b)* no longer crashes: "
        ++ show (submatchw ab "xaby" :: LeftLong)
        ++ "  (LeftLong's (<+>) used to pattern-match-fail on NoRange)"
    )
