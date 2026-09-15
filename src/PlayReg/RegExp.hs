module PlayReg.RegExp where

{--
    ( someFunc,
    Reg(..),
    split',
    parts,
    accept,
    Semiring(..),

    )
   -}

data Reg
  = Eps --ε
  | Sym Char --a
  | Alt Reg Reg --α|β
  | Seq Reg Reg --αβ
  | Rep Reg --α*
  deriving (Show)

split' :: [a] -> [([a], [a])]
split' [] = [([], [])]
split' (c : cs) = ([], c : cs) : [(c : s1, s2) | (s1, s2) <- split' cs]

parts :: [a] -> [[[a]]]
parts [] = [[]]
parts [c] = [[[c]]]
parts (c : cs) =
  concat [[(c : p) : ps, [c] : p : ps] | p : ps <- parts cs]

accept :: Reg -> String -> Bool
accept Eps u = null u
accept (Sym c) u = u == [c]
accept (Alt p q) u = accept p u || accept q u
accept (Seq p q) u = or [(accept p u1 && accept q u2) | (u1, u2) <- split' u]
accept (Rep r) u = or [and [accept r ui | ui <- ps] | ps <- parts u]

-- weights

class Semiring s where
  zero, one :: s
  (<+>), (<.>) :: s -> s -> s

data Reg' c s
  = Eps' --ε
  | Sym' (c -> s) --a
  | Alt' (Reg' c s) (Reg' c s) --α|β
  | Seq' (Reg' c s) (Reg' c s) --αβ
  | Rep' (Reg' c s) --α*
  --deriving (Show)

sym :: Semiring s => Char -> Reg' Char s
sym c = Sym' (\x -> if x == c then one else zero)

weighted :: Semiring s => Reg -> Reg' Char s
weighted Eps = Eps'
weighted (Sym c) = sym c
weighted (Alt p q) = Alt' (weighted p) (weighted q)
weighted (Seq p q) = Seq' (weighted p) (weighted q)
weighted (Rep p) = Rep' (weighted p)

accept' :: Semiring s => Reg' c s -> [c] -> s
accept' Eps' u = if null u then one else zero
accept' (Sym' f) u = case u of
  [c] -> f c
  _ -> zero
accept' (Alt' p q) u = accept' p u <+> accept' q u
accept' (Seq' p q) u =
  sum' [(accept' p u1 <.> accept' q u2) | (u1, u2) <- split' u]
accept' (Rep' r) u =
  sum' [prod' [accept' r ui | ui <- ps] | ps <- parts u]

sum', prod' :: Semiring s => [s] -> s
sum' = foldr (<+>) zero
prod' = foldr (<.>) one

instance Semiring Bool where
  zero = False
  one = True
  (<+>) = (||)
  (<.>) = (&&)

instance Semiring Int where
  zero = 0
  one = 1
  (<+>) = (+)
  (<.>) = (*)

--accept r u = accept' (weighted r) u

{-
testes 1

let as = Alt (Sym 'a') (Rep (Sym 'a'))

accept' (weighted as) "a" :: Int  -- essa expressao pode casar pela primeiro caso ou o segundo caso do alt
let bs = Alt (Sym 'b') (Rep (Sym 'b'))
accept' (weighted  (Seq as bs) ) "ab" :: Int

accept' (weighted (Rep Eps)) "" :: Int -- aqui o numero de casamentos seria infinito
-}

-- o algorimo, baseado em automato

data REG
  = EPS
  | SYM Bool Char
  | ALT REG REG
  | SEQ REG REG
  | REP REG

--deriving(Show)

-- parser: recursive descent, standard regex precedence
--   regex ::= term ('|' term)*        -- alternation, lowest precedence
--   term  ::= factor*                  -- concatenation (juxtaposition)
--   factor ::= atom '*'?               -- postfix Kleene star, highest precedence
--   atom  ::= '(' regex ')' | <any char not in "()|*">
--
-- This replaces the earlier subregs/combine tokenizer, which only
-- split on '(' and ')' and so glued '|'/'*' to an adjacent literal
-- whenever they weren't next to a parenthesis (e.g. "(ab)|c" failed
-- because "|c" stayed as a single token). A recursive-descent parser
-- over the raw string sidesteps that whole class of bug.
regexP :: String -> (REG, String)
regexP s = altRest t rest
  where
    (t, rest) = termP s
    altRest acc ('|' : more) =
      let (t2, rest') = termP more
      in altRest (ALT acc t2) rest'
    altRest acc more = (acc, more)

termP :: String -> (REG, String)
termP s@(c : _) | c /= '|' && c /= ')' =
  let (f, rest')  = factorP s
      (t, rest'') = termP rest'
  in (SEQ f t, rest'')
termP s = (EPS, s)

factorP :: String -> (REG, String)
factorP s =
  let (atom, rest) = atomP s
  in case rest of
       ('*' : rest') -> (REP atom, rest')
       _             -> (atom, rest)

atomP :: String -> (REG, String)
atomP ('(' : rest) =
  let (r, rest') = regexP rest
  in case rest' of
       (')' : rest'') -> (r, rest'')
       _               -> error ("parseReg: expected ')', got: " ++ show rest')
atomP (c : rest) | c `notElem` ("()|*" :: String) = (SYM False c, rest)
atomP s = error ("parseReg: unexpected input: " ++ show s)

parseReg :: String -> REG
parseReg s = case regexP s of
  (r, "")   -> simplify r
  (_, rest) -> error ("parseReg: unexpected trailing input: " ++ show rest)

simplify :: REG -> REG
simplify (SEQ p q) = case (simplify p, simplify q) of
  (EPS, q') -> q'
  (p', EPS) -> p'
  (p', q')  -> SEQ p' q'
simplify (ALT p q) = ALT (simplify p) (simplify q)
simplify (REP r)   = REP (simplify r)
simplify r          = r

-- now works: parseReg "(ab)|c"  ==  parseReg "(ab)|(c)"

--"hello there" =~ "e" :: Bool
-- https://gabebw.com/blog/2015/10/11/regular-expressions-in-haskell

matchPS :: String -> String -> Bool
matchPS r s = match (parseReg r) s

-- ((a|b)*c(a|b)*c)*(a|b)*
-- parseReg "((a|b)*c(a|b)*c)*(a|b)*"
instance Show REG where
  show (EPS) = ""
  show (SYM _ c) = [c]
  show (ALT p q) = "(" ++ (show p) ++ "|" ++ (show q) ++ ")"
  show (SEQ p q) = "(" ++ (show p) ++ (show q) ++ ")"
  show (REP r) = (show r) ++ "*"

shift :: Bool -> REG -> Char -> REG
shift _ EPS _ = EPS
shift m (SYM _ x) c = SYM (m && x == c) x
shift m (ALT p q) c = ALT (shift m p c) (shift m q c)
shift m (SEQ p q) c =
  SEQ
    (shift m p c)
    (shift (m && empty p || final p) q c)
shift m (REP r) c = REP (shift (m || final r) r c)

empty :: REG -> Bool
empty EPS = True
empty (SYM _ _) = False
empty (ALT p q) = empty p || empty q
empty (SEQ p q) = empty p && empty q
empty (REP _) = True

final :: REG -> Bool
final EPS = False
final (SYM b _) = b
final (ALT p q) = final p || final q
final (SEQ p q) = (final p && empty q) || final q
final (REP r) = final r

match :: REG -> String -> Bool
match r [] = empty r
match r (c : cs) =
  final (foldl (shift False) (shift True r c) cs)

{--
match (SYM False 'a') "aa"
final (

foldl (shift False) (shift True (SYM False 'a') 'a') "a"

 (SYM True 'a') (shift False)  'a'

-- testes
as = ALT (SYM False 'a') (REP (SYM False 'a'))

a =  (SYM False 'a')
b =  (SYM True 'b')
a_b = SEQ a b

--}

---- implementacao final

data REGw c s = REGw
  { emptyw :: s,
    finalw :: s,
    regw :: REw c s
  }

--instance Show (REGw c s) where
--    show (REGw _ _ r) = show r

data REw c s
  = EPSw --ε
  | SYMw (c -> s) --a
  | ALTw (REGw c s) (REGw c s) --α|β
  | SEQw (REGw c s) (REGw c s) --αβ
  | REPw (REGw c s) --α*

instance Show (REw c s) where
  show EPSw = "ε"
  show (SYMw _) = "<sym>" -- the wrapped function c -> s has no Show instance
  show (ALTw _ _) = "<alt>" -- REGw itself has no Show instance to recurse into
  show (SEQw _ _) = "<seq>"
  show (REPw _) = "<rep>"

epsw :: Semiring s => REGw c s
epsw =
  REGw
    { emptyw = one,
      finalw = zero,
      regw = EPSw
    }

symw :: Semiring s => (c -> s) -> REGw c s
symw f =
  REGw
    { emptyw = zero,
      finalw = zero,
      regw = SYMw f
    }

altw :: Semiring s => REGw c s -> REGw c s -> REGw c s
altw p q =
  REGw
    { emptyw = emptyw p <+> emptyw q,
      finalw = finalw p <+> finalw q,
      regw = ALTw p q
    }

seqw :: Semiring s => REGw c s -> REGw c s -> REGw c s
seqw p q =
  REGw
    { emptyw = emptyw p <.> emptyw q,
      finalw = finalw p <.> emptyw q <+> finalw q,
      regw = SEQw p q
    }

repw :: Semiring s => REGw c s -> REGw c s
repw r =
  REGw
    { emptyw = one,
      finalw = finalw r,
      regw = REPw r
    }

matchw :: Semiring s => REGw c s -> [c] -> s
matchw r [] = emptyw r
matchw r (c : cs) =
  finalw (foldl (shiftw zero . regw) (shiftw one (regw r) c) cs)

shiftw :: Semiring s => s -> REw c s -> c -> REGw c s
shiftw _ EPSw _ = epsw
shiftw m (SYMw f) c = (symw f) {finalw = m <.> f c}
shiftw m (ALTw p q) c =
  altw (shiftw m (regw p) c) (shiftw m (regw q) c)
shiftw m (SEQw p q) c =
  seqw
    (shiftw m (regw p) c)
    (shiftw (m <.> emptyw p <+> finalw p) (regw q) c)
shiftw m (REPw r) c =
  repw (shiftw (m <+> finalw r) (regw r) c)

-- matchw (seqw (symw (=='a'))  (symw (=='b') )) "ab"

submatchw :: Semiring s => REGw (Int, c) s -> [c] -> s
submatchw r s =
  matchw (seqw arb (seqw r arb)) (zip [0 ..] s)
  where
    arb = repw (symw (\_ -> one))

class Semiring s => Semiringi s where
  index :: Int -> s

symi :: Semiringi s => Char -> REGw (Int, Char) s
symi c = symw weight
  where
    weight (pos, x)
      | x == c = index pos
      | otherwise = zero

data Leftmost = NoLeft | Leftmost Start deriving (Eq, Show)

data Start = NoStart | Start Int deriving (Eq, Show)

instance Semiring Leftmost where
  zero = NoLeft
  one = Leftmost NoStart
  NoLeft <+> x = x
  x <+> NoLeft = x
  Leftmost x <+> Leftmost y = Leftmost (leftmost x y)
    where
      leftmost NoStart NoStart = NoStart
      leftmost NoStart (Start i) = Start i
      leftmost (Start i) NoStart = Start i
      leftmost (Start i) (Start j) = Start (min i j)
  NoLeft <.> _ = NoLeft
  _ <.> NoLeft = NoLeft
  Leftmost x <.> Leftmost y = Leftmost (start x y)
    where
      start NoStart s = s
      start s _ = s

instance Semiringi Leftmost where
  index = Leftmost . Start

data LeftLong
  = NoLeftLong
  | LeftLong Range
  deriving (Eq, Show)

data Range
  = NoRange
  | Range
      Int
      Int
  deriving (Eq, Show)

instance Semiring LeftLong where
  zero = NoLeftLong
  one = LeftLong NoRange

  -- Addition
  NoLeftLong <+> x = x
  x <+> NoLeftLong = x
  LeftLong x <+> LeftLong y = LeftLong (leftlong x y)
    where
      -- The NoRange cases were missing here (unlike the analogous
      -- 'leftmost' helper for the Leftmost semiring, which does
      -- handle its NoStart case on both sides). Any regex containing
      -- a Rep whose body can match the empty string (e.g. (a|b)*)
      -- produces LeftLong NoRange values during the fold, and without
      -- these cases the pattern match was incomplete and crashed at
      -- run time with "Non-exhaustive patterns in function leftlong".
      leftlong NoRange NoRange = NoRange
      leftlong NoRange r       = r
      leftlong r       NoRange = r
      leftlong (Range i j) (Range k l)
        | i < k || i == k && j >= l = Range i j
        | otherwise = Range k l

  -- Multiplication
  NoLeftLong <.> _ = NoLeftLong
  _ <.> NoLeftLong = NoLeftLong
  LeftLong x <.> LeftLong y = LeftLong (range x y)
    where
      range NoRange r = r
      range r NoRange = r
      range (Range i _) (Range _ j) = Range i j

instance Semiringi LeftLong where
  index i = LeftLong (Range i i)

mkRegexP :: Semiringi s => String -> REGw (Int, Char) s
mkRegexP [] = epsw
mkRegexP (x : xs) = seqw (symi x) (mkRegexP xs)

-- Example values used by the tests in test/Spec.hs and by the demo in
-- app/Main.hs. The original comment here said "nao esta dando os
-- resultados esperados" (not giving the expected results) -- that was
-- LeftLong's (<+>) crashing on any pattern with a Rep matching the
-- empty string (see 'ab' below). Fixed; see the README.

a, ab, aaba, aib :: Semiringi s => REGw (Int, Char) s
a = symi 'a'
ab = repw (a `altw` symi 'b')
aaba = a `seqw` ab `seqw` a
aib = a `seqw` symi 'b'