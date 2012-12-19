
-- Advanced Programming, Final Project
-- by Greg Terrono gterrono, Aaditya R. Shirodkar aadis

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

module Regex where

data Reg = Eps
  | Sym Char
  | Alt Reg Reg
  | Seq Reg Reg
  | Rep Reg
  | Any 
  | ZeroOrOne Reg
  | StartsWith Reg
  | EndsWith Reg
  | Extract Reg
  deriving (Show, Eq)

newtype MatchWithExtraction = MWE [String]
  deriving (Show, Eq)

data Result = Exists Bool
  | Matches [MatchWithExtraction]
  deriving (Show, Eq)

-- | Returns whether the full string matches the regex, and any extractions if applicable
acceptExact :: Reg -> String -> Result
acceptExact Eps _             = Exists True
acceptExact (Sym c) u         = Exists ([c] == u)  
acceptExact (Alt p q) u       = combinesOr [(acceptExact p u),  (acceptExact q u)]
acceptExact (Seq p q) u       = combinesOr [combineExtractions (acceptExact p u1) (acceptExact q u2) | (u1, u2) <- split u]
acceptExact (Rep p) u         = combinesOr $ (acceptExact p u) : [combinesAnd [acceptExact p u1 | u1 <- ps] | ps <- parts u]
acceptExact Any [_]           = Exists True
acceptExact Any _             = Exists False
acceptExact (ZeroOrOne p) u   = combinesOr [Exists (u == ""), (acceptExact p u)]
acceptExact (StartsWith _) _  = Exists False
acceptExact (EndsWith _) _    = Exists False
acceptExact (Extract p) u     = if acceptExactToBool p u then Matches [MWE [u]] else Exists False

-- | Combines two results, appending the list of extractions together when appropriate
combineExtractions :: Result -> Result -> Result
combineExtractions (Matches [MWE e1]) (Matches [MWE e2]) = Matches [MWE (e1 ++ e2)] --TODO: Sort this out
combineExtractions r1 r2 = combinesAnd [r1, r2]

-- | Combines a list of Results in an "and-like" fashion
combinesAnd :: [Result] -> Result
combinesAnd = foldr combiner (Exists True) where
  combiner (Exists u) (Exists v)   = Exists (u && v)
  combiner (Exists u) (Matches v)  = if u then (Matches v) else Exists False
  combiner (Matches v) (Exists u)  = if u then (Matches v) else Exists False
  combiner (Matches u) (Matches v) = Matches (u ++ v)

-- | Combines a list of Results in an "or-like" fashion
combinesOr :: [Result] -> Result
combinesOr = foldr combiner (Exists False) where
  combiner (Exists u) (Exists v)   = Exists (u || v)
  combiner (Exists _) (Matches v)  = Matches v
  combiner (Matches v) (Exists _)  = Matches v
  combiner (Matches u) (Matches v) = Matches (u ++ v)

-- | Returns the extractions resulting from the given regex being matched with the given string
acceptExtract :: Reg -> String -> [MatchWithExtraction]
acceptExtract p u = resultToExtraction (acceptResult p u) where
  acceptResult (EndsWith (StartsWith r)) v =
    acceptExact r v
  acceptResult (StartsWith r) v            =
    combinesOr [acceptExact r s | s <- substringsFromStart v]
  acceptResult (EndsWith r) v              =
    combinesOr [acceptExact r s | s <- substringsWithEnd v]
  acceptResult r v                         =
    combinesOr [acceptExact r s | s <- allSubstrings v]

-- | Returns whether or not a given regex matches a given string
accept :: Reg -> String -> Bool
accept p u = resultToBool (acceptResult p u) where
  acceptResult (EndsWith (StartsWith r)) v =
    acceptExact r v
  acceptResult (StartsWith r) v            =
    combinesOr [acceptExact r s | s <- substringsFromStart v]
  acceptResult (EndsWith r) v              =
    combinesOr [acceptExact r s | s <- substringsWithEnd v]
  acceptResult r v                         =
    combinesOr [acceptExact r s | s <- allSubstrings v]

resultToBool :: Result -> Bool
resultToBool r = case r of
  Exists b -> b
  _ -> True

resultToExtraction :: Result -> [MatchWithExtraction]
resultToExtraction r = case r of
  Exists _ -> []
  Matches b -> b

-- | Returns all of the ways a string matches a regex
matches :: Reg -> String -> Either String [String]
matches (EndsWith (StartsWith r)) u = filterHelper acceptExactToBool return r u
matches (StartsWith r) u = filterHelper accept substringsFromStart r u
matches (EndsWith r) u  = filterHelper accept substringsWithEnd r u
matches r u = filterHelper acceptExactToBool allSubstrings r u

-- | A commonly used pattern extracted into a higher ordered function
filterHelper :: (Reg -> String -> Bool) -> (String -> [String]) -> Reg -> String -> Either String [String]
filterHelper g f r u = case filter (\s -> g r s) (f u) of
   [] -> Left "No matches"
   l  -> Right l

-- | Takes a regex and a string and returns if the regex matches it with nothing left over
acceptExactToBool :: Reg -> String -> Bool
acceptExactToBool r s = resultToBool (acceptExact r s)

allSubstrings :: String -> [String]
allSubstrings ""       = [""]
allSubstrings l@(_:cs) = substringsFromStart l ++ allSubstrings cs

substringsFromStart :: String -> [String]
substringsFromStart "" = [""]
substringsFromStart l  = map (\x -> take x l) [0..length l]

-- | Returns all the substrings of a string that go until the end
substringsWithEnd :: String -> [String]
substringsWithEnd "" = []
substringsWithEnd l  = map (\x -> drop x l) [0..(length l) - 1]

-- | Takes a list and returns all the ways to split up that list into two parts
split :: [a] -> [([a], [a])]
split []     = [([], [])]
split (c:cs) = ([], c:cs):[(c: s1, s2) | (s1, s2) <- split cs]

-- | Takes a list and returns a list of all the the possible ways to split up the list
parts :: [a] -> [[[a]]]
parts []     = [[]]
parts [c]    = [[[c]]]
parts (c:cs) = concat [[(c:p):ps, [c]:p:ps]| p:ps <- parts cs]
