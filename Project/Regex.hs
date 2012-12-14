
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

type Extraction = [String]

data Result = Exists Bool
  | Matches [Extraction]
  deriving Show

acceptExact :: Reg -> String -> Result
acceptExact Eps _             = Exists True
acceptExact (Sym c) u         = Exists ([c] == u)  
acceptExact (Alt p q) u       = combinesOr [(acceptExact p u),  (acceptExact q u)]
acceptExact (Seq p q) u       = combinesOr [combinesAnd [acceptExact p u1, acceptExact q u2] | (u1, u2) <- split u]
acceptExact (Rep p) u         = combinesOr $ (acceptExact p u) : [combinesAnd [acceptExact p u1 | u1 <- ps] | ps <- parts u]
acceptExact Any u             = Exists (u /= [])
acceptExact (ZeroOrOne p) u   = combinesOr [Exists(u == []), (acceptExact p u)]
acceptExact (StartsWith _) _  = Exists False
acceptExact (EndsWith _) _    = Exists False
acceptExact (Extract p) u     = case (matches p u) of
  Left _   -> Exists False
  Right v  -> Matches [v]

combinesAnd :: [Result] -> Result
combinesAnd = foldr combiner (Exists True) where
  combiner (Exists u) (Exists v)   = Exists (u && v)
  combiner (Exists u) (Matches v)  = if u then (Matches v) else Exists False
  combiner (Matches v) (Exists u)  = if u then (Matches v) else Exists False
  combiner (Matches u) (Matches v) = Matches (u ++ v)

combinesOr :: [Result] -> Result
combinesOr = foldr combiner (Exists False) where
  combiner (Exists u) (Exists v)   = Exists (u || v)
  combiner (Exists _) (Matches v)  = Matches v
  combiner (Matches v) (Exists _)  = Matches v
  combiner (Matches u) (Matches v) = Matches (u ++ v)

acceptExtract :: Reg -> String -> [Extraction]
acceptExtract p u = resultToExtraction (acceptResult p u)
  where
    acceptResult (EndsWith (StartsWith r)) v   = acceptExact r v
    acceptResult (StartsWith r) v              = combinesOr [acceptExact r s | s <- substringsFromStart v]
    acceptResult (EndsWith r) v                = combinesOr [acceptExact r s | s <- substringsWithEnd v]
    acceptResult r v                           = combinesOr [acceptExact r s | s <- allSubstrings v]

accept :: Reg -> String -> Bool
accept p u = resultToBool (acceptResult p u)
  where
    acceptResult (EndsWith (StartsWith r)) v   = acceptExact r v
    acceptResult (StartsWith r) v              = combinesOr [acceptExact r s | s <- substringsFromStart v]
    acceptResult (EndsWith r) v                = combinesOr [acceptExact r s | s <- substringsWithEnd v]
    acceptResult r v                           = combinesOr [acceptExact r s | s <- allSubstrings v]

resultToBool :: Result -> Bool
resultToBool r = case r of
                 Exists b -> b
                 _ -> True

resultToExtraction :: Result -> [Extraction]
resultToExtraction r = case r of
                       Exists _ -> []
                       Matches b -> b

matches :: Reg -> String -> Either String [String]

matches (EndsWith (StartsWith r)) u = if resultToBool (acceptExact r u) then Right [u] else Left "No matches"
matches (StartsWith r) u = helper2 accept substringsFromStart r u
matches (EndsWith r) u  = helper2 accept substringsWithEnd r u
matches r u = helper2 (helper3 resultToBool acceptExact) allSubstrings r u where
  helper3 f g s v = f (g s v)

helper2 :: (Reg -> String -> Bool) -> (String -> [String]) -> Reg -> String -> Either String [String]
helper2 g f r u = case foldr helper [] [if g r p then Just p else Nothing | p <- f u] of
   [] -> Left "No matches"
   l  -> Right l

helper :: Maybe a -> [a] -> [a]
helper (Just s) l = s:l
helper Nothing l  = l

allSubstrings :: String -> [String]
allSubstrings []       = []
allSubstrings l@(_:cs) = (map (\x -> take x l) [0..length l]) ++ allSubstrings cs

substringsFromStart :: String -> [String]
substringsFromStart [] = []
substringsFromStart l  = map (\x -> take x l) [0..length l]

substringsWithEnd :: String -> [String]
substringsWithEnd [] = []
substringsWithEnd l  = map (\x -> drop x l) [0..(length l) - 1]

split :: [a] -> [([a], [a])]
split []     = [([], [])]
split (c:cs) = ([], c:cs):[(c: s1, s2) | (s1, s2) <- split cs]

parts :: [a] -> [[[a]]]
parts[]      = [[]]
parts [c]    = [[[c]]]
parts (c:cs) = concat [[(c:p):ps, [c]:p:ps]| p:ps <- parts cs]
