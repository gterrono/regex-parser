
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
  deriving (Show, Eq)

acceptExact :: Reg -> String -> Bool
acceptExact Eps _             = True
acceptExact (Sym c) u         = [c] == u  
acceptExact (Alt p q) u       = acceptExact p u || acceptExact q u
acceptExact (Seq p q) u       = or [acceptExact p u1 && acceptExact q u2 | (u1, u2) <- split u]
acceptExact (Rep p) u         = or ((acceptExact p u):[and [acceptExact p u1 | u1 <- ps] | ps <- parts u])
acceptExact Any u             = u /= []
acceptExact (ZeroOrOne p) u   = u == [] || acceptExact p u
acceptExact (StartsWith _) _  = False
acceptExact (EndsWith _) _    = False

accept :: Reg -> String -> Bool
accept (EndsWith (StartsWith r)) u   = acceptExact r u
accept (StartsWith r) u              = or [acceptExact r p | p <- substringsFromStart u]
accept (EndsWith r) u                = or [acceptExact r p | p <- substringsWithEnd u]
accept r u                           = or [acceptExact r p | p <- allSubstrings u]

matches :: Reg -> String -> Either String [String]
matches (EndsWith (StartsWith r)) u = if acceptExact r u then Right [u] else Left "No matches"
matches (StartsWith r) u = helper2 accept substringsFromStart r u
matches (EndsWith r) u  = helper2 accept substringsWithEnd r u
matches r u = helper2 acceptExact allSubstrings r u

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
