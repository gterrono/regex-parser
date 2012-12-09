
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
  deriving (Show, Eq)

acceptExact:: Reg -> String -> Bool
acceptExact Eps _           = True
acceptExact (Sym c) u       = [c] == u  
acceptExact (Alt p q) u     = acceptExact p u || acceptExact q u
acceptExact (Seq p q) u     = or [acceptExact p u1 && acceptExact q u2 | (u1, u2) <- split u]
acceptExact (Rep _) _       = True
acceptExact Any u           = u /= []
acceptExact (ZeroOrOne p) u = u == [] || acceptExact p u
acceptExact (StartsWith _) _ = False

accept :: Reg -> String -> Bool
accept (StartsWith r) u = or [acceptExact r p | p <- substringsFromStart u]
accept r u = or [acceptExact r p | p <- allSubstrings u]

allSubstrings :: String -> [String]
allSubstrings [] = []
allSubstrings l@(_:cs) = (map (\x -> take x l) [1..length l]) ++ allSubstrings cs

substringsFromStart :: String -> [String]
substringsFromStart [] = []
substringsFromStart l = map (\x -> take x l) [1..length l]

split :: [a] -> [([a], [a])]
split []     = [([], [])]
split (c:cs) = ([], c:cs):[(c: s1, s2) | (s1, s2) <- split cs]

parts :: [a] -> [[[a]]]
parts[]      = [[]]
parts [c]    = [[[c]]]
parts (c:cs) = concat [[(c:p):ps, [c]:p:ps]| p:ps <- parts cs]
