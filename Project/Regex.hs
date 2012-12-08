module Regex where

data Reg = Eps
  | Sym Char
  | Alt Reg Reg
  | Seq Reg Reg
  | Rep Reg

accept:: Reg -> String -> Bool
accept Eps u     = null u
accept (Sym c) u = [c] == u
accept (Alt p q) u = accept p u || accept q u
accept (Seq p q) u = or [accept p u1 && accept q u2 | (u1, u2) <- split u]
accept (Rep p) u = or ((accept p u):[and [accept p u1 | u1 <- ps] | ps <- parts u])

split :: [a] -> [([a], [a])]
split [] = [([], [])]
split (c:cs) = ([], c:cs):[(c: s1, s2) | (s1, s2) <- split cs]

parts :: [a] -> [[[a]]]
parts[] = [[]]
parts [c] = [[[c]]]
parts (c:cs) = concat [[(c:p):ps, [c]:p:ps]| p:ps <- parts cs]