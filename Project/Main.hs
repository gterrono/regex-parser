
-- Advanced Programming, Final Project
-- by Greg Terrono gterrono, Aaditya R. Shirodkar aadis

import Control.Monad

import Text.PrettyPrint.HughesPJ (Doc, (<+>),($$),(<>))
import qualified Text.PrettyPrint.HughesPJ as PP

import Regex
import ParserTrans
import ParserCombinators

import Test.HUnit
import Test.QuickCheck


main :: IO () 
main = return ()

-- Pretty Printing a RegEx
---------------------------------------------
class PP a where
  pp :: a -> Doc

instance PP Reg where
  pp (Sym a) = PP.char a
  pp (Alt (Sym a) (Sym b)) = (pp (Sym a)) <> PP.char '|' <> (pp (Sym b))
  pp (Alt (Sym a) b) = (pp (Sym a)) <> PP.char '|' <> PP.parens (pp b)
  pp (Alt a (Sym b)) = PP.parens (pp a) <> PP.char '|' <> (pp (Sym b))
  pp (Alt a b) = PP.parens (pp a) <> PP.char '|' <> PP.parens (pp b)
  pp (Seq a (Rep b)) | a == b = (pp a) <> PP.char '+'
  pp (Seq a b) = (pp a) <> (pp b)
  pp (Rep (Sym a)) = (pp (Sym a)) <> PP.char '*'
  pp (Rep a) = PP.parens (pp a) <> PP.char '*'
  pp Any = PP.char '.'
  pp (ZeroOrOne a) = (pp a) <> PP.char '?'

display :: PP a => a -> String
display = show . pp



-- Parsing a RegEx
---------------------------------------------

symP :: GenParser Char Reg
symP = liftM Sym $ satisfy (\x -> x /= ')' && x /= '^')

anyP :: GenParser Char Reg
anyP = char '.' >> return Any

parensP :: GenParser Char Reg
parensP = choice [between (char '(') statementP ((char ')')), anyP, symP]

statementP :: GenParser Char Reg
statementP = sequenceP <|> nonSequenceP where
  altP = do
    s1 <- choice [repP, zeroP, parensP]
    char '|'
    s2 <- parensP
    return (Alt s1 s2)
  repP = do
    s1 <- parensP
    char '*'
    return (Rep s1)
  zeroP = do
    s1 <- parensP
    char '?'
    return (ZeroOrOne s1)
  oneP = do
    s1 <- parensP
    char '+'
    return (Seq s1 (Rep s1))
  sequenceP = do
    s1 <- nonSequenceP
    s2 <- statementP
    return (Seq s1 s2)
  nonSequenceP = choice [altP, repP, zeroP, oneP, parensP]

startsWithP :: GenParser Char Reg
startsWithP = startsP <|> statementP where
  startsP = do
    char '^'
    s1 <- statementP
    return (StartsWith s1)
