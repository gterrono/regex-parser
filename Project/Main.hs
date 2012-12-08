
-- Advanced Programming, HW 5
-- by Greg Terrono gterrono, Aaditya R. Shirodkar aadis

import Control.Monad

--import Text.PrettyPrint.HughesPJ (Doc, (<+>),($$),(<>))
--import qualified Text.PrettyPrint.HughesPJ as PP

import Regex
import ParserTrans
import ParserCombinators

import Test.HUnit
import Test.QuickCheck


main :: IO () 
main = return ()

-- Problem 0
---------------------------------------------




-- Problem 1
---------------------------------------------

symP :: GenParser Char Reg
symP = liftM Sym $ satisfy (\x -> x /= ')')

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
  sequenceP = do
    s1 <- nonSequenceP
    s2 <- statementP
    return (Seq s1 s2)
  nonSequenceP = choice [altP, repP, zeroP, parensP]

