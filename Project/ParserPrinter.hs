-- Advanced Programming, Final Project
-- by Greg Terrono gterrono, Aaditya R. Shirodkar aadis

module ParserPrinter (returnBool, returnMatches, returnExtractions) where

import Control.Monad

import Text.PrettyPrint.HughesPJ (Doc, (<+>),($$),(<>))
import qualified Text.PrettyPrint.HughesPJ as PP

import Regex
import ParserTrans
import ParserCombinators

import Test.HUnit
import Test.QuickCheck

-- Pretty Printing a RegEx
---------------------------------------------
class PP a where
  pp :: a -> Doc

instance PP Reg where
  pp (Sym a)               = PP.char a
  {- pp (Alt (Sym a) (Sym b)) = (pp (Sym a)) <> PP.char '|' <> (pp (Sym b))
  pp (Alt (Sym a) Any)     = (pp (Sym a)) <> PP.char '|' <> (pp Any)
  pp (Alt Any (Sym a))     = (pp Any) <> PP.char '|' <> (pp (Sym a))
  pp (Alt Any Any)         = (pp Any) <> PP.char '|' <> (pp Any)
  pp (Alt Any a)           = (pp Any) <> PP.char '|' <> PP.parens (pp a)
  pp (Alt a Any)           = PP.parens (pp a) <> PP.char '|' <> (pp Any)
  pp (Alt (Sym a) b)       = (pp (Sym a)) <> PP.char '|' <> PP.parens (pp b)
  pp (Alt a (Sym b))       = PP.parens (pp a) <> PP.char '|' <> (pp (Sym b))
  pp (Alt a b)             = PP.parens (pp a) <> PP.char '|' <> PP.parens (pp b) -}
  pp (Alt a b)             = (pp a) <> PP.char '|' <> (pp b)
  pp (Seq a (Rep b)) 
   | a == b                = (pp a) <> PP.char '+'
  pp (Seq a b)             = (pp a) <> (pp b)
  {- pp (Rep (Sym a))         = (pp (Sym a)) <> PP.char '*'
  pp (Rep Any)             = (pp Any) <> PP.char '*'
  pp (Rep a)               = PP.parens (pp a) <> PP.char '*' -}
  pp (Rep a)               = (pp a) <> PP.char '*'
  pp Any                   = PP.char '.'
  pp (ZeroOrOne a)         = (pp a) <> PP.char '?'
  pp (StartsWith a)        = PP.char '^' <> (pp a)
  pp (EndsWith a)          = (pp a) <> PP.char '$' 
  pp (Extract a)           = PP.parens (pp a)

display :: PP a => a -> String
display = show . pp



-- Parsing a RegEx
---------------------------------------------

symP :: GenParser Char Reg
symP = liftM Sym $ satisfy (\x -> x /= ')' && x /= '^' && x /= '$')

anyP :: GenParser Char Reg
anyP = char '.' >> return Any

escapeP :: GenParser Char Reg
escapeP = do
  char '\\'
  liftM Sym getC

parensP :: GenParser Char Reg
parensP = choice [between (char '(') (liftM Extract statementP) ((char ')')), anyP, symP]

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
  nonSequenceP = choice [escapeP, altP, repP, zeroP, oneP, parensP]

endsWithP :: GenParser Char Reg
endsWithP = endsP <|> startsWithP where
  endsP = do
    s1 <- startsWithP
    char '$'
    return (EndsWith s1)

startsWithP :: GenParser Char Reg
startsWithP = startsP <|> statementP where
  startsP = do
    char '^'
    s1 <- statementP
    return (StartsWith s1)

returnBool :: String -> String -> Bool
returnBool a b = case (parse endsWithP a) of
  Left _  -> False
  Right c -> accept c b

returnMatches :: String -> String -> Either String [String]
returnMatches a b = case (parse endsWithP a) of
  Left _  -> Left "Not a valid Regex"
  Right c -> matches c b

returnExtractions :: String -> String -> [MatchWithExtraction]
returnExtractions a b = case (parse endsWithP a) of
  Left _  -> []
  Right c -> acceptExtract c b

testAcceptExact :: Test
testAcceptExact = TestList[ 
  acceptExact (Sym 'a') "a" ~?= Exists True,
  acceptExact (Sym 'a') "ab" ~?= Exists False,
  acceptExact (Sym 'a') "b" ~?= Exists False,
  acceptExact Eps "a" ~?= Exists True,
  acceptExact (Alt (Seq (Sym 'a') (Sym 'b')) (Sym 'b')) "b" ~?= Exists True,
  acceptExact (Alt (Seq (Sym 'a') (Sym 'b')) (Sym 'b')) "ab" ~?= Exists True,
  acceptExact (Alt (Seq (Sym 'a') (Sym 'b')) (Sym 'b')) "a" ~?= Exists False,
  acceptExact (Seq (Sym 'a') (Seq (Sym 'b') (Sym 'c'))) "abc" ~?= Exists True,
  acceptExact (Seq (Sym 'a') (Seq (Sym 'b') (Sym 'c'))) "a" ~?= Exists False,
  acceptExact (Rep (Sym 'a')) "a" ~?= Exists True,
  acceptExact (Rep (Sym 'a')) "" ~?= Exists True,
  acceptExact (Rep (Sym 'a')) "aaaa" ~?= Exists True,
  acceptExact (Rep (Sym 'a')) "b" ~?= Exists False,
  acceptExact (Rep (Sym 'a')) "baaa" ~?= Exists False,
  acceptExact Any "a" ~?= Exists True,
  acceptExact Any "ab" ~?= Exists False,
  acceptExact (ZeroOrOne (Sym 'a')) "" ~?= Exists True,
  acceptExact (ZeroOrOne (Sym 'a')) "a" ~?= Exists True,
  acceptExact (ZeroOrOne (Sym 'a')) "aa" ~?= Exists False,
  acceptExact (ZeroOrOne (Sym 'a')) "b" ~?= Exists False,
  acceptExact (Extract (Sym 'a')) "a" ~?= Matches [MWE ["a"]],
  acceptExact (Extract (Sym 'a')) "ab" ~?= Exists False,
  acceptExact (Seq (Extract (Seq (Sym 'a') (Sym 'b'))) (Rep (Sym 'c'))) "ab" ~?= Matches [MWE ["ab"]],
  acceptExact (Seq (Extract (Seq (Sym 'a') (Sym 'b'))) (Rep (Sym 'c'))) "abcc" ~?= Matches [MWE ["ab"]],
  acceptExact (Seq (Seq Any (ZeroOrOne (Seq (Sym 'c') (Alt (Sym 'a') (Sym 'b'))))) (Rep (Sym 'd'))) "ca" ~?= Exists False,
  acceptExact (Seq (Seq Any (ZeroOrOne (Seq (Sym 'c') (Alt (Sym 'a') (Sym 'b'))))) (Rep (Sym 'd'))) "xcb" ~?= Exists True,
  acceptExact (Seq (Seq Any (ZeroOrOne (Seq (Sym 'c') (Alt (Sym 'a') (Sym 'b'))))) (Rep (Sym 'd'))) "xcbd" ~?= Exists True,
  acceptExact (Seq (Seq Any (ZeroOrOne (Seq (Sym 'c') (Alt (Sym 'a') (Sym 'b'))))) (Rep (Sym 'd'))) "xcadddd" ~?= Exists True,
  acceptExact (Seq (Seq Any (ZeroOrOne (Seq (Sym 'c') (Alt (Sym 'a') (Sym 'b'))))) (Rep (Sym 'd'))) "xddd" ~?= Exists True,
  acceptExact (Seq (Seq Any (ZeroOrOne (Seq (Sym 'c') (Alt (Sym 'a') (Sym 'b'))))) (Rep (Sym 'd'))) "xcacad" ~?= Exists False] 
