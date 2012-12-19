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
  pp (Alt a b)             = (pp a) <> PP.char '|' <> (pp b)
  pp (Seq a (Rep b)) 
   | a == b                = (pp a) <> PP.char '+'
  pp (Seq a b)             = (pp a) <> (pp b)
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

testAccept :: Test
testAccept = TestList [
  accept (Sym 'a') "a" ~?= True,
  accept (Sym 'a') "ab" ~?= True,
  accept (Sym 'a') "b" ~?= False,
  accept Eps "a" ~?= True,
  accept (Alt (Seq (Sym 'a') (Sym 'b')) (Sym 'b')) "b" ~?= True,
  accept (Alt (Seq (Sym 'a') (Sym 'b')) (Sym 'b')) "ab" ~?= True,
  accept (Alt (Seq (Sym 'a') (Sym 'b')) (Sym 'b')) "a" ~?= False,
  accept (Seq (Sym 'a') (Seq (Sym 'b') (Sym 'c'))) "abc" ~?= True,
  accept (Seq (Sym 'a') (Seq (Sym 'b') (Sym 'c'))) "a" ~?= False,
  accept (Rep (Sym 'a')) "a" ~?= True,
  accept (Rep (Sym 'a')) "" ~?= True,
  accept (Rep (Sym 'a')) "aaaa" ~?= True,
  accept (Rep (Sym 'a')) "b" ~?= True,
  accept (Rep (Sym 'a')) "baaa" ~?= True,
  accept Any "a" ~?= True,
  accept Any "ab" ~?= True,
  accept (ZeroOrOne (Sym 'a')) "" ~?= True,
  accept (ZeroOrOne (Sym 'a')) "a" ~?= True,
  accept (ZeroOrOne (Sym 'a')) "aa" ~?= True,
  accept (ZeroOrOne (Sym 'a')) "b" ~?= True,
  accept (StartsWith (Sym 'a')) "a" ~?= True,
  accept (StartsWith (Sym 'a')) "ab" ~?= True,
  accept (StartsWith (Seq (Sym 'a') (Sym 'b'))) "abc" ~?= True,
  accept (EndsWith (Sym 'c')) "abc" ~?= True,
  accept (EndsWith (Sym 'c')) "c" ~?= True,
  accept (EndsWith (StartsWith (Seq (Sym 'a') (Sym 'b')))) "ab" ~?= True,
  accept (Extract (Sym 'a')) "a" ~?= True,
  accept (Extract (Sym 'a')) "ab" ~?= True,
  accept (Seq (Extract (Seq (Sym 'a') (Sym 'b'))) (Rep (Sym 'c'))) "ab" ~?= True,
  accept (Seq (Extract (Seq (Sym 'a') (Sym 'b'))) (Rep (Sym 'c'))) "abcc" ~?= True,
  accept (Seq (Seq Any (ZeroOrOne (Seq (Sym 'c') (Alt (Sym 'a') (Sym 'b'))))) (Rep (Sym 'd'))) "ca" ~?= True,
  accept (Seq (Seq Any (ZeroOrOne (Seq (Sym 'c') (Alt (Sym 'a') (Sym 'b'))))) (Rep (Sym 'd'))) "xcb" ~?= True,
  accept (Seq (Seq Any (ZeroOrOne (Seq (Sym 'c') (Alt (Sym 'a') (Sym 'b'))))) (Rep (Sym 'd'))) "xcbd" ~?= True,
  accept (Seq (Seq Any (ZeroOrOne (Seq (Sym 'c') (Alt (Sym 'a') (Sym 'b'))))) (Rep (Sym 'd'))) "xcadddd" ~?= True,
  accept (Seq (Seq Any (ZeroOrOne (Seq (Sym 'c') (Alt (Sym 'a') (Sym 'b'))))) (Rep (Sym 'd'))) "xddd" ~?= True,
  accept (Seq (Seq Any (ZeroOrOne (Seq (Sym 'c') (Alt (Sym 'a') (Sym 'b'))))) (Rep (Sym 'd'))) "xcacad" ~?= True]

testMatches :: Test
testMatches = TestList [
  matches (Sym 'a') "a" ~?= Right ["a"],
  matches (Sym 'a') "ab" ~?= Right ["a"],
  matches (Sym 'a') "b" ~?= Left "No matches",
  matches Eps "a" ~?= Right ["", "a"],
  matches (Alt (Seq (Sym 'a') (Sym 'b')) (Sym 'b')) "b" ~?= Right ["b"],
  matches (Alt (Seq (Sym 'a') (Sym 'b')) (Sym 'b')) "ab" ~?= Right ["ab", "b"],
  matches (Alt (Seq (Sym 'a') (Sym 'b')) (Sym 'b')) "a" ~?= Left "No matches",
  matches (Seq (Sym 'a') (Seq (Sym 'b') (Sym 'c'))) "abc" ~?= Right ["abc"],
  matches (Seq (Sym 'a') (Seq (Sym 'b') (Sym 'c'))) "a" ~?= Left "No matches",
  matches (Rep (Sym 'a')) "a" ~?= Right ["", "a"],
  matches (Rep (Sym 'a')) "" ~?= Right [""],
  matches (Rep (Sym 'a')) "aaaa" ~?= Right ["","a","aa","aaa","aaaa","","a","aa","aaa","","a","aa","","a"],
  matches (Rep (Sym 'a')) "b" ~?= Right [""],
  matches (Rep (Sym 'a')) "ba" ~?= Right ["", "", "a"],
  matches Any "a" ~?= Right ["a"],
  matches Any "ab" ~?= Right ["a", "b"],
  matches (ZeroOrOne (Sym 'a')) "" ~?= Right [""],
  matches (ZeroOrOne (Sym 'a')) "a" ~?= Right ["", "a"],
  matches (ZeroOrOne (Sym 'a')) "aa" ~?= Right ["", "a", "", "a"],
  matches (ZeroOrOne (Sym 'a')) "b" ~?= Right [""],
  matches (Extract (Sym 'a')) "a" ~?= Right ["a"],
  matches (Extract (Sym 'a')) "ab" ~?= Right ["a"],
  matches (Seq (Extract (Seq (Sym 'a') (Sym 'b'))) (Rep (Sym 'c'))) "ab" ~?= Right ["ab"],
  matches (Seq (Extract (Seq (Sym 'a') (Sym 'b'))) (Rep (Sym 'c'))) "abcc" ~?= Right ["ab", "abc", "abcc"],
  matches (Seq (Seq Any (ZeroOrOne (Seq (Sym 'c') (Alt (Sym 'a') (Sym 'b'))))) (Rep (Sym 'd'))) "ca" ~?= Right ["c", "a"],
  matches (Seq (Seq Any (ZeroOrOne (Seq (Sym 'c') (Alt (Sym 'a') (Sym 'b'))))) (Rep (Sym 'd'))) "xcb" ~?= Right ["x","xcb","c","b"],
  matches (Seq (Seq Any (ZeroOrOne (Seq (Sym 'c') (Alt (Sym 'a') (Sym 'b'))))) (Rep (Sym 'd'))) "xcbd" ~?= Right ["x","xcb","xcbd","c","b","bd","d"],
  matches (Seq (Seq Any (ZeroOrOne (Seq (Sym 'c') (Alt (Sym 'a') (Sym 'b'))))) (Rep (Sym 'd'))) "xcadddd" ~?= Right ["x","xca","xcad","xcadd","xcaddd","xcadddd","c","a","ad","add","addd","adddd","d","dd","ddd","dddd","d","dd","ddd","d","dd","d"],
  matches (Seq (Seq Any (ZeroOrOne (Seq (Sym 'c') (Alt (Sym 'a') (Sym 'b'))))) (Rep (Sym 'd'))) "xddd" ~?= Right ["x","xd","xdd","xddd","d","dd","ddd","d","dd","d"],
  matches (Seq (Seq Any (ZeroOrOne (Seq (Sym 'c') (Alt (Sym 'a') (Sym 'b'))))) (Rep (Sym 'd'))) "xcacad" ~?= Right ["x","xca","c","a","aca","acad","c","a","ad","d"]]
