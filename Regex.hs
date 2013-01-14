
-- Advanced Programming, Final Project
-- by Greg Terrono gterrono, Aaditya R. Shirodkar aadis

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

module Regex (returnBool, returnMatches, returnExtractions, display, allTests) where
import Control.Monad

import Text.PrettyPrint.HughesPJ (Doc, (<>))
import qualified Text.PrettyPrint.HughesPJ as PP

import ParserTrans
import ParserCombinators

import Test.HUnit

-- | The Regular Expression data type, supports characters, sequences, escaping, |, *, ?, +, ., ^, $, () 
data Reg = Eps
  | Sym Char        -- a
  | Alt Reg Reg     -- a|b
  | Seq Reg Reg     -- ab
  | Rep Reg         -- a*
  | Any             -- .
  | ZeroOrOne Reg   -- ?
  | StartsWith Reg  -- ^
  | EndsWith Reg    -- $
  | Extract Reg     -- (a)
  deriving (Show, Eq)

-- | Allows for extracting everything within parens in a regular expression, and for multiple extractions
-- within the same regular extraction.
newtype MatchWithExtraction = MWE [String] 
  deriving (Show, Eq)

-- | Result type, returns a boolean value for most regular expression types, except Extract, for which it
-- returns the extraction.
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
combineExtractions (Matches [MWE e1]) (Matches [MWE e2]) = Matches [MWE (e1 ++ e2)]
combineExtractions r1 r2                                 = combinesAnd [r1, r2]

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

-- | Converts a result type to a Bool, for a call to accept
resultToBool :: Result -> Bool
resultToBool r = case r of
  Exists b -> b
  _        -> True

-- | Converts a result type for a call to return acceptExtract
resultToExtraction :: Result -> [MatchWithExtraction]
resultToExtraction r = case r of
  Exists _  -> []
  Matches b -> b

-- | Returns all of the ways a string matches a regex
matches :: Reg -> String -> Either String [String]
matches (EndsWith (StartsWith r)) u = filterHelper acceptExactToBool return r u
matches (StartsWith r) u            = filterHelper accept substringsFromStart r u
matches (EndsWith r) u              = filterHelper accept substringsWithEnd r u
matches r u                         = filterHelper acceptExactToBool allSubstrings r u

-- | A commonly used pattern extracted into a higher ordered function
filterHelper :: (Reg -> String -> Bool) -> (String -> [String]) -> Reg -> String -> Either String [String]
filterHelper g f r u = case filter (\s -> g r s) (f u) of
   [] -> Left "No matches"
   l  -> Right l

-- | Takes a regex and a string and returns if the regex matches it with nothing left over
acceptExactToBool :: Reg -> String -> Bool
acceptExactToBool r s = resultToBool (acceptExact r s)

-- | Returns all substrings of a list
allSubstrings :: String -> [String]
allSubstrings ""       = [""]
allSubstrings l@(_:cs) = substringsFromStart l ++ allSubstrings cs

-- | Returns all substrings of a list that start at the front
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


-- Pretty Printing a RegEx
---------------------------------------------
class PP a where
  pp :: a -> Doc

instance PP Reg where
  pp Eps                   = PP.text ""
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

-- | Pretty prints the PP Doc to a String
display :: PP a => a -> String
display = show . pp



-- Parsing a RegEx
---------------------------------------------

-- | a Parser for symbols
symP :: GenParser Char Reg
symP = liftM Sym $ satisfy (\x -> and (map (x /=) [')', '^', '$', '*', '+', '|', '?']))

-- | a Parser for the type Any
anyP :: GenParser Char Reg
anyP = char '.' >> return Any

-- | a Parser to escape special characters, like ), ?, / etc.
escapeP :: GenParser Char Reg
escapeP = do
  _ <- char '\\'
  liftM Sym getC

-- | a parser to returning Extractions from within parens
parensP :: GenParser Char Reg
parensP = choice [between (char '(') (liftM Extract statementP) ((char ')')), anyP, symP]

-- | a parser for general statements
statementP :: GenParser Char Reg
statementP = sequenceP <|> nonSequenceP where
  altP = do
    s1 <- choice [repP, zeroP, parensP]
    _  <- char '|'
    s2 <- parensP
    return (Alt s1 s2)
  repP = do
    s1 <- parensP
    _  <- char '*'
    return (Rep s1)
  zeroP = do
    s1 <- parensP
    _  <- char '?'
    return (ZeroOrOne s1)
  oneP = do
    s1 <- parensP
    _  <- char '+'
    return (Seq s1 (Rep s1))
  sequenceP = do
    s1 <- nonSequenceP
    s2 <- statementP
    return (Seq s1 s2)
  nonSequenceP = choice [escapeP, altP, repP, zeroP, oneP, parensP]

-- | a Parser for '^'
startsWithP :: GenParser Char Reg
startsWithP = startsP <|> statementP where
  startsP = do
    _  <- char '^'
    s1 <- statementP
    return (StartsWith s1)

-- | a Parser for '$'
endsWithP :: GenParser Char Reg
endsWithP = endsP <|> startsWithP where
  endsP = do
    s1 <- startsWithP
    _  <- char '$'
    return (EndsWith s1)

-- Parses a RegEx and returns a result
-------------------------------------------------------

-- | A top level function that parses the first string as a RegEx and matches it against
-- the second String to return a Boolean value corresponding to 'accept'
returnBool :: String -> String -> Bool
returnBool a b = case (parse endsWithP a) of
  Left _  -> False
  Right c -> accept c b

-- | A top level function that parses the first string as a RegEx and matches it against
-- the second String to return a Boolean value corresponding to 'matches'
returnMatches :: String -> String -> Either String [String]
returnMatches a b = case (parse endsWithP a) of
  Left _  -> Left "Not a valid Regex"
  Right c -> matches c b

-- | A top level function that parses the first string as a RegEx and matches it against
-- the second String to return a Boolean value corresponding to 'acceptExtract'
returnExtractions :: String -> String -> [MatchWithExtraction]
returnExtractions a b = case (parse endsWithP a) of
  Left _  -> []
  Right c -> acceptExtract c b

-- Testing Suite
----------------------------------------------------
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
  matches Eps "a" ~?= Right ["","a",""],
  matches (Alt (Seq (Sym 'a') (Sym 'b')) (Sym 'b')) "b" ~?= Right ["b"],
  matches (Alt (Seq (Sym 'a') (Sym 'b')) (Sym 'b')) "ab" ~?= Right ["ab", "b"],
  matches (Alt (Seq (Sym 'a') (Sym 'b')) (Sym 'b')) "a" ~?= Left "No matches",
  matches (Seq (Sym 'a') (Seq (Sym 'b') (Sym 'c'))) "abc" ~?= Right ["abc"],
  matches (Seq (Sym 'a') (Seq (Sym 'b') (Sym 'c'))) "a" ~?= Left "No matches",
  matches (Rep (Sym 'a')) "a" ~?= Right ["","a",""],
  matches (Rep (Sym 'a')) "" ~?= Right [""],
  matches (Rep (Sym 'a')) "aaaa" ~?= Right ["","a","aa","aaa","aaaa","","a","aa","aaa","","a","aa","","a",""],
  matches (Rep (Sym 'a')) "b" ~?= Right ["",""],
  matches (Rep (Sym 'a')) "ba" ~?= Right ["","","a",""],
  matches Any "a" ~?= Right ["a"],
  matches Any "ab" ~?= Right ["a", "b"],
  matches (ZeroOrOne (Sym 'a')) "" ~?= Right [""],
  matches (ZeroOrOne (Sym 'a')) "a" ~?= Right ["", "a", ""],
  matches (ZeroOrOne (Sym 'a')) "aa" ~?= Right ["", "a", "", "a", ""],
  matches (ZeroOrOne (Sym 'a')) "b" ~?= Right ["", ""],
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

testAcceptExtract :: Test
testAcceptExtract = TestList [
  acceptExtract (Sym 'a') "a" ~?= [],
  acceptExtract (Extract (Sym 'a')) "a" ~?= [MWE ["a"]],
  acceptExtract (Extract (Rep (Sym 'a'))) "a" ~?= [MWE [""], MWE ["a"], MWE [""]],
  acceptExtract (StartsWith (Extract (Seq (Sym 'a') (Sym 'b')))) "abc" ~?= [MWE ["ab"]],
  acceptExtract (StartsWith (Seq (Extract (Seq (Sym 'a') (Sym 'b'))) (Sym 'c'))) "abc" ~?= [MWE ["ab"]],
  acceptExtract (StartsWith (Seq (Extract (Seq (Sym 'a') (Sym 'b'))) (Extract (Sym 'c')))) "abc" ~?= [MWE ["ab", "c"]],
  acceptExtract (EndsWith (StartsWith (Extract (Seq (Sym 'a') (Sym 'b'))))) "ab" ~?= [MWE ["ab"]],
  acceptExtract (EndsWith (StartsWith (Extract (Seq (Sym 'a') (Sym 'b'))))) "abc" ~?= []]


testDisplay :: Test
testDisplay = TestList[
  display (Sym 'a') ~?= "a",
  display (Extract (Seq (Sym 'a') (Rep (Sym 'b')))) ~?= "(ab*)",
  display (Seq (Sym 'a') (Rep (Sym 'a'))) ~?= "a+",
  display (StartsWith (ZeroOrOne (Extract (Alt (Extract (Seq (Rep (Sym 'a')) Any)) (Sym 'c'))))) ~?= "^((a*.)|c)?"]

testReturnBool :: Test
testReturnBool = TestList[
  returnBool "^abc" "ab" ~?= False,
  returnBool "^abc" "abcd" ~?= True,
  returnBool "^abc$" "abcd" ~?= False,
  returnBool "^abc$" "abc" ~?= True,
  returnBool "a|bc" "abc" ~?= True,
  returnBool "a*b?c" "ab" ~?= False]

testReturnExtractions :: Test
testReturnExtractions = TestList[
  returnExtractions "^abc" "ab" ~?= [],
  returnExtractions "^a(bc)" "abcd" ~?= [MWE ["bc"]],
  returnExtractions "^(abc)$" "abcd" ~?= [],
  returnExtractions "^(abc)$" "abc" ~?= [MWE ["abc"]],
  returnExtractions "(a|b)c" "abc" ~?= [MWE ["b"]],
  returnExtractions "^(ab)(c)" "abc" ~?= [MWE ["ab", "c"]]]

testReturnMatches :: Test
testReturnMatches =  TestList[
  returnMatches "^abc" "ab" ~?= Left "No matches",
  returnMatches "^a(bc)" "abcd" ~?= Right ["abc", "abcd"],
  returnMatches "^(abc)$" "abcd" ~?= Left "No matches",
  returnMatches "^(abc)$" "abc" ~?= Right ["abc"],
  returnMatches "(a|b)c" "abc" ~?= Right ["bc"],
  returnMatches "^(ab)(c)" "abc" ~?= Right ["abc"]]

testSplit :: Test
testSplit =  split "ab" ~?= [("","ab"),("a","b"),("ab","")]

testParts :: Test
testParts = parts "ab" ~?= [["ab"],["a","b"]]

allTests :: Test
allTests = TestList [
  testParts, 
  testSplit, 
  testReturnMatches, 
  testReturnExtractions, 
  testReturnBool, 
  testDisplay, 
  testAcceptExtract, 
  testMatches, 
  testAccept, 
  testAcceptExact ]
