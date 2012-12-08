
-- Advanced Programming, HW 5
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
main = do _ <- runTestTT (TestList [ t0, 
                                     t11, t12, t13, 
                                     t2 ])
          return ()

-- Problem 0
---------------------------------------------




-- Problem 1
---------------------------------------------

valueP :: GenParser Char Value
valueP = intP <|> boolP

constP :: String -> a -> GenParser Char a
constP s a = liftM (\_ -> a) (string s)

opP :: GenParser Char Bop 
opP = choice [constP "*" Rep, constP "|" Alt]

symP :: GenParser Char Reg
symP = liftM Sym getC

parensP :: GenParser Char Expression
parensP = between (char '(') exprP ((char ')')) <|> symP

vP :: GenParser Char Expression
vP = liftM Val (wsP valueP) <|> liftM Var (wsP varP)

t11 :: Test
t11 = TestList ["s1" ~: succeed (parse exprP "1 "),
                "s2" ~: succeed (parse exprP "1  + 2") ] where
  succeed (Left _)  = assert False
  succeed (Right _) = assert True

statementP :: GenParser Char Statement
statementP = sequenceP <|> nonSquenceP where
  assignP = do
    var <- wsP varP
    _ <- wsP (string ":=")
    exp <- exprP
    return (Assign var exp)
  ifP = do
    exp <- between (wsP (string "if")) exprP (wsP (string "then"))
    s1 <- statementP
    s2 <- between (wsP (string "else")) statementP (wsP (string "endif"))
    return (If exp s1 s2)
  whileP = do
    _ <- wsP (wsP (string "while"))
    exp <- exprP
    s <- between (wsP (string "do")) statementP (wsP (string "endwhile"))
    return (While exp s)
  sequenceP = do
    s1 <- nonSquenceP
    _ <- wsP (char ';')
    s2 <- statementP
    return (Sequence s1 s2)
  nonSquenceP = choice [(wsP (constP "skip" Skip)), assignP, ifP, whileP]

t12 :: Test
t12 = TestList ["s1" ~: p "fact.imp",
                "s2" ~: p "test.imp", 
                "s3" ~: p "abs.imp" ,
                "s4" ~: p "times.imp" ] where
  p s = do { y <- parseFromFile statementP s ; succeed y }
  succeed (Left _)  = assert False
  succeed (Right _) = assert True

testRT :: String -> Assertion
testRT filename = do 
   x <- parseFromFile statementP filename 
   case x of 
     Right ast -> case parse statementP (display ast) of
       Right ast' -> assert (ast == ast')
       Left _ -> assert False
     Left _ -> assert False                             

t13 :: Test
t13 = TestList ["s1" ~: testRT "fact.imp",
                "s2" ~: testRT "test.imp", 
                "s3" ~: testRT "abs.imp" ,
                "s4" ~: testRT "times.imp" ]

instance Arbitrary Bop where
  arbitrary = elements [Plus, Minus, Times, Divide, Gt, Ge, Lt, Le]

instance Arbitrary Value where
  arbitrary = oneof [liftM IntVal arbitrary, liftM BoolVal arbitrary]

instance Arbitrary Expression where
  arbitrary = oneof [liftM Var (liftM (:[]) (elements ['A'..'Z'])), liftM Val arbitrary, liftM3 Op arbitrary arbitrary arbitrary]
  shrink (Op _ e1 e2) = [e1, e2] ++ (shrink e1) ++ (shrink e2)
  shrink _ = []

instance Arbitrary Statement where
  arbitrary = frequency [(3, liftM2 Assign (liftM (:[]) (elements ['A'..'Z'])) arbitrary), 
              (3, liftM3 If arbitrary arbitrary arbitrary), (1, liftM2 While arbitrary arbitrary), 
              (3, liftM2 Sequence (frequency [(1, liftM2 Assign (liftM (:[]) (elements ['A'..'Z'])) arbitrary), 
                (1, liftM3 If arbitrary arbitrary arbitrary), (1, liftM2 While arbitrary arbitrary), (5, return Skip)]) arbitrary), 
              (5, return Skip)]
  shrink (If _ s1 s2) = [s1, s2] ++ (shrink s1) ++ (shrink s2)
  shrink (While _ s) = [s] ++ (shrink s)
  shrink (Sequence s1 s2) = [s1, s2] ++ (shrink s1) ++ (shrink s2)
  shrink _ = []

quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n }

prop_RT :: Statement -> Bool
prop_RT ast = case parse statementP (display ast) of
  Right ast' -> ast == ast'
  Left _ -> False

