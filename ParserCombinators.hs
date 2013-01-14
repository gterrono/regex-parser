
-- Advanced Programming, Final Project
-- by Greg Terrono gterrono, Aaditya R. Shirodkar aadis

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

module ParserCombinators where

import ParserTrans
            
type ParseError = String

-- | Use a parser for a particular RegEx string. Only allows atomic parses,
-- i.e. complete parses, so if an error occurs midway, the entire RegEx
-- is thrown out.
parse :: GenParser b a -> [b] -> Either ParseError a
parse parser bs = case (doParse parser bs) of 
    []      -> Left  "No parses"
    [(a,[])] -> Right a
    _       -> Left  "Incorrect parses"  
   
-- | Parses and returns the specified character        
-- succeeds only if the input is exactly that character
char :: Char -> GenParser Char Char
char c = satisfy (c ==)                         
                      
-- | Combine all parsers in the list (sequentially)
choice :: [GenParser b a] -> GenParser b a
choice = foldr (<|>) (fail "")

-- | @between open close p@ parses @open@, followed by @p@ and finally
--   @close@. Only the value of @p@ is returned.
between :: Show a => GenParser b open -> GenParser b a -> GenParser b close -> GenParser b a
between open p close = do _ <- open
                          x <- p
                          _ <- close
                          return x



