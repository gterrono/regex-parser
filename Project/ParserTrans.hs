{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
module ParserTrans (GenParser, Parser, 
                   getC,
                   choose,
                   (<|>),
                   satisfy,
                   doParse,  
                   ) where

--import Control.Monad.List
import Control.Monad.State

type GenParser e a = StateT [e] [] a

type Parser a =  GenParser Char a

doParse :: GenParser e a  -> [e] -> [(a,[e])]
doParse = runStateT


-- | Return the next character
getC :: GenParser e e 
getC = StateT (\e -> case e of
                     (x:xs) -> [ (x, xs) ]
                     []     -> [])

-- | Return the next character if it satisfies the given predicate
-- (this was called satP in lecture)
satisfy :: (e -> Bool) -> GenParser e e 
satisfy p = do c <- getC
               if (p c) then return c else fail "End of input"

-- | Combine two parsers together in parallel, producing all 
-- possible results from either parser.                 
choose :: GenParser e a -> GenParser e a -> GenParser e a
p1 `choose` p2 = StateT (\e -> doParse p1 e ++ doParse p2 e)

-- | Combine two parsers together in parallel, but only use the 
-- first result. This means that the second parser is used only 
-- if the first parser completely fails. 
(<|>) :: GenParser e a -> GenParser e a -> GenParser e a
p1 <|> p2 = StateT (\e -> case doParse (p1 `choose` p2) e of
                             []   -> []
                             x:_  -> [x])

