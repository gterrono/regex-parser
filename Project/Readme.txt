Names: Aaditya R. Shirodkar - aadis
       Greg Terrono - gterrono

Files:
Main.hs

OVERVIEW: The top level, abstracted out file which contains the main method and will be compiled and run.
It essentially allows for the user to use one of the top level functions for RegEx matching, which are
i) Checking if matches exist (returnBool)
ii) Returning all matches against the RegEx (returnMatches)
iii) Returning only those matches that are within the extract constructor of the RegEx ('(' & ')')

DEPENDENCIES: ParserPrinter

----------------------------------------------------------------

ParserPrinter.hs

OVERVIEW: This is the organizing module, which uses the parsing capabilities from Parser and ParserCombinators and RegEx matching from RegEx to first parse a string into a RegEx and then match that RegEx against a sample string to return the required result. We're using Monad Transformers to do the parsing.

In addition, it also includes Hughes Pretty Printer to print a given RegEx to its string equivalent.

DEPENDENCIES: Regex, ParserTrans, ParserCombinators, Test.HUnit, Test.QuickCheck, Control.Monad, Text.PrettyPrint.HughesPJ

----------------------------------------------------------------

Regex.hs

OVERVIEW: This is the module which contains the structural information as well as the matching mechanisms for RegExes. It supports (), ^, $, ., *, +, ?, sequences and escaping. It also matches for boolean results, all possible matches and specific extractions.

DEPENDENCIES: None

----------------------------------------------------------------

ParserTrans.hs

OVERVIEW: This is the Parser module which defines the required functions for parsing a RegEx from a string. It follows the structure developed in a previous homework assignment, using Monads and Monad Transformers.

DEPENDENCIES: Control.Monad.State

----------------------------------------------------------------

ParserCombinators.hs

OVERVIEW: This is the Parser Combinators module developed in a previous homework which contains the various combinators which implement the Parser developed, such as between and choice.

DEPENDENCIES: Control.Monad, ParserTrans, Data.Char, System.IO

----------------------------------------------------------------

External Libraries: Hughes Pretty Printer for printing a RegEx as an equivalent string.
