
-- Advanced Programming, Final Project
-- by Greg Terrono gterrono, Aaditya R. Shirodkar aadis

import Regex
import Test.HUnit

main :: IO () 
main = do
       runTestTT allTests
       putStrLn $ "Use any of the following functions:"
       putStrLn $ "returnBool <RegEx> <String>"
       putStrLn $ "returnMatches <RegEx> <String>"
       putStrLn $ "returnExtractions <RegEx> <String>"
       return ()
