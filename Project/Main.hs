
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

-- returnBool "ab*c+(d|e)?." "abbbcde"
-- returnBool "ab*c+(d|e)?." "abbbde"
-- returnBool "ab*c+(d|e)?." "abbbcdf"
-- returnBool "ab*c+(d|e)?." "acdx"
-- returnMatches "((a|b)|c)*" "a"
-- returnExtractions "((a|b)|c)" "a"
-- returnMatches "^ab$" "ab"
-- returnMatches "a**" "a"
-- returnExtractions "^(a.)(c)" "abc"