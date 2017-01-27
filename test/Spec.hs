import Lib
import ParserMonads
import Test.HUnit
import Test.QuickCheck
import Data.String
import Data.Char
import Control.Monad


prop_countLines x = countLines x == length (lines x)
prop_countLetters x = countLines x == length (lines x )
prop_countWords x = countWords x == length (words x)


test1 = TestCase $ assertEqual "Function countLetters: " 2 (countLetters "ala"  'a')
test2 = TestCase $ assertEqual "Function countWords: "  4 (countWords "Ala ma kota Ule")
test3 = TestCase (do inpStr <- readFile "testy.txt"
                     assertEqual "Function countLines" 4 (countLines inpStr))

test4 = TestCase (do inpStr <- readFile "testy.txt"
                     assertEqual "Function longerThan80 " 1 (longerThan80 inpStr))

tests = TestList [TestLabel "test1: " test1,
                  TestLabel "test2: " test2,
                  TestLabel "test3: " test3,
                  TestLabel "test4: " test4]


main = do
  runTestTT tests
  quickCheck prop_countWords
  quickCheck prop_countLines
  quickCheck prop_countLetters
