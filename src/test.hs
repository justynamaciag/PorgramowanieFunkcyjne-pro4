module test where

import Lib
import Test.HUnit


test 1 = testCase  $ assertEqual "should return 3 " 3 (countLetters "ala")
{-}
test 1 = testCase "countLetters of ala is 3" (assertEqual 3 (countLetters "ala"))
--test 2 = testCase "countWords of ala ma kota ule  is 4"  assertEqual 4 (countWords "Ala ma kota Ule")*-}

tests = TestList [TestLabel "test1: " test 1]
                  --TestLabel "test2: ", test 2]


main = do
	runTestTT testy
