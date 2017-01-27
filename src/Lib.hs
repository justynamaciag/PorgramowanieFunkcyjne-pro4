module Lib
    ( statistics,
    countLetters,
    countWords,
    countLines,
    longerThan80
    ) where

import System.Environment
import System.IO
import System.IO.Error
import Control.Exception
import ParserMonads


-- | Makes statistics about string from input File, using parseeee
statistics :: IO ()
statistics= do
  (inFileName:outFileName:_) <- getArgs
  inpStr <- readFile inFileName
  writeFile outFileName inpStr
  putStrLn inpStr
  parseeee [inpStr]

  putStrLn "Liczba liter a:"
  print (countLetters inpStr 'a')
  putStrLn "Liczba słow:"
  print (countWords inpStr )
  putStrLn "Liczba linii:"
  print (countLines inpStr )
  putStrLn"Liczba słow dłuższych niż 80 znaków:"
  print (longerThan80 inpStr )


-- | Count amount of specified char in input string
countLetters :: String -> Char -> Int
countLetters xs x =  foldl (\count char -> if char == x then (count + 1) else count) 0 xs

-- |Count amount od words in input string
countWords :: String -> Int
countWords str = length (words str)

-- |Count amount of lines in input string
countLines :: String -> Int
countLines str =  length (lines str )

-- |Count amount of words longer than 80 chars in string
longerThan80 :: String -> Int
longerThan80 str = length $ filter (\x -> length x > 80) $ lines (str)
