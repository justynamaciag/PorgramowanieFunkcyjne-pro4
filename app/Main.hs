module Main where

import Lib
import ParserMonads
import System.Environment
import System.IO.Error
import Control.Exception

main :: IO ()
main = do
   result <- try statistics
   case result of
     Left ex -> exHld ex
     Right _ -> putStrLn "Operacja udana"


exHld :: IOError -> IO ()
exHld = \ex -> if isDoesNotExistError ex
               then putStrLn "Plik nie istnieje"
               else ioError ex
