module ParserMonads
    ( parseeee,
    zero,
    item,
    useFunction,
    char,
    upper,
    lower,
    result,
    isVowel
    ) where

import System.Environment
import System.IO
import Control.Monad

-- |Using item,char,upper,lower
parseeee :: [String] -> IO ()
parseeee [inpStr] = do
      putStrLn "Parsing first char"
      print (parse item inpStr)
      putStrLn "If first char is Z"
      print (parse (char 'Z') inpStr)
      putStrLn "Is first char upper/lower"
      print (parse upper inpStr)
      print (parse lower inpStr)
      print (parse isVowel inpStr)


type ParserFun a = String -> [(a, String )]

newtype Parser a = Parser { parse :: ParserFun a }

-- |  Parser with empty value
zero :: Parser a
zero = Parser $ \ inp -> []

-- |Funtion which gives first char from input
item :: Parser Char
item = Parser $ \ inp -> case inp of
      [] -> []
      (x:xs) -> [(x,xs)]

-- |Function which is using 'useFunction' in order to check if is it a upper letter
upper :: Parser Char
upper = useFunction (\x -> 'A' <= x && x <= 'Z')

-- | Usinf 'useFunction' in order to check if char is a vowel
isVowel :: Parser Char
isVowel = useFunction (\x -> x `elem` "aeiouAEIOU")


-- |Function which is using 'useFunction' in order to check if is it a lower letter
lower :: Parser Char
lower = useFunction (\x -> 'a' <= x && x <= 'z')

-- |Returns chars packed in Parser, which fulfill the condition gave as true/false function
useFunction :: ( Char -> Bool ) -> Parser Char
useFunction p = do
  x <- item
  if p x then result x
        else zero

-- |Funcion which is using sat in order to check identity
char :: Char -> Parser Char
char x = useFunction (\y -> x == y)


-- | Function which returns value packed in Parser
result :: a -> Parser a
result v = Parser $ \ inp -> [(v, inp)]


instance Monad Parser where
-- return :: a -> Parser a
    return = result
--(>>=) :: Parser a -> (a -> Parser b) -> Parser b
    ( Parser p) >>= f = Parser $ bind p ( parse . f)


instance Functor Parser where
-- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f ( Parser p) = Parser $ \ inp ->
      map (\(x,str ) -> (f x, str )) $ p inp


instance Applicative Parser where
      -- pure :: a -> Parser a
      pure = result
      --ap :: Monad m => m (a -> b) -> m a -> m b
      --(<* >) :: Parser (a -> b) -> Parser a -> Parser b
      (<*>) = ap


bind :: ParserFun a -> (a -> ParserFun b)
                          -> ParserFun b
bind p f = \ inp ->
    concat [f v out | (v, out) <- p inp]
