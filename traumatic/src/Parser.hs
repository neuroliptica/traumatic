module Parser
  where

import Control.Applicative
import Control.Arrow (first)

import Data.Char (isDigit)

data Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f p = Parser $ \s -> first f <$> runParser p s

instance Applicative Parser where
    pure a = Parser $ \s -> Just (a, s)
    fp <*> vp = Parser $ \s -> do
        (f, s') <- runParser fp s
        (v, s'') <- runParser vp s'
        return (f v, s'')

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing 
    p <|> q = Parser $ \s ->
        let res = runParser p s
        in if null res
             then runParser q s
             else res

char :: Char -> Parser Char
char c = Parser fun where
    fun [] = Nothing
    fun (c':cs) | c' == c = Just (c, cs)
                | otherwise = Nothing

notChar :: Char -> Parser Char
notChar c = Parser fun where
    fun [] = Nothing
    fun (c':cs) | c' == c = Nothing
                | otherwise = Just (c', cs)

key :: Parser String
key = (many $ char '-') *> (many $ notChar '=')

value :: Parser String
value = char '=' *> (many $ notChar ' ')

flag :: Parser (String, String)
flag = (,) <$> key <*> value

digit :: Parser Char
digit = Parser fun where
    fun [] = Nothing
    fun (c:cs) | isDigit c = Just (c, cs)
               | otherwise = Nothing

parseThread :: Parser String
parseThread = many digit

parseBoard :: Parser String
parseBoard = many $ notChar ' '

{-
 - Прокси это либо
 - Число . Число . Число . Число : Число
 - 
 - либо
 -
 - Строка : Строка @ Число . Число . Число . Число : Число
 - -}

data IP a = IP a a a a a
    deriving Show

data Auth = Auth String String (IP String)
    deriving Show

substr :: String -> Parser String
substr [] = Parser $ \s -> Just ([], s)
substr (x:xs) = Parser fun where
    fun [] = Nothing
    fun (y:ys) | y == x = first (y:) <$> runParser (substr xs) ys
               | otherwise = Nothing

chain :: Parser String
chain = many digit <* char '.'

ip :: Parser (IP String)
ip = IP <$> chain <*> chain <*> chain <*> (many digit <* char ':') <*> many digit

{-# INLINE address #-}
address :: IP String -> String
address (IP a b c d _) = a <> "." <> b <> "." <> c <> "." <> d

port :: IP String -> String
port (IP _ _ _ _ p) = p

login :: Parser String
login = many (notChar ':') <* char ':'

pass :: Parser String
pass = many (notChar '@') <* char '@'

auth_ip :: Parser Auth
auth_ip = Auth <$> login <*> pass <*> ip

