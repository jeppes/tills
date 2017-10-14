module Combinators (Parser, parse, item, sat, digit, lower,
  alphanum, char, string, ident, space, token, identifier,
  integer, symbol, comment, stringLiteral, newline) where

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

instance Functor Parser where
  fmap g p = P (\inp -> case parse p inp of
                          [] -> []
                          [(v, out)] -> [(g v, out)])

instance Applicative Parser where
  pure v = P (\inp -> [(v, inp)])
  pg <*> px = P (\inp -> case parse pg inp of
                           [] -> []
                           [(g,out)] -> parse (fmap g px) out)

instance Alternative Parser where
  empty = P (\inp -> [])
  p <|> q = P (\inp -> case parse p inp of
                         [] -> parse q inp
                         [(v, out)] -> [(v, out)])

instance Monad Parser where
  p >>= f = P (\inp -> case parse p inp of
                         [] -> []
                         [(v, out)] -> parse (f v) out)

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x,xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

alphanum :: Parser Char
alphanum = sat isAlphaNum

validChar :: Parser Char
validChar = sat $ \c -> c == '_' || isAlphaNum c

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)


stringLiteral :: Parser String
stringLiteral = do space 
                   symbol "\'"
                   content <- many (sat (/= '\''))
                   symbol "\'"
                   space
                   return content 
                <|> do
                   symbol "\""
                   content <- many (sat (/= '\"'))
                   symbol "\""
                   space
                   return content 

keywords = [ "if" , "then" , "else" , "true" , "false" , "skip" , "while" , "do" , "try" , "catch", "read_int", "print" ]

ident :: Parser String
ident = do x <- lower
           xs <- many validChar
           let id = x:xs
           if (id `notElem` keywords)
              then return id
              else empty

nat :: Parser Integer
nat = do xs <- some digit
         return (read xs)

int :: Parser Integer
int = do char '-'
         n <- nat
         return (-n)
        <|> nat

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             return v

newline :: Parser ()
newline = do many (sat (\c -> isSpace c && not (c =='\n')))
             char '\n'
             space

identifier :: Parser String
identifier = token ident

natural :: Parser Integer
natural = token nat

integer :: Parser Integer
integer = token int

symbol :: String -> Parser String
symbol s = token (string s)

comment :: Parser String
comment = do symbol "#"
             _ <- many (sat (/= '\n'))
             space
             return ""
