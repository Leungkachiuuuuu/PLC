module MonadicParsing (Parser,
                       parse,
                       item,
                       sat,
                       digit,
                       lower,
                       upper,
                       alphanum,
                       ident,
                       identifier,
                       natural,
                       space,
                       spaceKeep,
                       notQuote,
                       token,
                       string,
                       nat,
                       integer,
                       symbol) where

import Control.Applicative
import Data.Char

-- similar definitions to ST / app
-- parser is a (constructor-wrapped) function from String to a (singleton or empty) list of a thing and remaining String
newtype Parser a = P (String -> [(a, String)])

-- given a Parser and an input string, run the parser
-- or, just think of it as given a Parser, strip off the P constructor to expose the function inside
parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

-- simplest kind of Parser, which just splits input into first char and rest
-- or if no input then return no output
-- In other words, it just consumes one character and returns it as the result
item :: Parser Char
item = P (\inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x,xs)])

-- the usual exercise
instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P (\inp -> case parse p inp of
                   [] -> []
                   [(v,out)] -> [(f v, out)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = P (\inp -> [(x, inp)])
  -- pure builds a parser that always succeeds but does not consume its input

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = P (\inp -> case parse pf inp of
                    [] -> []
                    [(f,out)] -> case parse px out of
                      [] -> []
                      [(x,out')] -> [(f x, out')])


-- make a new Parser which takes the output of the given Parser and uses it as input
instance Monad Parser where
  -- >>= :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
                     [] -> []
                     [(v,out)] -> parse (f v) out)

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
                         [] -> parse q inp
                         x -> x)

-- building block for single-character parsers
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

-- use sat to turn a bunch of Char -> Bool into Parser Char
digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

-- is specific character
char :: Char -> Parser Char
char x = sat (== x)

-- next (n) chars are a specific string
string :: String -> Parser String
string [] = return []
string (x:xs) = do char x       -- cause to fail if next char is not x
                   string xs    -- cause to fail if next (n) chars are not xs
                   return (x:xs)

-- lower case letter followed by numbers or letters
ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

-- any amount of contiguous whitespace
space :: Parser ()
space = do many (sat isSpace)
           return ()

-- keep a single white space
spaceKeep :: Parser Char
spaceKeep = sat isSpace

notQuote :: Parser Char
notQuote = sat (\c -> c /= '"')

-- integer, negative or positive
int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat

-- helpful function to produce a Parser that doesn't care about spacing
token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

-- use token to define space-agnostic versions of the above Parsers
identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)


data Value = I Int | St String | Array[Value] deriving (Show, Eq)
justplus :: Parser Value
justplus = do
  x <- identifier
  return (St x)
